{-|
Native messaging host for browser and mail client add-ons
(e.g. the TaskLite Thunderbird add-on).

Messages are JSON objects prefixed with their length
as a 32-bit unsigned integer in native byte order.
https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/Native_messaging
-}
module NativeMessaging (
  installNativeHost,
  runNativeHost,
)
where

import Protolude (
  Either (Left, Right),
  IO,
  Maybe (..),
  Text,
  catMaybes,
  fromIntegral,
  not,
  pure,
  show,
  ($),
  (&),
  (.),
  (<$>),
  (<&>),
  (<>),
  (==),
 )
import Protolude qualified as P

import Control.Monad.Catch (catchAll)
import Control.Monad.Fail (fail)
import Data.Aeson (Value, object, withObject, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Builder (lazyByteString, toLazyByteString)
import Data.ByteString.Builder.Extra (word32Host)
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Database.SQLite.Simple (Connection, Only (Only), query)
import Foreign.Ptr (castPtr)
import Foreign.Storable (peek)
import Prettyprinter (Doc, hardline, pretty, vsep)
import Prettyprinter.Render.Terminal (AnsiStyle)
import System.Directory (
  createDirectoryIfMissing,
  getHomeDirectory,
  getPermissions,
  setOwnerExecutable,
  setPermissions,
 )
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath ((</>))
import System.IO (hFlush, hSetBinaryMode, stdin, stdout)
import System.Info (os)

import Config (Config (dataDir))
import Email (emailToImportTask)
import ImportExport (
  EditResult (DeleteRequested, Edited),
  applyEditResult,
  insertImportTask,
  parseEditedMarkdown,
 )
import ImportTask (ImportTask (task), setMissingFields)
import Task (Task (body, ulid), taskToEditableMarkdown)


hostName :: Text
hostName = "tasklite"


-- | IDs of the add-ons which are allowed to use the native messaging host
allowedExtensions :: [Text]
allowedExtensions = ["thunderbird@tasklite.org"]


data Request
  = ImportEmail BS.ByteString
  | -- | Get the task as Markdown like `tasklite edit`
    GetTask Text
  | -- | Apply the edited Markdown like `tasklite edit`
    UpdateTask Text BS.ByteString


parseRequest :: Value -> Parser Request
parseRequest = withObject "request" $ \o -> do
  action :: Text <- o .: "action"
  case action of
    "importEmail" -> do
      emailBase64 :: Text <- o .: "emailBase64"
      case Base64.decode (P.encodeUtf8 emailBase64) of
        Left error -> fail $ "Invalid base64 encoding: " <> error
        Right email -> pure $ ImportEmail email
    "getTask" -> GetTask <$> o .: "ulid"
    "updateTask" -> do
      ulid <- o .: "ulid"
      markdown :: Text <- o .: "markdown"
      pure $ UpdateTask ulid (P.encodeUtf8 markdown)
    _ -> fail $ "Unknown action " <> show action


response :: Text -> Text -> [(Aeson.Key, Value)] -> Value
response status message fields =
  object $ ["status" .= status, "message" .= message] <> fields


findTask :: Connection -> Text -> IO (Maybe Task)
findTask connection ulid =
  query connection "SELECT * FROM tasks WHERE ulid == ?" (Only ulid)
    <&> P.head


handleRequest :: Config -> Connection -> Request -> IO Value
handleRequest conf connection = \case
  ImportEmail email ->
    case emailToImportTask (BSL.fromStrict email) of
      Left error -> pure $ response "error" error []
      Right importTaskRaw -> do
        importTask <- setMissingFields importTaskRaw
        let theTask = importTask.task
        existingMb <- findTask connection theTask.ulid
        status <- case existingMb of
          Nothing -> do
            _ <- insertImportTask conf connection importTask
            pure "added"
          Just _ -> pure "exists"
        pure $ response status theTask.body ["ulid" .= theTask.ulid]
  GetTask ulid -> do
    taskMb <- findTask connection ulid
    case taskMb of
      Nothing -> pure $ response "error" ("Task " <> ulid <> " does not exist") []
      Just theTask -> do
        markdown <- taskToEditableMarkdown connection theTask
        pure $
          response "ok" theTask.body ["markdown" .= P.decodeUtf8 markdown]
  UpdateTask ulid markdown -> do
    taskMb <- findTask connection ulid
    case taskMb of
      Nothing -> pure $ response "error" ("Task " <> ulid <> " does not exist") []
      Just theTask -> do
        currentMarkdown <- taskToEditableMarkdown connection theTask
        if markdown == currentMarkdown
          then pure $ response "unchanged" theTask.body []
          else case parseEditedMarkdown markdown of
            -- The add-on keeps the editor open, so the user can fix the error
            Left error -> pure $ response "invalid" error []
            Right editResult -> do
              _ <- applyEditResult conf connection theTask editResult
              pure $ case editResult of
                DeleteRequested -> response "deleted" theTask.body []
                Edited importTask _ -> response "updated" importTask.task.body []


-- | XDG directories of the current environment as shell `export` statements
getXdgExports :: IO [Text]
getXdgExports =
  ["XDG_CONFIG_HOME", "XDG_DATA_HOME"]
    & P.mapM
      ( \name ->
          lookupEnv name <&> \case
            Just value
              | not (P.null value) ->
                  Just $ "export " <> T.pack name <> "=" <> shellQuote value <> ";"
            _ -> Nothing
      )
      <&> catMaybes


writeExecutable :: P.FilePath -> Text -> IO ()
writeExecutable filePath content = do
  T.writeFile filePath content
  permissions <- getPermissions filePath
  setPermissions filePath (setOwnerExecutable P.True permissions)


readMessage :: IO (Maybe BS.ByteString)
readMessage = do
  lengthBytes <- BS.hGet stdin 4
  if BS.length lengthBytes P.< 4
    then pure Nothing
    else do
      messageLength :: P.Word32 <-
        BS.useAsCString lengthBytes (peek . castPtr)
      BS.hGet stdin (fromIntegral messageLength) <&> Just


writeMessage :: Value -> IO ()
writeMessage value = do
  let encoded = Aeson.encode value
  BSL.hPut stdout $
    toLazyByteString $
      word32Host (fromIntegral $ BSL.length encoded)
        <> lazyByteString encoded
  hFlush stdout


-- | Handle one request from stdin and write the response to stdout
runNativeHost :: Config -> Connection -> IO (Doc AnsiStyle)
runNativeHost conf connection = do
  hSetBinaryMode stdin P.True
  hSetBinaryMode stdout P.True

  messageMb <- readMessage
  case messageMb of
    Nothing -> pure "No message received"
    Just message -> do
      responseValue <-
        catchAll
          ( case Aeson.eitherDecodeStrict message
              P.>>= parseEither parseRequest of
              Left error -> pure $ response "error" (T.pack error) []
              Right request -> handleRequest conf connection request
          )
          (\exception -> pure $ response "error" (show exception) [])
      writeMessage responseValue
      pure P.mempty


-- | Quote a string for POSIX shells
shellQuote :: P.FilePath -> Text
shellQuote path =
  "'" <> T.replace "'" "'\\''" (T.pack path) <> "'"


{-| Write the launcher script and the native messaging manifest,
so that Thunderbird can start `tasklite nativehost run`.
-}
installNativeHost :: Config -> IO (Doc AnsiStyle)
installNativeHost conf = do
  homeDir <- getHomeDirectory
  manifestDir <- case os of
    "darwin" -> pure $ homeDir </> "Library/Mozilla/NativeMessagingHosts"
    "linux" -> pure $ homeDir </> ".mozilla/native-messaging-hosts"
    _ -> P.die $ "Native messaging is not supported on " <> T.pack os

  executablePath <- getExecutablePath

  -- Thunderbird starts the host with a minimal environment,
  -- so the XDG directories of the current environment are preserved
  xdgExports <- getXdgExports

  let
    launcherPath = conf.dataDir </> "native-messaging-host"
    manifestPath = manifestDir </> T.unpack hostName <> ".json"
    launcher =
      T.unlines $
        ["#!/bin/sh", "# Generated by `tasklite nativehost install`"]
          <> xdgExports
          <> ["exec " <> shellQuote executablePath <> " nativehost run"]
    manifest =
      object
        [ "name" .= hostName
        , "description" .= ("TaskLite" :: Text)
        , "path" .= launcherPath
        , "type" .= ("stdio" :: Text)
        , "allowed_extensions" .= allowedExtensions
        ]

  createDirectoryIfMissing P.True conf.dataDir
  writeExecutable launcherPath launcher

  createDirectoryIfMissing P.True manifestDir
  BSL.writeFile manifestPath (Aeson.encode manifest)

  pure $
    vsep
      [ "Installed native messaging host:"
      , "  Manifest: " <> pretty manifestPath
      , "  Launcher: " <> pretty launcherPath
      , "  TaskLite: " <> pretty executablePath
      ]
      <> hardline
