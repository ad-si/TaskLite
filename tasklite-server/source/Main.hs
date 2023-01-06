-- Necessary to print git hash in help output,
-- to embed example config file, and for Servant
{-# LANGUAGE TemplateHaskell #-}


module Main where

import Protolude

import Lib
import Data.Aeson as Aeson
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Prettyprinter.Render.Terminal
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Paths_tasklite_server ()
import System.FilePath ((</>))
import qualified Database.SQLite.Simple as SQLite
import Network.Wai.Middleware.Cors (simpleCors)

import Migrations
-- import Paths_tasklite_server (version)  -- Special module provided by Cabal
import Utils

import Config (Config(..), HookSet(..), HooksConfig(..), addHookFilesToConfig)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import Task
import FullTask


type TaskAPI = "tasks" :> QueryParams "tags" Text :> Get '[JSON] [FullTask]
          :<|> "tags" :> Get '[JSON] [Tag]


type Tag = (Text, Integer, Integer, Double)


taskAPI :: Proxy TaskAPI
taskAPI =
  Proxy


server :: Config -> Server TaskAPI
server conf =
  getTasks conf
  :<|> getTags conf


getTasks :: Config -> [Text] -> Servant.Handler [FullTask]
getTasks conf tags = do
  liftIO $
    -- TODO: Use Task instead of FullTask to fix broken notes export
    execWithConn conf $ \connection ->
      getWithTag connection (Just IsOpen) tags


getTags :: Config -> Servant.Handler [Tag]
getTags conf =
  liftIO $ execWithConn conf $ \connection ->
    SQLite.query_ connection "select * from tags" :: IO [Tag]


-- `serve` comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a web-server.
app :: Config -> Application
app conf =
  simpleCors $ serve taskAPI $ server conf


startServer :: [Char] -> Config -> IO ()
startServer appName config = do
  let dataPath = config.dataDir

  configNormDataDir <-
    if null dataPath
    then do
      xdgDataDir <- getXdgDirectory XdgData appName
      pure $ config {dataDir = xdgDataDir}
    else
      case T.stripPrefix "~/" $ T.pack dataPath of
        Nothing -> pure $ config
        Just rest -> do
          homeDir <- getHomeDirectory
          pure $ config { dataDir = homeDir </> T.unpack rest }


  let hooksPath = configNormDataDir.hooks & directory

  configNormHookDir <-
    if null hooksPath
    then pure $
      configNormDataDir
        { hooks = (configNormDataDir.hooks)
          { directory = dataDir configNormDataDir </> "hooks" }
        }
    else
      case T.stripPrefix "~/" $ T.pack hooksPath of
        Nothing -> pure $ configNormDataDir
        Just rest -> do
          homeDir <- getHomeDirectory
          pure $ configNormDataDir
            { hooks = (configNormDataDir.hooks)
              { directory = homeDir </> T.unpack rest }
            }

  let hooksPathNorm = configNormHookDir & hooks & directory

  createDirectoryIfMissing True hooksPathNorm

  hookFiles <- listDirectory hooksPathNorm

  hookFilesPerm :: [(FilePath, Permissions)] <- sequence $ hookFiles
    & filter (\name ->
        ("pre-" `isPrefixOf` name) || ("post-" `isPrefixOf` name))
    <&> (hooksPathNorm </>)
    <&> \path -> do
            perm <- getPermissions path
            pure (path, perm)

  hookFilesPermContent <- sequence $ hookFilesPerm
    & filter (\(_, perm) -> executable perm)
    <&> \(filePath, perm) -> do
            fileContent <- readFile filePath
            pure (filePath, perm, fileContent)


  let configNorm = addHookFilesToConfig configNormHookDir hookFilesPermContent

  preLaunchResult <- executeHooks "" (configNorm.hooks & launch & pre)
  putDoc preLaunchResult

  connection <- setupConnection configNorm

  -- For debugging SQLite interactions
  -- SQLite.setTrace connection $ Just print

  migrationsStatus <- runMigrations configNorm connection

  putDoc migrationsStatus

  -- nowElapsed <- timeCurrentP

  -- let
  --   now = timeFromElapsedP nowElapsed :: DateTime

  args <- getArgs
  postLaunchResult <- executeHooks
    (TL.toStrict $ TL.decodeUtf8 $ Aeson.encode $ object ["arguments" .= args])
    (configNorm.hooks & launch & post)
  putDoc postLaunchResult


  let port = 8081
  putStrLn $ "Starting server at http://localhost:" ++ show (port :: Int)
  run port $ app configNorm


exampleConfig :: Text
exampleConfig = $(
    makeRelativeToProject "../tasklite-core/example-config.yaml"
    >>= embedStringFile
  )


main :: IO ()
main = do
  -- Necessary for Docker image
  setLocaleEncoding utf8

  let appName = "tasklite"

  configDirectory <- getXdgDirectory XdgConfig appName
  createDirectoryIfMissing True configDirectory

  let configPath = configDirectory </> "config.yaml"

  configResult <- decodeFileEither configPath

  case configResult of
    Left error -> do
      if "file not found" `T.isInfixOf`
            (T.pack $ prettyPrintParseException error)
      then do
        writeFile configPath exampleConfig
        configResult2 <- decodeFileEither configPath

        case configResult2 of
          Left error2 -> die $ T.pack $ prettyPrintParseException error2
          Right configUser -> startServer appName configUser
      else
        die $ T.pack $ prettyPrintParseException error

    Right configUser ->
      startServer appName configUser
