{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use maybe" #-}

{-|
Functions to import and export tasks
-}
module ImportExport where

import Protolude (
  Applicative (pure),
  Bool (..),
  Char,
  Either (..),
  Eq ((==)),
  FilePath,
  Foldable (foldl),
  Functor (fmap),
  Hashable (hash),
  IO,
  Integral (toInteger),
  Maybe (..),
  Num (abs),
  Semigroup ((<>)),
  Text,
  Traversable (sequence),
  die,
  fromMaybe,
  putErrLn,
  rightToMaybe,
  show,
  stderr,
  toStrict,
  ($),
  (&),
  (+),
  (.),
  (<$>),
  (<&>),
  (=<<),
  (||),
 )
import Protolude qualified as P

import Config (
  Config (dataDir, dbName, hooks),
  HookSet (post, pre),
  HooksConfig (modify),
 )
import Control.Arrow ((>>>))
import Control.Monad.Catch (catchAll)
import Data.Aeson (Value, object, (.=))
import Data.Aeson as Aeson (
  Value (Array, Object, String),
  eitherDecode,
  encode,
 )
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString.Lazy qualified as BSL
import Data.Csv qualified as Csv
import Data.Hourglass (
  TimeFormat (toFormat),
  timePrint,
 )
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.ULID (ulidFromInteger)
import Data.ULID.TimeStamp (getULIDTimeStamp)
import Data.Vector qualified as V
import Data.Yaml (
  ParseException (InvalidYaml),
  YamlException (YamlException, YamlParseException),
  YamlMark (YamlMark),
 )
import Data.Yaml qualified as Yaml
import Database.SQLite.Simple (Connection, Only (Only), query, query_)
import Database.SQLite.Simple.QQ (sql)
import FullTask (FullTask (..))
import Hooks (HookResult (message, task), executeHooks, formatHookResult)
import ImportTask (
  ImportTask (..),
  emptyImportTask,
  importTaskToFullTask,
  importUtcFormat,
  setMissingFields,
 )
import Lib (
  execWithConn,
  execWithTask,
  insertNotes,
  insertRecord,
  insertTags,
  updateTask,
 )
import Note (Note (..))
import Prettyprinter (
  Doc,
  Pretty (pretty),
  annotate,
  dquotes,
  hardline,
  vsep,
  (<+>),
 )
import Prettyprinter.Internal.Type (Doc (Empty))
import Prettyprinter.Render.Terminal (
  AnsiStyle,
  Color (Red),
  color,
  hPutDoc,
  putDoc,
 )
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
import System.FilePath (isExtensionOf, takeExtension, (</>))
import System.Posix.User (getEffectiveUserName)
import System.Process (readProcess)
import Task (Task (..), setMetadataField, taskToEditableYaml)
import Text.Editor (runUserEditorDWIM, yamlTemplate)
import Text.Parsec.Rfc2822 qualified as Email
import Text.ParserCombinators.Parsec as Parsec (parse)
import Text.PortableLines.ByteString.Lazy (lines8)
import Time.System (dateCurrent, timeCurrent)
import Utils (
  IdText,
  emptyUlid,
  setDateTime,
  ulidTextToDateTime,
  zeroUlidTxt,
  zonedTimeToDateTime,
  (<!!>),
  (<$$>),
 )


insertImportTask :: Connection -> ImportTask -> IO (Doc AnsiStyle)
insertImportTask connection importTask = do
  effectiveUserName <- getEffectiveUserName
  let taskNorm =
        importTask.task
          { Task.user =
              if importTask.task.user == ""
                then T.pack effectiveUserName
                else importTask.task.user
          }
  insertRecord "tasks" connection taskNorm
  tagWarnings <-
    insertTags
      connection
      (ulidTextToDateTime taskNorm.ulid)
      taskNorm
      importTask.tags
  noteWarnings <-
    insertNotes
      connection
      (ulidTextToDateTime taskNorm.ulid)
      taskNorm
      importTask.notes
  pure $
    tagWarnings
      <$$> noteWarnings
      <$$> "📥 Imported task"
      <+> dquotes (pretty taskNorm.body)
      <+> "with ulid"
      <+> dquotes (pretty taskNorm.ulid)
      <+> hardline


importJson :: Config -> Connection -> IO (Doc AnsiStyle)
importJson _ connection = do
  content <- BSL.getContents

  case Aeson.eitherDecode content of
    Left error -> die $ T.pack error <> " in task \n" <> show content
    Right importTaskRec -> do
      importTaskNorm <- importTaskRec & setMissingFields
      insertImportTask connection importTaskNorm


decodeAndInsertYaml :: Connection -> BSL.LazyByteString -> IO (Doc AnsiStyle)
decodeAndInsertYaml conn content = do
  case content & BSL.toStrict & Yaml.decodeEither' of
    Left error ->
      die $ T.pack $ Yaml.prettyPrintParseException error
    Right importTaskRec -> do
      importTaskNorm <- importTaskRec & setMissingFields
      insertImportTask conn importTaskNorm


importYaml :: Config -> Connection -> IO (Doc AnsiStyle)
importYaml _ conn = do
  content <- BSL.getContents
  decodeAndInsertYaml conn content


parseMarkdownWithFrontMatter ::
  BSL.LazyByteString -> Either Text (BSL.LazyByteString, Text)
parseMarkdownWithFrontMatter content = do
  let
    contentText = TL.decodeUtf8 content
    contentLines = TL.lines contentText

    isClosingDelimiter line = line == "---" || line == "..."

  case contentLines of
    ("---" : rest) -> do
      let (frontMatterLines, bodyLines) = P.break isClosingDelimiter rest
      case bodyLines of
        (closingDelim : actualBody) | isClosingDelimiter closingDelim -> do
          let
            frontMatterYaml = TL.intercalate "\n" frontMatterLines
            bodyText =
              actualBody
                & TL.intercalate "\n"
                & TL.stripStart
                & TL.toStrict
            frontMatterWithBody =
              frontMatterYaml
                <> "\nbody: |\n"
                <> TL.fromStrict (T.unlines $ T.lines bodyText <&> ("  " <>))
          Right (TL.encodeUtf8 frontMatterWithBody, bodyText)
        _ -> Left "Missing closing front-matter delimiter '---' or '...'"
    _ -> do
      let
        bodyText = TL.toStrict contentText
        yamlWithBody =
          "body: |\n"
            <> TL.fromStrict (T.unlines $ T.lines bodyText <&> ("  " <>))
      Right (TL.encodeUtf8 yamlWithBody, bodyText)


importMarkdown :: Config -> Connection -> IO (Doc AnsiStyle)
importMarkdown _ conn = do
  content <- BSL.getContents
  case parseMarkdownWithFrontMatter content of
    Left error -> die error
    Right (yamlContent, _) -> decodeAndInsertYaml conn yamlContent


importEml :: Config -> Connection -> IO (Doc AnsiStyle)
importEml _ connection = do
  content <- BSL.getContents

  case Parsec.parse Email.message "<stdin>" content of
    Left error -> die $ show error
    Right email -> insertImportTask connection $ emailToImportTask email


emailToImportTask :: Email.GenericMessage BSL.ByteString -> ImportTask
emailToImportTask email@(Email.Message headerFields msgBody) =
  let
    addBody (ImportTask task notes tags) =
      ImportTask
        task
          { Task.body =
              task.body
                <> ( msgBody
                      & lines8
                      <&> (TL.decodeUtf8 >>> toStrict)
                      & T.unlines
                      & T.dropEnd 1
                   )
          }
        notes
        tags

    namesToJson names =
      Array $
        V.fromList $
          names
            <&> ( \(Email.NameAddr name emailAddress) ->
                    Object $
                      KeyMap.fromList
                        [ ("name", Aeson.String $ T.pack $ fromMaybe "" name)
                        , ("email", Aeson.String $ T.pack emailAddress)
                        ]
                )

    addHeaderToTask :: ImportTask -> Email.Field -> ImportTask
    addHeaderToTask impTask@(ImportTask task notes tags) headerValue =
      case headerValue of
        Email.Date emailDate ->
          let
            utc = zonedTimeToDateTime emailDate
            ulidGeneratedRes =
              (email & show :: Text)
                & (hash >>> toInteger >>> abs >>> ulidFromInteger)
            ulidCombined =
              (ulidGeneratedRes & P.fromRight emptyUlid)
                `setDateTime` utc
          in
            ImportTask
              task
                { Task.ulid = T.toLower $ show ulidCombined
                , Task.modified_utc =
                    T.pack $ timePrint (toFormat importUtcFormat) utc
                }
              notes
              tags
        Email.From names ->
          ImportTask
            (setMetadataField "from" (namesToJson names) task)
            notes
            tags
        Email.To names ->
          ImportTask
            (setMetadataField "to" (namesToJson names) task)
            notes
            tags
        Email.MessageID msgId ->
          ImportTask
            (setMetadataField "messageId" (Aeson.String $ T.pack msgId) task)
            notes
            tags
        Email.Subject subj ->
          ImportTask
            task{Task.body = task.body <> T.pack subj}
            notes
            tags
        Email.Keywords kwords ->
          ImportTask
            task
            notes
            (tags <> fmap (T.unwords . fmap T.pack) kwords)
        Email.Comments cmnts ->
          ImportTask
            (setMetadataField "comments" (Aeson.String $ T.pack cmnts) task)
            notes
            tags
        _ -> impTask
  in
    foldl addHeaderToTask (addBody emptyImportTask) headerFields


isDirError :: FilePath -> P.SomeException -> IO (Doc AnsiStyle)
isDirError filePath exception = do
  if "is a directory" `T.isInfixOf` show exception
    then do
      hPutDoc stderr $
        annotate (color Red) $
          ("ERROR: \"" <> pretty filePath <> "\" is a directory. ")
            <> "Use `importdir` instead."
      die ""
    else die $ show exception


importFile :: Config -> Connection -> FilePath -> IO (Doc AnsiStyle)
importFile _ conn filePath = do
  let decodeAndInsertMd content =
        case parseMarkdownWithFrontMatter content of
          Left error -> die error
          Right (yamlContent, _) -> decodeAndInsertYaml conn yamlContent

  catchAll
    ( do
        content <- BSL.readFile filePath
        let fileExt = filePath & takeExtension
        case fileExt of
          ".json" -> do
            let decodeResult = Aeson.eitherDecode content
            case decodeResult of
              Left error ->
                die $ T.pack error <> " in task \n" <> show content
              Right importTaskRec -> do
                importTaskNorm <- importTaskRec & setMissingFields
                insertImportTask conn importTaskNorm
          ".yaml" -> decodeAndInsertYaml conn content
          ".yml" -> decodeAndInsertYaml conn content
          ".md" -> decodeAndInsertMd content
          ".markdown" -> decodeAndInsertMd content
          ".eml" ->
            case Parsec.parse Email.message filePath content of
              Left error -> die $ show error
              Right email -> insertImportTask conn $ emailToImportTask email
          _ ->
            die $ T.pack $ "File type " <> fileExt <> " is not supported"
    )
    (isDirError filePath)


filterImportable :: FilePath -> Bool
filterImportable filePath =
  (".json" `isExtensionOf` filePath)
    || (".yaml" `isExtensionOf` filePath)
    || (".yml" `isExtensionOf` filePath)
    || (".md" `isExtensionOf` filePath)
    || (".markdown" `isExtensionOf` filePath)
    || (".eml" `isExtensionOf` filePath)


importDir :: Config -> Connection -> FilePath -> IO (Doc AnsiStyle)
importDir conf connection dirPath = do
  files <- listDirectory dirPath
  resultDocs <-
    files
      & P.filter filterImportable
      <&> (dirPath </>)
      & P.mapM (importFile conf connection)
  pure $ P.fold resultDocs


ingestFile :: Config -> Connection -> FilePath -> IO (Doc AnsiStyle)
ingestFile conf connection filePath = do
  let
    ingestYaml content = do
      let decodeResult = Yaml.decodeEither' (BSL.toStrict content)
      case decodeResult of
        Left error ->
          die $ T.pack $ Yaml.prettyPrintParseException error
        Right importTaskRec -> do
          importTaskNorm <- importTaskRec & setMissingFields
          sequence
            [ insertImportTask connection importTaskNorm
            , editTaskByTask
                conf
                OpenEditor
                connection
                importTaskNorm.task
            ]
    ingestMd content = do
      case parseMarkdownWithFrontMatter content of
        Left error -> die error
        Right (yamlContent, _) -> ingestYaml yamlContent

  catchAll
    ( do
        content <- BSL.readFile filePath
        resultDocs <- case takeExtension filePath of
          ".json" -> do
            let decodeResult = Aeson.eitherDecode content
            case decodeResult of
              Left error ->
                die $ T.pack error <> " in task \n" <> show content
              Right importTaskRec -> do
                importTaskNorm <- importTaskRec & setMissingFields
                sequence
                  [ insertImportTask connection importTaskNorm
                  , editTaskByTask
                      conf
                      OpenEditor
                      connection
                      importTaskNorm.task
                  ]
          ".yaml" -> ingestYaml content
          ".yml" -> ingestYaml content
          ".md" -> ingestMd content
          ".markdown" -> ingestMd content
          ".eml" ->
            case Parsec.parse Email.message filePath content of
              Left error -> die $ show error
              Right email -> do
                let taskRecord@ImportTask{task} = emailToImportTask email
                sequence
                  [ insertImportTask connection taskRecord
                  , editTaskByTask conf OpenEditor connection task
                  ]
          fileExt ->
            die $ T.pack $ "File type " <> fileExt <> " is not supported"

        removeFile filePath

        pure $
          P.fold resultDocs
            <> ("❌ Deleted file" <+> dquotes (pretty filePath))
    )
    (isDirError filePath)


ingestDir :: Config -> Connection -> FilePath -> IO (Doc AnsiStyle)
ingestDir conf connection dirPath = do
  files <- listDirectory dirPath
  resultDocs <-
    files
      & P.filter filterImportable
      <&> (dirPath </>)
      & P.mapM (ingestFile conf connection)
  pure $ P.fold resultDocs


-- TODO: Use Task instead of FullTask to fix broken notes export
dumpCsv :: Config -> IO (Doc AnsiStyle)
dumpCsv conf = do
  execWithConn conf $ \connection -> do
    rows :: [FullTask] <- query_ connection "SELECT * FROM tasks_view"
    pure $ pretty $ TL.decodeUtf8 $ Csv.encodeDefaultOrderedByName rows


getNdjsonLines :: Connection -> IO [Doc AnsiStyle]
getNdjsonLines conn = do
  -- TODO: Fix after tasks_view is updated to include notes
  tasksWithoutNotes :: [FullTask] <- query_ conn "SELECT * FROM tasks_view"
  tasks <-
    tasksWithoutNotes
      & P.mapM
        ( \task -> do
            notes <-
              query
                conn
                [sql|
                  SELECT ulid, note
                  FROM task_to_note
                  WHERE task_ulid == ?
                |]
                (Only task.ulid)

            pure $
              task
                { FullTask.notes =
                    if P.null notes then Nothing else Just notes
                }
        )

  pure $ tasks <&> (Aeson.encode >>> TL.decodeUtf8 >>> pretty)


dumpNdjson :: Config -> IO (Doc AnsiStyle)
dumpNdjson conf = do
  execWithConn conf $ \conn -> do
    lines <- getNdjsonLines conn
    pure $ vsep lines


dumpJson :: Config -> IO (Doc AnsiStyle)
dumpJson conf = do
  -- TODO: Use Task instead of FullTask to fix broken notes export
  execWithConn conf $ \connection -> do
    tasks :: [FullTask] <- query_ connection "SELECT * FROM tasks_view"
    pure $ pretty $ fmap (TL.decodeUtf8 . Aeson.encode) tasks


dumpSql :: Config -> IO (Doc AnsiStyle)
dumpSql conf = do
  result <-
    readProcess
      "sqlite3"
      [ conf.dataDir </> conf.dbName
      , ".dump"
      ]
      []
  pure $ pretty result


backupDatabase :: Config -> IO (Doc AnsiStyle)
backupDatabase conf = do
  now <- timeCurrent

  let
    fileUtcFormat = toFormat ("YYYY-MM-DDtHMI" :: [Char])
    backupDirName = "backups"
    backupDirPath = conf.dataDir </> backupDirName
    backupFilePath = backupDirPath </> timePrint fileUtcFormat now <> ".db"

  -- Create directory (and parents because of True)
  createDirectoryIfMissing True backupDirPath

  result <-
    pretty
      <$> readProcess
        "sqlite3"
        [ conf.dataDir </> conf.dbName
        , ".backup '" <> backupFilePath <> "'"
        ]
        []

  pure $
    result
      <> hardline
      <> pretty
        ( "✅ Backed up database \""
            <> conf.dbName
            <> "\" to \""
            <> backupFilePath
            <> "\""
        )


data EditMode
  = ApplyPreEdit (P.ByteString -> P.ByteString)
  | OpenEditor
  | OpenEditorRequireEdit


{-| Edit the task until it is valid YAML and can be decoded.
| Return the the tuple `(task, valid YAML content)`
-}
editUntilValidYaml ::
  EditMode ->
  Connection ->
  P.ByteString ->
  P.ByteString ->
  IO (Either ParseException (ImportTask, P.ByteString))
editUntilValidYaml editMode conn initialYaml wipYaml = do
  yamlAfterEdit <- case editMode of
    ApplyPreEdit editFunc -> pure $ editFunc wipYaml
    OpenEditor -> runUserEditorDWIM yamlTemplate wipYaml
    OpenEditorRequireEdit -> runUserEditorDWIM yamlTemplate wipYaml

  if yamlAfterEdit == initialYaml
    then pure $ Left $ InvalidYaml $ Just $ YamlException $ case editMode of
      -- Content doesn't have to be changed -> log nothing
      OpenEditor -> ""
      _ -> "⚠️ Nothing changed"
    else do
      case yamlAfterEdit & Yaml.decodeEither' of
        Left error -> do
          case error of
            -- Adjust the line and column numbers to be 1-based
            InvalidYaml
              (Just (YamlParseException prblm ctxt (YamlMark idx line col))) ->
                let yamlMark = YamlMark (idx + 1) (line + 1) (col + 1)
                in  putErrLn $
                      Yaml.prettyPrintParseException
                        ( InvalidYaml
                            (Just (YamlParseException prblm ctxt yamlMark))
                        )
                        <> "\n"
            _ ->
              putErrLn $ Yaml.prettyPrintParseException error <> "\n"
          editUntilValidYaml editMode conn initialYaml yamlAfterEdit
        ---
        Right newTask -> do
          pure $ Right (newTask, yamlAfterEdit)


editTaskByTask :: Config -> EditMode -> Connection -> Task -> IO (Doc AnsiStyle)
editTaskByTask conf editMode conn taskToEdit = do
  taskYaml <- taskToEditableYaml conn taskToEdit
  taskYamlTupleRes <- editUntilValidYaml editMode conn taskYaml taskYaml
  case taskYamlTupleRes of
    Left error -> case error of
      InvalidYaml (Just (YamlException "")) -> pure P.mempty
      _ -> pure $ pretty $ Yaml.prettyPrintParseException error
    Right (importTaskRec, newContent) -> do
      effectiveUserName <- getEffectiveUserName
      now <- getULIDTimeStamp <&> (show >>> T.toLower)
      let
        parseMetadata :: Value -> Parser Bool
        parseMetadata val = case val of
          Object obj -> do
            let mdataMaybe = KeyMap.lookup "metadata" obj
            pure $ case mdataMaybe of
              Just (Object _) -> True
              _ -> False
          _ -> pure False

        hasMetadata =
          parseMaybe parseMetadata
            =<< rightToMaybe (Yaml.decodeEither' newContent)

        taskFixed =
          importTaskRec.task
            { Task.user =
                if importTaskRec.task.user == ""
                  then T.pack effectiveUserName
                  else importTaskRec.task.user
            , Task.metadata =
                if hasMetadata == Just True
                  then importTaskRec.task.metadata
                  else Nothing
            , -- Set to previous value to force SQL trigger to update it
              Task.modified_utc = taskToEdit.modified_utc
            }
        notesCorrectUtc =
          importTaskRec.notes
            <&> ( \note ->
                    note
                      { Note.ulid =
                          if zeroUlidTxt `T.isPrefixOf` note.ulid
                            then note.ulid & T.replace zeroUlidTxt now
                            else note.ulid
                      }
                )

      updateTask conn taskFixed

      nowDateTime <- dateCurrent

      let taskFixedUtc =
            if P.isNothing taskFixed.closed_utc
              then taskFixed
              else
                taskFixed
                  { Task.modified_utc =
                      nowDateTime
                        & timePrint (toFormat importUtcFormat)
                        & T.pack
                  }

      -- TODO: Remove after it was added to `createSetClosedUtcTrigger`
      -- Update again with the same `state` field to avoid firing
      -- SQL trigger which would overwrite the `closed_utc` field.
      P.when (P.isJust taskFixed.closed_utc) $ do
        updateTask conn taskFixedUtc

      tagWarnings <- insertTags conn Nothing taskFixedUtc importTaskRec.tags
      noteWarnings <- insertNotes conn Nothing taskFixedUtc notesCorrectUtc

      args <- P.getArgs
      postModifyResults <-
        executeHooks
          ( TL.toStrict $
              TL.decodeUtf8 $
                Aeson.encode $
                  object
                    [ "arguments" .= args
                    , "taskModified" .= taskFixedUtc
                    -- TODO: Add tags and notes to task
                    ]
          )
          conf.hooks.modify.post

      let postModifyHookMsg =
            ( postModifyResults
                <&> \case
                  Left error -> "ERROR:" <+> pretty error
                  Right hookResult -> pretty hookResult.message
                & P.fold
            )
              <> hardline

      pure $
        tagWarnings
          <$$> noteWarnings
          <$$> "✏️  Edited task"
          <+> dquotes (pretty taskFixed.body)
          <+> "with ulid"
          <+> dquotes (pretty taskFixed.ulid)
            <!!> postModifyHookMsg


-- TODO: Eliminate code duplications with `addTask`
editTask :: Config -> Connection -> IdText -> IO (Doc AnsiStyle)
editTask conf conn idSubstr = do
  execWithTask conf conn idSubstr $ \taskToEdit -> do
    let importTaskDraft =
          emptyImportTask
            { ImportTask.task = taskToEdit
            , ImportTask.tags = []
            , ImportTask.notes = []
            }
    args <- P.getArgs
    preModifyResults <-
      executeHooks
        ( TL.toStrict $
            TL.decodeUtf8 $
              Aeson.encode $
                object
                  [ "arguments" .= args
                  , "taskToModify" .= importTaskToFullTask importTaskDraft
                  ]
        )
        conf.hooks.modify.pre

    -- Maybe the task was changed by the hook
    (importTask, preModifyHookMsg) <- case preModifyResults of
      [] -> pure (importTaskDraft, Empty)
      [Left error] -> do
        _ <- P.exitFailure
        pure (importTaskDraft, pretty error)
      [Right hookResult] -> do
        case hookResult.task of
          Nothing -> pure (importTaskDraft, Empty)
          Just importTask -> do
            fullImportTask <-
              setMissingFields
                importTask
                  { ImportTask.task =
                      importTask.task{Task.ulid = taskToEdit.ulid}
                  }
            pure (fullImportTask, formatHookResult hookResult)
      _ -> do
        pure
          ( importTaskDraft
          , annotate (color Red) $
              "ERROR: Multiple pre-add hooks are not supported yet. "
                <> "None of the hooks were executed."
          )

    updateTask conn importTask.task
    warnings <- insertTags conn Nothing importTask.task importTask.tags

    putDoc $
      preModifyHookMsg
        <!!> warnings
        <!!> hardline

    editTaskByTask conf OpenEditorRequireEdit conn importTask.task
