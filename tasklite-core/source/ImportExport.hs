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
import Data.Monoid.Extra (mwhen)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.ULID (ulidFromInteger)
import Data.ULID.TimeStamp (ULIDTimeStamp, getULIDTimeStamp)
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
  addEmptyTask,
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
  hPutDoc,
  putDoc,
 )
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
import System.FilePath (isExtensionOf, takeExtension, (</>))
import System.Posix.User (getEffectiveUserName)
import System.Process (readProcess)
import Task (Task (..), emptyTask, setMetadataField, taskToEditableMarkdown)
import Taskwarrior (fullTaskToTwJson)
import Text.Editor (markdownTemplate, runUserEditorDWIM)
import Text.Parsec.Rfc2822 qualified as Email
import Text.ParserCombinators.Parsec as Parsec (parse)
import Text.PortableLines.ByteString.Lazy (lines8)
import Time.System (dateCurrent, timeCurrent, timeCurrentP)
import Utils (
  IdText,
  colr,
  countCharTL,
  emptyUlid,
  formatElapsedP,
  setDateTime,
  ulidTextToDateTime,
  zeroUlidTxt,
  zonedTimeToDateTime,
  (<!!>),
  (<$$>),
 )


insertImportTask :: Config -> Connection -> ImportTask -> IO (Doc AnsiStyle)
insertImportTask conf connection importTask = do
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
      conf
      connection
      (ulidTextToDateTime taskNorm.ulid)
      taskNorm
      importTask.tags
  noteWarnings <-
    insertNotes
      conf
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


insertImported :: Config -> Connection -> ImportTask -> IO ()
insertImported conf connection task = do
  importTaskNorm <- task & setMissingFields
  result <- insertImportTask conf connection importTaskNorm
  putDoc result


importJson :: Config -> Connection -> IO (Doc AnsiStyle)
importJson conf connection = do
  content <- BSL.getContents
  -- Try to decode as an array first
  case Aeson.eitherDecode content of
    Right (importTaskRecs :: [ImportTask]) -> do
      P.mapM_ (insertImported conf connection) importTaskRecs
      pure "Done"
    Left _ -> do
      -- If array decoding fails, try to decode as a single object
      case Aeson.eitherDecode content of
        Left error -> die $ T.pack error <> " in task \n" <> show content
        Right (importTaskRec :: ImportTask) -> do
          insertImported conf connection importTaskRec
          pure "Done"


decodeAndInsertYaml ::
  Config -> Connection -> BSL.LazyByteString -> IO (Doc AnsiStyle)
decodeAndInsertYaml conf conn content = do
  let strictContent = BSL.toStrict content
  -- Try to decode as an array first
  case Yaml.decodeEither' strictContent of
    Right (importTaskRecs :: [ImportTask]) -> do
      P.mapM_ (insertImported conf conn) importTaskRecs
      pure "Done"
    Left _ -> do
      -- If array decoding fails, try to decode as a single object
      case Yaml.decodeEither' strictContent of
        Left error ->
          die $ T.pack $ Yaml.prettyPrintParseException error
        Right (importTaskRec :: ImportTask) -> do
          importTaskNorm <- importTaskRec & setMissingFields
          insertImportTask conf conn importTaskNorm


importYaml :: Config -> Connection -> IO (Doc AnsiStyle)
importYaml conf conn = do
  content <- BSL.getContents
  decodeAndInsertYaml conf conn content


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
            bodyTextLazy = actualBody & TL.intercalate "\n" & TL.strip
            -- Only keep trailing newlines for multiline bodies
            noTrailingNewline = countCharTL '\n' bodyTextLazy == 0
            bodyText = bodyTextLazy & TL.toStrict
            frontMatterWithBody =
              frontMatterYaml
                <> "\nbody: |"
                <> mwhen noTrailingNewline "-"
                <> "\n"
                <> TL.fromStrict (bodyText & T.lines <&> ("  " <>) & T.unlines)
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
importMarkdown conf conn = do
  content <- BSL.getContents
  case parseMarkdownWithFrontMatter content of
    Left error -> die error
    Right (yamlContent, _) -> decodeAndInsertYaml conf conn yamlContent


importEml :: Config -> Connection -> IO (Doc AnsiStyle)
importEml conf connection = do
  content <- BSL.getContents

  case Parsec.parse Email.message "<stdin>" content of
    Left error -> die $ show error
    Right email -> insertImportTask conf connection $ emailToImportTask email


emailToImportTask :: Email.GenericMessage BSL.ByteString -> ImportTask
emailToImportTask email@(Email.Message headerFields msgBody) =
  let
    addBody (ImportTask task notes tags wasExplicit) =
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
        wasExplicit

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
    addHeaderToTask impTask@(ImportTask task notes tags wasExplicit) headerValue =
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
              wasExplicit
        Email.From names ->
          ImportTask
            (setMetadataField "from" (namesToJson names) task)
            notes
            tags
            wasExplicit
        Email.To names ->
          ImportTask
            (setMetadataField "to" (namesToJson names) task)
            notes
            tags
            wasExplicit
        Email.MessageID msgId ->
          ImportTask
            (setMetadataField "messageId" (Aeson.String $ T.pack msgId) task)
            notes
            tags
            wasExplicit
        Email.Subject subj ->
          ImportTask
            task{Task.body = task.body <> T.pack subj}
            notes
            tags
            wasExplicit
        Email.Keywords kwords ->
          ImportTask
            task
            notes
            (tags <> fmap (T.unwords . fmap T.pack) kwords)
            wasExplicit
        Email.Comments cmnts ->
          ImportTask
            (setMetadataField "comments" (Aeson.String $ T.pack cmnts) task)
            notes
            tags
            wasExplicit
        _ -> impTask
  in
    foldl addHeaderToTask (addBody emptyImportTask) headerFields


isDirError :: Config -> FilePath -> P.SomeException -> IO (Doc AnsiStyle)
isDirError conf filePath exception = do
  if "is a directory" `T.isInfixOf` show exception
    then do
      hPutDoc stderr $
        annotate (colr conf Red) $
          ("ERROR: \"" <> pretty filePath <> "\" is a directory. ")
            <> "Use `importdir` instead."
      die ""
    else die $ show exception


importFile :: Config -> Connection -> FilePath -> IO (Doc AnsiStyle)
importFile conf conn filePath = do
  let decodeAndInsertMd content =
        case parseMarkdownWithFrontMatter content of
          Left error -> die error
          Right (yamlContent, _) -> decodeAndInsertYaml conf conn yamlContent

  catchAll
    ( do
        content <- BSL.readFile filePath
        let fileExt = filePath & takeExtension
        case fileExt of
          ".json" -> do
            -- Try to decode as an array first
            case Aeson.eitherDecode content of
              Right (importTaskRecs :: [ImportTask]) -> do
                P.mapM_ (insertImported conf conn) importTaskRecs
                pure "Done"
              Left _ -> do
                -- If array decoding fails, try to decode as a single object
                case Aeson.eitherDecode content of
                  Left error ->
                    die $ T.pack error <> " in task \n" <> show content
                  Right (importTaskRec :: ImportTask) -> do
                    importTaskNorm <- importTaskRec & setMissingFields
                    insertImportTask conf conn importTaskNorm
          ".yaml" -> decodeAndInsertYaml conf conn content
          ".yml" -> decodeAndInsertYaml conf conn content
          ".md" -> decodeAndInsertMd content
          ".markdown" -> decodeAndInsertMd content
          ".eml" ->
            case Parsec.parse Email.message filePath content of
              Left error -> die $ show error
              Right email -> insertImportTask conf conn $ emailToImportTask email
          _ ->
            die $ T.pack $ "File type " <> fileExt <> " is not supported"
    )
    (isDirError conf filePath)


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
            [ insertImportTask conf connection importTaskNorm
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
                  [ insertImportTask conf connection importTaskNorm
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
                  [ insertImportTask conf connection taskRecord
                  , editTaskByTask conf OpenEditor connection task
                  ]
          fileExt ->
            die $ T.pack $ "File type " <> fileExt <> " is not supported"

        removeFile filePath

        pure $
          P.fold resultDocs
            <> ("❌ Deleted file" <+> dquotes (pretty filePath))
    )
    (isDirError conf filePath)


ingestDir :: Config -> Connection -> FilePath -> IO (Doc AnsiStyle)
ingestDir conf connection dirPath = do
  files <- listDirectory dirPath
  let filePaths =
        files
          & P.filter filterImportable
            <&> (dirPath </>)
  filePaths
    & P.mapM_
      ( \filePath -> do
          resultDoc <- ingestFile conf connection filePath
          putDoc $ resultDoc <> hardline <> hardline
      )
  pure $
    "📥 Ingested"
      <+> pretty (P.length filePaths)
      <+> "file(s) from"
      <+> dquotes (pretty dirPath)


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


dumpTaskwarrior :: Config -> IO (Doc AnsiStyle)
dumpTaskwarrior conf = do
  execWithConn conf $ \conn -> do
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

    let twLines = tasks <&> (fullTaskToTwJson >>> Aeson.encode >>> TL.decodeUtf8 >>> pretty)
    pure $ vsep twLines


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


{-| Edit task until it's valid Markdown with frontmatter and can be decoded.
| Return the the tuple `(task, valid YAML content from frontmatter)`
-}
editUntilValidMarkdown ::
  EditMode ->
  Connection ->
  P.ByteString ->
  P.ByteString ->
  IO (Either ParseException (ImportTask, P.ByteString))
editUntilValidMarkdown editMode conn initialMarkdown wipMarkdown = do
  markdownAfterEdit <- case editMode of
    ApplyPreEdit editFunc -> pure $ editFunc wipMarkdown
    OpenEditor -> runUserEditorDWIM markdownTemplate wipMarkdown
    OpenEditorRequireEdit -> runUserEditorDWIM markdownTemplate wipMarkdown

  if markdownAfterEdit == initialMarkdown
    then pure $ Left $ InvalidYaml $ Just $ YamlException $ case editMode of
      -- Content doesn't have to be changed -> log nothing
      OpenEditor -> ""
      _ -> "⚠️ Nothing changed"
    else do
      case markdownAfterEdit & BSL.fromStrict & parseMarkdownWithFrontMatter of
        Left error -> do
          putErrLn $ error <> "\n"
          editUntilValidMarkdown editMode conn initialMarkdown markdownAfterEdit
        Right (yamlContent, _) -> do
          case BSL.toStrict yamlContent & Yaml.decodeEither' of
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
              editUntilValidMarkdown editMode conn initialMarkdown markdownAfterEdit
            Right newTask -> do
              pure $ Right (newTask, BSL.toStrict yamlContent)


insertTaskFromEdit ::
  Config ->
  Connection ->
  ImportTask ->
  P.ByteString ->
  P.Text ->
  Maybe P.Text ->
  IO (Doc AnsiStyle)
insertTaskFromEdit conf conn importTaskRec newContent modified_utc origClosedUtc = do
  -- Insert empty task if the edited task was newly created
  ulid <-
    if T.null importTaskRec.task.ulid
      then addEmptyTask conf conn <&> Task.ulid
      else pure importTaskRec.task.ulid

  effectiveUserName <- getEffectiveUserName
  now <- getULIDTimeStamp <&> (show @ULIDTimeStamp >>> T.toLower)
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
        { Task.ulid = ulid
        , Task.user =
            if importTaskRec.task.user == ""
              then T.pack effectiveUserName
              else importTaskRec.task.user
        , Task.metadata =
            if hasMetadata == Just True
              then importTaskRec.task.metadata
              else Nothing
        , -- Set to previous value to force SQL trigger to update it
          Task.modified_utc = modified_utc
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

  let
    -- Should preserve closed_utc if:
    -- 1. Original task already had a closed_utc (was already closed), OR
    -- 2. User explicitly provided a closed_utc value in the edit
    shouldPreserveClosedUtc =
      P.isJust origClosedUtc || importTaskRec.closedUtcWasExplicit
    taskFixedUtc =
      if P.not shouldPreserveClosedUtc
        then taskFixed
        else
          taskFixed
            { Task.modified_utc =
                nowDateTime
                  & timePrint (toFormat importUtcFormat)
                  & T.pack
            }

  -- Update again with the same `state` field to avoid firing
  -- SQL trigger which would overwrite the `closed_utc` field.
  -- Only do this if closed_utc should be preserved.
  -- When closing a task for the first time without explicit closed_utc,
  -- let the trigger set closed_utc to the current time.
  P.when shouldPreserveClosedUtc $ do
    updateTask conn taskFixedUtc

  tagWarnings <- insertTags conf conn Nothing taskFixedUtc importTaskRec.tags
  noteWarnings <- insertNotes conf conn Nothing taskFixedUtc notesCorrectUtc

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


enterTask :: Config -> Connection -> IO (Doc AnsiStyle)
enterTask conf conn = do
  taskMarkdown <- taskToEditableMarkdown conn emptyTask
  taskMarkdownTupleRes <-
    editUntilValidMarkdown OpenEditorRequireEdit conn taskMarkdown taskMarkdown
  case taskMarkdownTupleRes of
    Left error -> case error of
      InvalidYaml (Just (YamlException "")) -> pure P.mempty
      _ -> pure $ pretty $ Yaml.prettyPrintParseException error
    Right (importTaskRec, newContent) -> do
      modified_utc <- formatElapsedP conf timeCurrentP
      insertTaskFromEdit conf conn importTaskRec newContent modified_utc Nothing


editTaskByTask :: Config -> EditMode -> Connection -> Task -> IO (Doc AnsiStyle)
editTaskByTask conf editMode conn taskToEdit = do
  taskMarkdown <- taskToEditableMarkdown conn taskToEdit
  taskMarkdownTupleRes <-
    editUntilValidMarkdown editMode conn taskMarkdown taskMarkdown
  case taskMarkdownTupleRes of
    Left error -> case error of
      InvalidYaml (Just (YamlException "")) -> pure P.mempty
      _ -> pure $ pretty $ Yaml.prettyPrintParseException error
    Right (importTaskRec, newContent) -> do
      insertTaskFromEdit
        conf
        conn
        importTaskRec
        newContent
        taskToEdit.modified_utc
        taskToEdit.closed_utc


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
            pure (fullImportTask, formatHookResult conf hookResult)
      _ -> do
        pure
          ( importTaskDraft
          , annotate (colr conf Red) $
              "ERROR: Multiple pre-add hooks are not supported yet. "
                <> "None of the hooks were executed."
          )

    updateTask conn importTask.task
    warnings <- insertTags conf conn Nothing importTask.task importTask.tags

    putDoc $
      preModifyHookMsg
        <!!> warnings
        <!!> hardline

    editTaskByTask conf OpenEditorRequireEdit conn importTask.task
