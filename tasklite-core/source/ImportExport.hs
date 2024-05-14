{-|
Functions to import and export tasks
-}
module ImportExport where

import Protolude (
  Alternative ((<|>)),
  Applicative (pure),
  Bool (..),
  Char,
  Either (..),
  Eq ((==)),
  FilePath,
  Foldable (foldl),
  Functor (fmap),
  Generic,
  Hashable (hash),
  IO,
  Integral (toInteger),
  Maybe (..),
  Num (abs),
  Semigroup ((<>)),
  Show,
  Text,
  Traversable (sequence),
  asum,
  die,
  fromMaybe,
  hush,
  isJust,
  optional,
  putErrLn,
  rightToMaybe,
  show,
  toStrict,
  ($),
  (&),
  (+),
  (.),
  (<$>),
  (<&>),
  (=<<),
 )
import Protolude qualified as P

import Config (Config (dataDir, dbName))
import Control.Arrow ((>>>))
import Data.Aeson as Aeson (
  FromJSON (parseJSON),
  ToJSON,
  Value (Array, Object, String),
  eitherDecode,
  encode,
  withObject,
  (.:),
  (.:?),
 )
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString.Lazy qualified as BSL
import Data.Csv qualified as Csv
import Data.Hourglass (
  DateTime,
  Time (timeFromElapsedP),
  TimeFormat (toFormat),
  TimeFormatString,
  timePrint,
 )
import Data.Text qualified as T
import Data.Text.Lazy.Encoding qualified as TL
import Data.Time.ISO8601.Duration qualified as Iso
import Data.ULID (ULID, ulidFromInteger)
import Data.ULID.TimeStamp (getULIDTimeStamp)
import Data.Vector qualified as V
import Data.Yaml (ParseException (InvalidYaml), YamlException (YamlException, YamlParseException), YamlMark (YamlMark))
import Data.Yaml qualified as Yaml
import Database.SQLite.Simple as Sql (Connection, query_)
import FullTask (FullTask)
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
  dquotes,
  hardline,
  vsep,
  (<+>),
 )
import Prettyprinter.Render.Terminal (AnsiStyle)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath (takeExtension, (</>))
import System.Posix.User (getEffectiveUserName)
import System.Process (readProcess)
import Task (
  Task (
    Task,
    awake_utc,
    body,
    closed_utc,
    due_utc,
    group_ulid,
    metadata,
    modified_utc,
    priority_adjustment,
    ready_utc,
    recurrence_duration,
    repetition_duration,
    review_utc,
    state,
    ulid,
    user,
    waiting_utc
  ),
  emptyTask,
  setMetadataField,
  taskToEditableYaml,
  textToTaskState,
 )
import Text.Editor (runUserEditorDWIM, yamlTemplate)
import Text.Parsec.Rfc2822 (GenericMessage (..), message)
import Text.Parsec.Rfc2822 qualified as Email
import Text.ParserCombinators.Parsec as Parsec (parse)
import Text.PortableLines.ByteString.Lazy (lines8)
import Time.System (dateCurrent, timeCurrent)
import Utils (
  IdText,
  emptyUlid,
  parseUlidText,
  parseUtc,
  setDateTime,
  ulidTextToDateTime,
  zeroTime,
  zeroUlidTxt,
  zonedTimeToDateTime,
  (<$$>),
 )


data Annotation = Annotation
  { entry :: Text
  , description :: Text
  }
  deriving (Generic, Eq)


instance Hashable Annotation


instance ToJSON Annotation


instance FromJSON Annotation where
  parseJSON = withObject "annotation" $ \o -> do
    entry <- o .: "entry"
    description <- o .: "description"
    pure Annotation{..}


annotationToNote :: Annotation -> Note
annotationToNote annot@Annotation{entry, description} = do
  let
    utc = entry & parseUtc & fromMaybe (timeFromElapsedP 0 :: DateTime)
    ulidGeneratedRes = annot & (hash >>> toInteger >>> abs >>> ulidFromInteger)
    ulidCombined = (ulidGeneratedRes & P.fromRight emptyUlid) `setDateTime` utc

  Note
    { ulid = (T.toLower . show) ulidCombined
    , body = description
    }


textToNote :: DateTime -> Text -> Note
textToNote utc body =
  let
    ulidGeneratedRes = body & (hash >>> toInteger >>> abs >>> ulidFromInteger)
    ulidCombined = (ulidGeneratedRes & P.fromRight emptyUlid) `setDateTime` utc
  in
    Note
      { ulid = (T.toLower . show) ulidCombined
      , body = body
      }


importUtcFormat :: TimeFormatString
importUtcFormat =
  toFormat ("YYYY-MM-DD H:MI:S" :: [Char])


data ImportTask = ImportTask
  { task :: Task
  , notes :: [Note]
  , tags :: [Text]
  }
  deriving (Show)


emptyImportTask :: ImportTask
emptyImportTask =
  ImportTask
    { task = emptyTask
    , notes = []
    , tags = []
    }


-- | Values a suffixed with a prime (') to avoid name collisions
instance FromJSON ImportTask where
  parseJSON = withObject "task" $ \o -> do
    utc <- o .:? "utc"
    entry <- o .:? "entry"
    creation <- o .:? "creation"
    creation_utc <- o .:? "creation_utc"
    creationUtc <- o .:? "creationUtc"
    created <- o .:? "created"
    created_at <- o .:? "created_at"
    createdAt <- o .:? "createdAt"
    created_utc <- o .:? "created_utc"
    createdUtc_ <- o .:? "createdUtc"

    let
      parsedCreatedUtc =
        parseUtc
          =<< ( utc
                  <|> entry
                  <|> creation
                  <|> creation_utc
                  <|> creationUtc
                  <|> created
                  <|> created_at
                  <|> createdAt
                  <|> created_utc
                  <|> createdUtc_
              )
      createdUtc = fromMaybe zeroTime parsedCreatedUtc

    o_body <- o .:? "body"
    description <- o .:? "description"
    let body = fromMaybe "" (o_body <|> description)

    o_priority_adjustment <- o .:? "priority_adjustment"
    urgency <- o .:? "urgency"
    priority <- optional (o .: "priority")
    let priority_adjustment = o_priority_adjustment <|> urgency <|> priority

    modified <- o .:? "modified"
    modified_at <- o .:? "modified_at"
    o_modified_utc <- o .:? "modified_utc"
    modification_date <- o .:? "modification_date"
    updated_at <- o .:? "updated_at"
    let
      maybeModified =
        modified
          <|> modified_at
          <|> o_modified_utc
          <|> modification_date
          <|> updated_at
      modified_utc =
        T.pack $
          timePrint importUtcFormat $
            fromMaybe createdUtc (parseUtc =<< maybeModified)

    o_state <- o .:? "state"
    status <- o .:? "status"
    let
      state = textToTaskState =<< (o_state <|> status)
      implicitCloseUtcMaybe =
        if isJust state
          then
            maybeModified
              <|> Just (T.pack $ timePrint importUtcFormat createdUtc)
          else Nothing

    o_tags <- o .:? "tags"
    o_labels <- o .:? "labels"
    project <- o .:? "project"
    let
      projects = fmap (: []) project
      tags = fromMaybe [] (o_tags <> o_labels <> projects)

    due <- o .:? "due"
    o_due_utc <- o .:? "due_utc"
    due_on <- o .:? "due_on"
    let
      maybeDue = due <|> o_due_utc <|> due_on
      due_utc =
        fmap
          (T.pack . timePrint importUtcFormat)
          (parseUtc =<< maybeDue)

    awake' <- o .:? "awake"
    awake_at' <- o .:? "awake_at"
    sleep' <- o .:? "sleep"
    sleep_utc' <- o .:? "sleep_utc"
    sleep_until' <- o .:? "sleep_until"
    wait' <- o .:? "wait"
    wait_until' <- o .:? "wait_until"
    let
      maybeAwake =
        awake'
          <|> awake_at'
          <|> sleep'
          <|> sleep_utc'
          <|> sleep_until'
          <|> wait'
          <|> wait_until'
      awake_utc =
        fmap
          (T.pack . timePrint importUtcFormat)
          (parseUtc =<< maybeAwake)

    ready' <- o .:? "ready"
    ready_since' <- o .:? "ready_since"
    ready_utc' <- o .:? "ready_utc"
    let
      maybeReady = ready' <|> ready_since' <|> ready_utc'
      ready_utc =
        fmap
          (T.pack . timePrint importUtcFormat)
          (parseUtc =<< maybeReady)

    review' <- o .:? "review"
    review_at' <- o .:? "review_at"
    review_since' <- o .:? "review_since"
    review_utc' <- o .:? "review_utc"
    let
      maybeReview = review' <|> review_at' <|> review_since' <|> review_utc'
      review_utc =
        fmap
          (T.pack . timePrint importUtcFormat)
          (parseUtc =<< maybeReview)

    waiting' <- o .:? "waiting"
    waiting_since' <- o .:? "waiting_since"
    waiting_utc' <- o .:? "waiting_utc"
    let
      maybewaiting = waiting' <|> waiting_since' <|> waiting_utc'
      waiting_utc =
        fmap
          (T.pack . timePrint importUtcFormat)
          (parseUtc =<< maybewaiting)

    closed <- o .:? "closed"
    o_closed_utc <- o .:? "closed_utc"
    closed_at <- o .:? "closed_at"
    closed_on <- o .:? "closed_on"
    end <- o .:? "end"
    o_end_utc <- o .:? "end_utc"
    end_on <- o .:? "end_on"
    let
      maybeClosed =
        closed
          <|> o_closed_utc
          <|> closed_at
          <|> closed_on
          <|> end
          <|> o_end_utc
          <|> end_on
          <|> implicitCloseUtcMaybe
      closed_utc =
        fmap
          (T.pack . timePrint importUtcFormat)
          (parseUtc =<< maybeClosed)

    group_ulid <- o .:? "group_ulid"

    let parseIsoDurationMb durTextMb =
          hush $
            fmap (P.decodeUtf8 . Iso.formatDuration) $
              Iso.parseDuration $
                P.encodeUtf8 $
                  fromMaybe "" durTextMb

    repetition_duration' <- o .:? "repetition_duration"
    repeat_duration' <- o .:? "repeat_duration"
    let
      maybeRepetition = repetition_duration' <|> repeat_duration'
      repetition_duration = parseIsoDurationMb maybeRepetition

    recurrence_duration' <- o .:? "recurrence_duration"
    recur_duration' <- o .:? "recur_duration"
    let
      maybeRecurrence = recurrence_duration' <|> recur_duration'
      recurrence_duration = parseIsoDurationMb maybeRecurrence

    o_notes <-
      asum
        [ o .:? "notes" :: Parser (Maybe [Note])
        , do
            notesMb <- o .:? "notes" :: Parser (Maybe [Text])
            pure $ case notesMb of
              Just textNotes -> Just $ textNotes <&> textToNote createdUtc
              Nothing -> Just []
        ]
    annotations <- o .:? "annotations" :: Parser (Maybe [Annotation])

    let
      notes = case (o_notes, annotations) of
        (Nothing, Nothing) -> []
        (Nothing, Just values) -> values <&> annotationToNote
        (Just theNotes, _) -> case parsedCreatedUtc of
          Just crUtc ->
            theNotes
              <&> ( \theNote ->
                      theNote
                        { Note.ulid =
                            theNote.ulid
                              & parseUlidText
                              <&> P.flip setDateTime crUtc
                              <&> show @ULID
                              & fromMaybe theNote.ulid
                              & T.toLower
                        }
                  )
          Nothing -> theNotes

    o_user <- o .:? "user"
    o_author <- o .:? "author"
    let
      userMaybe = o_user <|> o_author
      user = fromMaybe "" userMaybe

    o_metadata <- o .:? "metadata"
    let
      metadata = o_metadata <|> Just (Object o)
      tempTask = Task{ulid = "", ..}

    o_ulid <- o .:? "ulid"
    let
      ulidGeneratedRes = tempTask & (hash >>> toInteger >>> abs >>> ulidFromInteger)
      ulidCombined = (ulidGeneratedRes & P.fromRight emptyUlid) `setDateTime` createdUtc
      ulid =
        T.toLower $
          fromMaybe
            ""
            (o_ulid <|> Just (show ulidCombined))

    -- let showInt = show :: Int -> Text
    -- uuid           <- o .:? "uuid"
    -- -- Map `show` over `Parser` & `Maybe` to convert possible `Int` to `Text`
    -- id             <- (o .:? "id" <|> ((showInt <$>) <$> (o .:? "id")))
    -- let id = (uuid <|> id)

    let finalTask = tempTask{Task.ulid = ulid}

    pure $ ImportTask finalTask notes tags


insertImportTask :: Connection -> ImportTask -> IO (Doc AnsiStyle)
insertImportTask connection importTaskRecord = do
  effectiveUserName <- getEffectiveUserName
  let
    taskParsed = task importTaskRecord
    theTask =
      if taskParsed.user == ""
        then taskParsed{Task.user = T.pack effectiveUserName}
        else taskParsed
  insertRecord "tasks" connection theTask
  tagWarnings <-
    insertTags
      connection
      (ulidTextToDateTime taskParsed.ulid)
      theTask
      importTaskRecord.tags
  noteWarnings <-
    insertNotes
      connection
      (ulidTextToDateTime taskParsed.ulid)
      theTask
      importTaskRecord.notes
  pure $
    tagWarnings
      <$$> noteWarnings
      <$$> "📥 Imported task"
      <+> dquotes (pretty theTask.body)
      <+> "with ulid"
      <+> dquotes (pretty theTask.ulid)
      <+> hardline


importJson :: Config -> Connection -> IO (Doc AnsiStyle)
importJson _ connection = do
  content <- BSL.getContents

  case Aeson.eitherDecode content of
    Left error -> die $ T.pack error <> " in task \n" <> show content
    Right importTaskRecord -> insertImportTask connection importTaskRecord


importEml :: Config -> Connection -> IO (Doc AnsiStyle)
importEml _ connection = do
  content <- BSL.getContents

  case Parsec.parse message "<stdin>" content of
    Left error -> die $ show error
    Right email -> insertImportTask connection $ emailToImportTask email


emailToImportTask :: GenericMessage BSL.ByteString -> ImportTask
emailToImportTask email@(Message headerFields msgBody) =
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


importFile :: Config -> Connection -> FilePath -> IO (Doc AnsiStyle)
importFile _ connection filePath = do
  content <- BSL.readFile filePath

  let
    fileExt = takeExtension filePath

  case fileExt of
    ".json" ->
      let decodeResult = Aeson.eitherDecode content :: Either [Char] ImportTask
      in  case decodeResult of
            Left error ->
              die $ T.pack error <> " in task \n" <> show content
            Right importTaskRecord ->
              insertImportTask connection importTaskRecord
    ".eml" ->
      case Parsec.parse message filePath content of
        Left error -> die $ show error
        Right email -> insertImportTask connection $ emailToImportTask email
    _ -> die $ T.pack $ "File type " <> fileExt <> " is not supported"


ingestFile :: Config -> Connection -> FilePath -> IO (Doc AnsiStyle)
ingestFile _config connection filePath = do
  content <- BSL.readFile filePath

  let
    fileExt = takeExtension filePath

  resultDocs <- case fileExt of
    ".json" ->
      let decodeResult = Aeson.eitherDecode content :: Either [Char] ImportTask
      in  case decodeResult of
            Left error ->
              die $ T.pack error <> " in task \n" <> show content
            Right importTaskRecord@ImportTask{task} ->
              sequence
                [ insertImportTask connection importTaskRecord
                , editTaskByTask NoPreEdit connection task
                ]
    ".eml" ->
      case Parsec.parse message filePath content of
        Left error -> die $ show error
        Right email ->
          let taskRecord@ImportTask{task} =
                emailToImportTask email
          in  sequence
                [ insertImportTask connection taskRecord
                , editTaskByTask NoPreEdit connection task
                ]
    _ -> die $ T.pack $ "File type " <> fileExt <> " is not supported"

  removeFile filePath

  pure $
    P.fold resultDocs
      <+> "❌ Deleted file \""
      <> pretty filePath
      <> "\""


-- TODO: Use Task instead of FullTask to fix broken notes export
dumpCsv :: Config -> IO (Doc AnsiStyle)
dumpCsv conf = do
  execWithConn conf $ \connection -> do
    rows :: [FullTask] <- query_ connection "SELECT * FROM tasks_view"
    pure $ pretty $ TL.decodeUtf8 $ Csv.encodeDefaultOrderedByName rows


dumpNdjson :: Config -> IO (Doc AnsiStyle)
dumpNdjson conf = do
  -- TODO: Use Task instead of FullTask to fix broken notes export
  execWithConn conf $ \connection -> do
    tasks :: [FullTask] <- query_ connection "SELECT * FROM tasks_view"
    pure $
      vsep $
        fmap (pretty . TL.decodeUtf8 . Aeson.encode) tasks


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
      [ dataDir conf </> dbName conf
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
    backupDirPath = dataDir conf </> backupDirName
    backupFilePath = backupDirPath </> timePrint fileUtcFormat now <> ".db"

  -- Create directory (and parents because of True)
  createDirectoryIfMissing True backupDirPath

  result <-
    pretty
      <$> readProcess
        "sqlite3"
        [ dataDir conf </> dbName conf
        , ".backup '" <> backupFilePath <> "'"
        ]
        []

  pure $
    result
      <> hardline
      <> pretty
        ( "✅ Backed up database \""
            <> dbName conf
            <> "\" to \""
            <> backupFilePath
            <> "\""
        )


data PreEdit
  = ApplyPreEdit (P.ByteString -> P.ByteString)
  | NoPreEdit


{-| Edit the task until it is valid YAML and can be decoded.
| Return the the tuple `(task, valid YAML content)`
-}
editUntilValidYaml
  :: PreEdit
  -> Connection
  -> P.ByteString
  -> P.ByteString
  -> IO (Either ParseException (ImportTask, P.ByteString))
editUntilValidYaml preEdit conn initialYaml wipYaml = do
  yamlAfterEdit <- case preEdit of
    ApplyPreEdit editFunc -> pure $ editFunc wipYaml
    NoPreEdit -> runUserEditorDWIM yamlTemplate wipYaml

  if yamlAfterEdit == initialYaml
    then pure $ Left $ InvalidYaml $ Just $ YamlException "⚠️ Nothing changed"
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
          editUntilValidYaml preEdit conn initialYaml yamlAfterEdit
        ---
        Right newTask -> do
          pure $ Right (newTask, yamlAfterEdit)


editTaskByTask :: PreEdit -> Connection -> Task -> IO (Doc AnsiStyle)
editTaskByTask preEdit conn taskToEdit = do
  taskYaml <- taskToEditableYaml conn taskToEdit
  taskYamlTupleRes <- editUntilValidYaml preEdit conn taskYaml taskYaml
  case taskYamlTupleRes of
    Left error -> pure $ pretty $ Yaml.prettyPrintParseException error
    Right (importTaskRecord, newContent) -> do
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
          importTaskRecord.task
            { Task.user =
                if importTaskRecord.task.user == ""
                  then T.pack effectiveUserName
                  else importTaskRecord.task.user
            , Task.metadata =
                if hasMetadata == Just True
                  then importTaskRecord.task.metadata
                  else Nothing
            , -- Set to previous value to force SQL trigger to update it
              Task.modified_utc = taskToEdit.modified_utc
            }
        notesCorrectUtc =
          importTaskRecord.notes
            <&> ( \note ->
                    note
                      { Note.ulid =
                          if zeroUlidTxt `T.isPrefixOf` note.ulid
                            then note.ulid & T.replace zeroUlidTxt now
                            else note.ulid
                      }
                )

      updateTask conn taskFixed

      -- TODO: Remove after it was added to `createSetClosedUtcTrigger`
      -- Update again with the same `state` field to avoid firing
      -- SQL trigger which would overwrite the `closed_utc` field.
      P.when (isJust taskFixed.closed_utc) $ do
        now_ <- dateCurrent
        updateTask conn taskFixed{Task.modified_utc = show @DateTime now_}

      tagWarnings <- insertTags conn Nothing taskFixed importTaskRecord.tags
      noteWarnings <- insertNotes conn Nothing taskFixed notesCorrectUtc
      pure $
        tagWarnings
          <$$> noteWarnings
          <$$> "✏️  Edited task"
          <+> dquotes (pretty taskFixed.body)
          <+> "with ulid"
          <+> dquotes (pretty taskFixed.ulid)
          <+> hardline


editTask :: Config -> Connection -> IdText -> IO (Doc AnsiStyle)
editTask conf conn idSubstr = do
  execWithTask conf conn idSubstr $ \taskToEdit -> do
    editTaskByTask NoPreEdit conn taskToEdit
