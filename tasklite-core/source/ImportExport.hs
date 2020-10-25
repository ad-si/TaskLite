{-|
Functions to import and export tasks
-}

module ImportExport where

import Protolude as P hiding (state)

import Data.Aeson as Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as Csv
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Lazy as HML
import Data.Hourglass
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time.ISO8601.Duration as Iso
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.ULID
import qualified Data.Vector as V
import Data.Yaml as Yaml
import Database.Beam
import Database.SQLite.Simple as Sql
import Lib
import System.Directory
import System.FilePath ((</>), takeExtension)
import System.Process
import System.Posix.User (getEffectiveUserName)
import System.ReadEditor (readEditorWith)
import Text.ParserCombinators.Parsec as Parsec (parse)
import qualified Text.Parsec.Rfc2822 as Email
import Text.Parsec.Rfc2822 (GenericMessage(..), message)
import Text.PortableLines.ByteString.Lazy (lines8)
import Time.System
import Utils
import Task
import FullTask (FullTask)
import Note (Note(..))
import Config


data Annotation = Annotation
  { entry :: Text
  , description :: Text
  } deriving Generic

instance Hashable Annotation

instance ToJSON Annotation

instance FromJSON Annotation where
  parseJSON = withObject "annotation" $ \o -> do
    entry        <- o .: "entry"
    description  <- o .: "description"
    pure Annotation{..}


annotationToNote :: Annotation -> Note
annotationToNote annot@Annotation {entry = entry, description = description} =
  let
    utc = fromMaybe (timeFromElapsedP 0 :: DateTime) (parseUtc entry)
    Right ulidGenerated = (ulidFromInteger . abs . toInteger . hash) annot
    ulidCombined = setDateTime ulidGenerated utc
  in
    Note { ulid = (T.toLower . show) ulidCombined
         , body = description
         }


textToNote :: DateTime -> Text -> Note
textToNote utc body =
  let
    Right ulidGenerated = (ulidFromInteger . abs . toInteger . hash) body
    ulidCombined = setDateTime ulidGenerated utc
  in
    Note { ulid = (T.toLower . show) ulidCombined
         , body = body
         }


importUtcFormat :: TimeFormatString
importUtcFormat = (toFormat ("YYYY-MM-DD H:MI:S" :: [Char]))


data ImportTask = ImportTask
  { task :: Task
  , notes :: [Note]
  , tags :: [Text]
  } deriving Show


emptyImportTask :: ImportTask
emptyImportTask = ImportTask
  { task = zeroTask
  , notes = []
  , tags = []
  }


-- | Values a suffixed with a prime (') to avoid name collisions
instance FromJSON ImportTask where
  parseJSON = withObject "task" $ \o -> do
    utc          <- o .:? "utc" <|> (o .:? "utc" :: Parser (Maybe Int))
    entry        <- o .:? "entry"
    creation     <- o .:? "creation"
    created_at   <- o .:? "created_at"

    let
      zeroTime = timeFromElapsedP 0 :: DateTime
      parsedCreatedUtc = parseUtc
        =<< ((fmap show utc) <|> entry <|> creation <|> created_at)
      createdUtc = fromMaybe zeroTime parsedCreatedUtc

    o_body       <- o .:? "body"
    description  <- o .:? "description"
    let body = fromMaybe "" (o_body <|> description)

    o_priority_adjustment <- o .:? "priority_adjustment"
    urgency               <- o .:? "urgency"
    priority              <- optional (o .: "priority")
    let priority_adjustment = o_priority_adjustment <|> urgency <|> priority

    modified          <- o .:? "modified"
    modified_at       <- o .:? "modified_at"
    o_modified_utc    <- o .:? "modified_utc"
    modification_date <- o .:? "modification_date"
    updated_at        <- o .:? "updated_at"
    let
      maybeModified = modified <|> modified_at <|> o_modified_utc
        <|> modification_date <|> updated_at
      modified_utc = T.pack $ timePrint importUtcFormat $
        fromMaybe createdUtc (parseUtc =<< maybeModified)

    o_state      <- o .:? "state"
    status       <- o .:? "status"
    let
      state = textToTaskState =<< (o_state <|> status)
      implicitCloseUtcMaybe =
        if isJust state
        then (maybeModified
          <|> (Just $ T.pack $ timePrint importUtcFormat $ createdUtc))
        else Nothing

    o_tags  <- o .:? "tags"
    o_labels  <- o .:? "labels"
    project <- o .:? "project"
    let
      projects = fmap (:[]) project
      tags = fromMaybe [] (o_tags <> o_labels <> projects)

    due       <- o .:? "due"
    o_due_utc <- o .:? "due_utc"
    due_on    <- o .:? "due_on"
    let
      maybeDue = due <|> o_due_utc <|> due_on
      due_utc = fmap
        (T.pack . (timePrint importUtcFormat))
        (parseUtc =<< maybeDue)

    awake'       <- o .:? "awake"
    awake_at'    <- o .:? "awake_at"
    sleep'       <- o .:? "sleep"
    sleep_utc'   <- o .:? "sleep_utc"
    sleep_until' <- o .:? "sleep_until"
    wait'        <- o .:? "wait"
    wait_until'  <- o .:? "wait_until"
    let
      maybeAwake = awake' <|> awake_at'
        <|> sleep' <|> sleep_utc' <|> sleep_until'
        <|> wait' <|> wait_until'
      awake_utc = fmap
        (T.pack . (timePrint importUtcFormat))
        (parseUtc =<< maybeAwake)

    ready'       <- o .:? "ready"
    ready_since' <- o .:? "ready_since"
    ready_utc'   <- o .:? "ready_utc"
    let
      maybeReady = ready' <|> ready_since' <|> ready_utc'
      ready_utc = fmap
        (T.pack . (timePrint importUtcFormat))
        (parseUtc =<< maybeReady)

    review'       <- o .:? "review"
    review_at'    <- o .:? "review_at"
    review_since' <- o .:? "review_since"
    review_utc'   <- o .:? "review_utc"
    let
      maybeReview = review' <|> review_at' <|> review_since' <|> review_utc'
      review_utc = fmap
        (T.pack . (timePrint importUtcFormat))
        (parseUtc =<< maybeReview)

    waiting'       <- o .:? "waiting"
    waiting_since' <- o .:? "waiting_since"
    waiting_utc'   <- o .:? "waiting_utc"
    let
      maybewaiting = waiting' <|> waiting_since' <|> waiting_utc'
      waiting_utc = fmap
        (T.pack . (timePrint importUtcFormat))
        (parseUtc =<< maybewaiting)

    closed       <- o .:? "closed"
    o_closed_utc <- o .:? "closed_utc"
    closed_at    <- o .:? "closed_at"
    closed_on    <- o .:? "closed_on"
    end          <- o .:? "end"
    o_end_utc    <- o .:? "end_utc"
    end_on       <- o .:? "end_on"
    let
      maybeClosed = closed <|> o_closed_utc <|> closed_at <|> closed_on
        <|> end <|> o_end_utc <|> end_on <|> implicitCloseUtcMaybe
      closed_utc = fmap
        (T.pack . (timePrint importUtcFormat))
        (parseUtc =<< maybeClosed)

    group_ulid <- o .:? "group_ulid"

    let parseIsoDurationMb durTextMb = hush
          $ fmap (P.decodeUtf8 . Iso.formatDuration)
          $ Iso.parseDuration
          $ P.encodeUtf8
          $ fromMaybe "" durTextMb

    repetition_duration' <- o .:? "repetition_duration"
    repeat_duration'     <- o .:? "repeat_duration"
    let
      maybeRepetition = repetition_duration' <|> repeat_duration'
      repetition_duration = parseIsoDurationMb maybeRepetition

    recurrence_duration' <- o .:? "recurrence_duration"
    recur_duration'      <- o .:? "recur_duration"
    let
      maybeRecurrence = recurrence_duration' <|> recur_duration'
      recurrence_duration = parseIsoDurationMb maybeRecurrence

    o_notes <- asum
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
        (Nothing, Nothing)     -> []
        (Nothing, Just values) -> values <&> annotationToNote
        (Just theNotes , _)    -> case parsedCreatedUtc of
          Just crUtc -> theNotes <&> (\theNote ->
              let
                noteUlidTxt = Note.ulid theNote
                mbNoteUlid = parseUlidText noteUlidTxt
                mbNewUlid = do
                  noteUlid <- mbNoteUlid

                  pure $ show $ setDateTime noteUlid crUtc
              in
                theNote { Note.ulid =
                  (T.toLower $ fromMaybe noteUlidTxt mbNewUlid) }
            )
          Nothing       -> theNotes

    o_user      <- o .:? "user"
    o_author      <- o .:? "author"
    let
      userMaybe = o_user <|> o_author
      user = fromMaybe "" userMaybe

    o_metadata  <- o .:? "metadata"
    let
      metadata = o_metadata <|> (Just $ Object o)
      tempTask = Task {ulid = "", ..}

    o_ulid  <- o .:? "ulid"
    let
      Right ulidGenerated = (ulidFromInteger . abs . toInteger . hash) tempTask
      ulidCombined = setDateTime ulidGenerated createdUtc
      ulid = T.toLower $ fromMaybe ""
        (o_ulid <|> Just (show ulidCombined))

    -- let showInt = show :: Int -> Text
    -- uuid           <- o .:? "uuid"
    -- -- Map `show` over `Parser` & `Maybe` to convert possible `Int` to `Text`
    -- id             <- (o .:? "id" <|> ((showInt <$>) <$> (o .:? "id")))
    -- let id = (uuid <|> id)

    let finalTask = tempTask {Task.ulid = ulid}

    pure $ ImportTask finalTask notes tags


insertImportTask :: Connection -> ImportTask -> IO (Doc ann)
insertImportTask connection importTaskRecord = do
  effectiveUserName <- getEffectiveUserName
  let
    taskParsed = task importTaskRecord
    theTask = if Task.user taskParsed == ""
      then taskParsed { Task.user = T.pack effectiveUserName }
      else taskParsed
  insertTask connection theTask
  insertTags connection (ulidTextToDateTime $ Task.ulid taskParsed)
    (primaryKey theTask) (tags importTaskRecord)
  insertNotes connection (ulidTextToDateTime $ Task.ulid taskParsed)
    (primaryKey theTask) (notes importTaskRecord)
  pure $
    "📥 Imported task" <+> dquotes (pretty $ Task.body theTask)
    <+> "with ulid" <+> dquotes (pretty $ Task.ulid theTask)
    <+> hardline


importJson :: Config -> Connection -> IO (Doc AnsiStyle)
importJson _ connection = do
  content <- BSL.getContents

  case Aeson.eitherDecode content of
    Left error -> die $ (T.pack error) <> " in task \n" <> show content
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
    addBody (ImportTask task notes tags) = ImportTask
      task {Task.body = Task.body task <> (msgBody
        & lines8
        <&> TL.decodeUtf8
        <&> toStrict
        & T.unlines
        & T.dropEnd 1
      )}
      notes
      tags

    namesToJson names = Array $ V.fromList $ names
      <&> (\(Email.NameAddr name emailAddress) -> Object $ HML.fromList $
              [ ("name", Aeson.String $ T.pack $ fromMaybe "" name)
              , ("email", Aeson.String $ T.pack emailAddress)
              ])

    addHeaderToTask :: ImportTask -> Email.Field -> ImportTask
    addHeaderToTask impTask@(ImportTask task notes tags) headerValue =
      case headerValue of
        Email.Date emailDate ->
          let
            utc = zonedTimeToDateTime emailDate
            Right ulidGenerated =
              (ulidFromInteger . abs . toInteger . hash) $ (show email :: Text)
            ulidCombined = setDateTime ulidGenerated utc
          in
            ImportTask
              task { Task.ulid = T.toLower $ show ulidCombined
                    , Task.modified_utc =
                        T.pack $ timePrint (toFormat importUtcFormat) utc
                    }
              notes
              tags

        Email.From names -> ImportTask
          (setMetadataField "from" (namesToJson names) task)
          notes
          tags

        Email.To names -> ImportTask
          (setMetadataField "to" (namesToJson names) task)
          notes
          tags

        Email.MessageID msgId -> ImportTask
          (setMetadataField "messageId" (Aeson.String $ T.pack msgId) task)
          notes
          tags

        Email.Subject subj -> ImportTask
          task {Task.body = Task.body task <> T.pack subj}
          notes
          tags

        Email.Keywords kwords -> ImportTask task notes
          (tags <> fmap (T.unwords . fmap T.pack) kwords)

        Email.Comments cmnts -> ImportTask
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
      in case decodeResult of
            Left error ->
              die $ (T.pack error) <> " in task \n" <> show content
            Right importTaskRecord ->
              insertImportTask connection importTaskRecord

    ".eml" ->
      case Parsec.parse message filePath content of
        Left error -> die $ show error
        Right email -> insertImportTask connection $ emailToImportTask email

    _ -> die $ T.pack $ "File type " <> fileExt <> " is not supported"


ingestFile :: Config -> Connection -> FilePath -> IO (Doc AnsiStyle)
ingestFile config connection filePath = do
  content <- BSL.readFile filePath

  let
    fileExt = takeExtension filePath

  resultDocs <- case fileExt of
    ".json" ->
      let decodeResult = Aeson.eitherDecode content :: Either [Char] ImportTask
      in case decodeResult of
            Left error ->
              die $ (T.pack error) <> " in task \n" <> show content
            Right importTaskRecord@ImportTask { task } ->
              sequence
                [ insertImportTask connection importTaskRecord
                , editTaskByTask config connection task
                ]

    ".eml" ->
      case Parsec.parse message filePath content of
        Left error -> die $ show error
        Right email ->
          let taskRecord@ImportTask { task } =
                emailToImportTask email
          in
            sequence
              [ insertImportTask connection taskRecord
              , editTaskByTask config connection task
              ]

    _ -> die $ T.pack $ "File type " <> fileExt <> " is not supported"

  removeFile filePath

  pure $ (P.fold resultDocs)
    <+> "❌ Deleted file \"" <> (pretty filePath) <> "\""


-- TODO: Use Task instead of FullTask to fix broken notes export
dumpCsv :: Config -> IO (Doc AnsiStyle)
dumpCsv conf = do
  execWithConn conf $ \connection -> do
    rows <- (query_ connection "select * from tasks_view") :: IO [FullTask]
    pure $ pretty $ TL.decodeUtf8 $ Csv.encodeDefaultOrderedByName rows


dumpNdjson :: Config -> IO (Doc AnsiStyle)
dumpNdjson conf = do
  -- TODO: Use Task instead of FullTask to fix broken notes export
  execWithConn conf $ \connection -> do
    tasks <- (query_ connection "select * from tasks_view") :: IO [FullTask]
    pure $ vsep $
      fmap (pretty . TL.decodeUtf8 . Aeson.encode) tasks


dumpSql :: Config -> IO (Doc AnsiStyle)
dumpSql conf = do
  result <- readProcess "sqlite3"
    [ (dataDir conf) </> (dbName conf)
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
    backupDirPath = (dataDir conf) </> backupDirName
    backupFilePath = backupDirPath </> (timePrint fileUtcFormat now) <> ".db"

  -- Create directory (and parents because of True)
  createDirectoryIfMissing True backupDirPath

  result <- pretty <$> readProcess "sqlite3"
    [ (dataDir conf) </> (dbName conf)
    , ".backup '" <> backupFilePath <> "'"
    ]
    []

  pure $ result
    <> hardline
    <> pretty (
          "✅ Backed up database \"" <> (dbName conf)
          <> "\" to \"" <> backupFilePath <> "\"")


editTaskByTask :: Config -> Connection -> Task -> IO (Doc AnsiStyle)
editTaskByTask _ connection taskToEdit = do
  let taskYaml = (T.unpack . decodeUtf8 . Yaml.encode) taskToEdit

  newContent <- readEditorWith taskYaml

  if newContent == taskYaml
  then
    pure $
      "⚠️  Nothing changed" <+> hardline
  else do
    let
      newContentBS = encodeUtf8 $ T.pack newContent

      parseMetadata :: Value -> Parser Bool
      parseMetadata val = case val of
          Object obj -> do
            let mdataMaybe = HM.lookup "metadata" obj

            hasMdata <- pure $ case mdataMaybe of
              Just (Object _) -> True
              _ -> False

            pure hasMdata
          _ -> pure False

      hasMetadata = parseMaybe parseMetadata
        =<< (rightToMaybe $ Yaml.decodeEither' newContentBS :: Maybe Value)

      decodeResult :: Either ParseException ImportTask
      decodeResult = Yaml.decodeEither' newContentBS

    case decodeResult of
      Left error -> die $ (show error) <> " in task \n" <> show newContent
      Right importTaskRecord -> do
        effectiveUserName <- getEffectiveUserName
        let
          taskParsed = task importTaskRecord
          taskFixed = taskParsed
            { Task.user =
                if Task.user taskParsed == ""
                then T.pack effectiveUserName
                else Task.user taskParsed
            , Task.metadata =
                if hasMetadata == Just True
                then Task.metadata taskParsed
                else Nothing
            }

        replaceTask connection taskFixed
        insertTags connection Nothing
          (primaryKey taskFixed) (tags importTaskRecord)
        insertNotes connection Nothing
          (primaryKey taskFixed) (notes importTaskRecord)
        pure $
          "✏️  Edited task" <+> dquotes (pretty $ Task.body taskFixed)
          <+> "with ulid" <+> dquotes (pretty $ Task.ulid taskFixed)
          <+> hardline


editTask :: Config -> Connection -> IdText -> IO (Doc AnsiStyle)
editTask conf connection idSubstr = do
  execWithTask conf connection idSubstr $ \taskToEdit -> do
    editTaskByTask conf connection taskToEdit
