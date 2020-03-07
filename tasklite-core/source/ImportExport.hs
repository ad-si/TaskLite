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
import Data.Hourglass
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.ULID
import Data.Yaml as Yaml
import Database.Beam
import Database.SQLite.Simple as Sql
import Lib
import System.Directory
import System.FilePath ((</>))
import System.Process
import System.Posix.User (getEffectiveUserName)
import System.ReadEditor (readEditorWith)
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
    ulidGenerated = (ulidFromInteger . abs . toInteger . hash) annot
    ulidCombined = setDateTime ulidGenerated utc
  in
    Note { ulid = (T.toLower . show) ulidCombined
         , body = description
         }


importUtcFormat :: TimeFormatString
importUtcFormat = (toFormat ("YYYY-MM-DD H:MI:S" :: [Char]))


data ImportTask = ImportTask
  { task :: Task
  , notes :: [Note]
  , tags :: [Text]
  } deriving Show


-- | Values a suffixed with a prime (') to avoid name collisions
instance FromJSON ImportTask where
  parseJSON = withObject "task" $ \o -> do
    utc          <- o .:? "utc"
    entry        <- o .:? "entry"
    creation     <- o .:? "creation"
    created_at   <- o .:? "created_at"
    let
      zeroTime = timeFromElapsedP 0 :: DateTime
      parsedCreatedUtc = parseUtc
        =<< (utc <|> entry <|> creation <|> created_at)
      createdUtc = fromMaybe zeroTime parsedCreatedUtc

    o_body       <- o .:? "body"
    description  <- o .:? "description"
    let body = fromMaybe "" (o_body <|> description)

    o_state      <- o .:? "state"
    status       <- o .:? "status"
    let state = textToTaskState =<< (o_state <|> status)

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

    o_tags  <- o .:? "tags"
    project <- o .:? "project"
    let
      projects = fmap (:[]) project
      tags = fromMaybe [] (o_tags  <> projects)

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
    closed_on    <- o .:? "closed_on"
    end          <- o .:? "end"
    o_end_utc    <- o .:? "end_utc"
    end_on       <- o .:? "end_on"
    let
      maybeClosed = closed <|> o_closed_utc <|> closed_on
        <|> end <|> o_end_utc <|> end_on
      closed_utc = fmap
        (T.pack . (timePrint importUtcFormat))
        (parseUtc =<< maybeClosed)

    group_ulid' <- o .:? "group_ulid"
    group_id'   <- o .:? "group_id"
    let
      maybeGroupUlid = group_ulid' <|> group_id'
      group_ulid = fmap
        (T.pack . (timePrint importUtcFormat))
        (parseUtc =<< maybeGroupUlid)

    repetition_duration' <- o .:? "repetition_duration"
    repeat_duration'     <- o .:? "repeat_duration"
    let
      maybeRepetition = repetition_duration' <|> repeat_duration'
      repetition_duration = fmap
        (T.pack . (timePrint importUtcFormat))
        (parseUtc =<< maybeRepetition)

    recurrence_duration' <- o .:? "recurrence_duration"
    recur_duration'      <- o .:? "recur_duration"
    let
      maybeRecurrence = recurrence_duration' <|> recur_duration'
      recurrence_duration = fmap
        (T.pack . (timePrint importUtcFormat))
        (parseUtc =<< maybeRecurrence)

    o_notes     <- o .:? "notes" :: Parser (Maybe [Note])
    annotations <- o .:? "annotations" :: Parser (Maybe [Annotation])
    let
      notes = case (o_notes, annotations) of
        (Nothing, Nothing)     -> []
        (Nothing, Just values) -> values <$$> annotationToNote
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
    let user = fromMaybe "" o_user

    o_metadata  <- o .:? "metadata"
    let
      metadata = o_metadata <|> (Just $ Object o)
      tempTask = Task {ulid = "", ..}

    o_ulid  <- o .:? "ulid"
    let
      ulidGenerated = (ulidFromInteger . abs . toInteger . hash) tempTask
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


importTask :: Config -> IO (Doc AnsiStyle)
importTask conf = do
  connection <- setupConnection conf
  content <- BSL.getContents

  let
    decodeResult = Aeson.eitherDecode content :: Either [Char] ImportTask

  case decodeResult of
    Left error -> die $ (T.pack error) <> " in task \n" <> show content
    Right importTaskRecord -> insertImportTask connection importTaskRecord


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


editTask :: Config -> Connection -> IdText -> IO (Doc AnsiStyle)
editTask conf connection idSubstr = do
  execWithTask conf connection idSubstr $ \taskToEdit -> do
    let taskYaml = (T.unpack . decodeUtf8 . Yaml.encode) taskToEdit

    newContent <- readEditorWith taskYaml

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
