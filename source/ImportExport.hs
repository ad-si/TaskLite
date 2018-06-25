module ImportExport where

import Protolude as P

import Codec.Crockford as Crock
import Data.Aeson as Aeson
import Data.Aeson.Text as Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Text as T
import Data.Hourglass
import Data.ULID
import Data.ULID.TimeStamp
import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Sqlite
import Database.Beam.Schema.Tables
import Database.Beam.Sqlite.Syntax (SqliteExpressionSyntax)
import Database.SQLite.Simple as Sql
import Database.SQLite.Simple.FromField as Sql.FromField
import Database.SQLite.Simple.ToField as Sql.ToField
import Database.SQLite.Simple.Internal hiding (result)
import Database.SQLite.Simple.Ok
import Foreign.C
import Lib
import System.Directory
import System.Process
import Time.System
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Text.Prettyprint.Doc.Render.Terminal
import Unsafe (unsafeHead)
import Utils
import qualified SqlUtils as SqlU
import Task
import FullTask (FullTask)
import Note (Note(..))
import Lib (Config(..), conf)


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
    utc = fromMaybe (timeFromElapsed 0 :: DateTime) (parseUtc entry)
    ulidGenerated = (ulidFromInteger . abs . toInteger . hash) annot
    ulidCombined = setDateTime ulidGenerated utc
  in
    Note { ulid = (T.toLower . show) ulidCombined
         , body = description
         }


parseUtc :: Text -> Maybe DateTime
parseUtc utcText =
  let
    isoFormatSpace = toFormat ("YYYY-MM-DD H:MI:S" :: [Char])
    isoFormat = toFormat ("YYYYMMDDTHMIS" :: [Char])
    utcString = T.unpack utcText
  in
        (timeParse ISO8601_DateAndTime utcString)
    <|> (timeParse isoFormatSpace utcString)
    <|> (timeParse isoFormat utcString)


setDateTime :: ULID -> DateTime -> ULID
setDateTime ulid dateTime = ULID
  (mkULIDTimeStamp $ realToFrac
    (timeFromElapsed $ timeGetElapsed dateTime :: CTime))
  (random ulid)


data ImportTask = ImportTask
  { task :: Task
  , notes :: [Note]
  , tags :: [Text]
  } deriving Show


instance FromJSON ImportTask where
  parseJSON = withObject "task" $ \o -> do
    entry        <- o .:? "entry"
    creation     <- o .:? "creation"
    created_at   <- o .:? "created_at"
    let createdUtc = fromMaybe (timeFromElapsed 0 :: DateTime)
          (parseUtc =<< (entry <|> creation <|> created_at))

    o_body       <- o .:? "body"
    description  <- o .:? "description"
    let body = fromMaybe "" (o_body <|> description)

    o_state      <- o .:? "state"
    status       <- o .:? "status"
    let state = fromMaybe Open (textToTaskState =<< (o_state <|> status))

    o_priority_adjustment <- o .:? "priority_adjustment"
    urgency             <- o .:? "urgency"
    priority          <- optional (o .: "priority")
    let priority_adjustment = o_priority_adjustment <|> urgency <|> priority

    modified          <- o .:? "modified"
    modified_at       <- o .:? "modified_at"
    o_modified_utc    <- o .:? "modified_utc"
    modification_date <- o .:? "modification_date"
    updated_at        <- o .:? "updated_at"
    let
      maybeModified = modified <|> modified_at <|> o_modified_utc
        <|> modification_date <|> updated_at
      modified_utc = T.pack $ timePrint ISO8601_DateAndTime $
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
        (T.pack . (timePrint ISO8601_DateAndTime))
        (parseUtc =<< maybeDue)

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
        (T.pack . (timePrint ISO8601_DateAndTime))
        (parseUtc =<< maybeClosed)

    o_notes     <- optional (o .: "notes") :: Parser (Maybe [Note])
    annotations <- o .:? "annotations" :: Parser (Maybe [Annotation])
    let
      notes = case (o_notes, annotations) of
        (Just theNotes , _   ) -> theNotes
        (Nothing, Just values) -> values <$$> annotationToNote
        _                      -> []

    let
      metadata = Just $ Object o
      ulid = ""
      tempTask = Task {..}

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


importTask :: IO ()
importTask = do
  connection <- setupConnection
  content <- BL.getContents

  let
    importResult = Aeson.eitherDecode content :: Either [Char] ImportTask

  case importResult of
    Left error -> die $ (T.pack error) <> " in task \n" <> show content
    Right importTask -> do
      putStr ("Importing … " :: Text)
      let theTask = task importTask
      insertTags connection (primaryKey theTask) (tags importTask)
      insertNotes connection (primaryKey theTask) (notes importTask)
      insertTask connection theTask


dumpCsv :: IO ()
dumpCsv = do
  -- TODO: Use Task instead of FullTask to fix broken notes export
  execWithConn $ \connection -> do
    rows <- (query_ connection "select * from tasks_view") :: IO [FullTask]

    putStrLn $ Csv.encodeDefaultOrderedByName rows


dumpNdjson :: IO ()
dumpNdjson = do
  -- TODO: Use Task instead of FullTask to fix broken notes export
  execWithConn $ \connection -> do
    rows <- (query_ connection "select * from tasks_view") :: IO [FullTask]

    forM_ rows $ putStrLn . Aeson.encode


dumpSql :: IO ()
dumpSql = do
  homeDir <- getHomeDirectory
  result <- readProcess "sqlite3"
    [ (getMainDir homeDir) <> "/" <> (dbName conf)
    , ".dump"
    ]
    []
  putStrLn result


backupDatabase :: IO ()
backupDatabase = do
  now <- timeCurrent
  homeDir <- getHomeDirectory

  let
    fileUtcFormat = toFormat ("YYYY-MM-DDtHMI" :: [Char])
    backupDirName = "backups"
    backupDirPath = (getMainDir homeDir) <> "/" <> backupDirName
    backupFilePath = backupDirPath <> "/"
      <> (timePrint fileUtcFormat now) <> ".db"

  -- Create directory (and parents because of True)
  createDirectoryIfMissing True backupDirPath

  result <- readProcess "sqlite3"
    [ (getMainDir homeDir) <> "/" <> (dbName conf)
    , ".dump '" <> backupFilePath <> "'"
    ]
    []
  print result

