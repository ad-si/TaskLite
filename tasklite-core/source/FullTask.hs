{-|
Data type to represent tasks from the `tasks_view`
-}

module FullTask where

import Protolude as P

import Data.Aeson as Aeson
import Data.Aeson.Text as Aeson
import Data.Yaml as Yaml
import Data.Csv as Csv
import Data.Text as T
import Database.SQLite.Simple as Sql
import Database.SQLite.Simple.FromField as Sql.FromField
import Database.SQLite.Simple.Internal hiding (result)
import Database.SQLite.Simple.Ok
import Prettyprinter hiding ((<>))
import Task
import Note (Note(..))


-- | Final user-facing format of tasks
data FullTask = FullTask
  { ulid :: Text -- Ulid
  , body :: Text
  , modified_utc :: Text
  , awake_utc :: Maybe Text
  , ready_utc :: Maybe Text
  , waiting_utc :: Maybe Text
  , review_utc :: Maybe Text
  , due_utc :: Maybe Text
  , closed_utc :: Maybe Text
  , state :: Maybe TaskState
  , group_ulid :: Maybe Text
  , repetition_duration :: Maybe Text
  , recurrence_duration :: Maybe Text
  , tags :: Maybe [Text]
  , notes :: Maybe [Note]
  , priority :: Maybe Float
  , user :: Text
  , metadata :: Maybe Aeson.Value
  } deriving (Generic, Show)


-- For conversion from SQLite with SQLite.Simple
instance FromRow FullTask where
  fromRow = FullTask
    <$> field <*> field <*> field
    <*> field <*> field <*> field
    <*> field <*> field <*> field
    <*> field <*> field <*> field
    <*> field <*> field <*> field
    <*> field <*> field <*> field

instance Sql.FromField.FromField [Text] where
  fromField (Field (SQLText txt) _) = Ok $ split (== ',') txt
  fromField f = returnError ConversionFailed f "expecting SQLText column type"

instance Sql.FromField.FromField [Note] where
  fromField (Field (SQLText txt) _) =
    let notes = split (== ',') txt
    in Ok $ notes <&> (Note "")
  fromField f = returnError ConversionFailed f "expecting SQLText column type"


instance Csv.ToField [Text] where
  toField = encodeUtf8 . (T.intercalate ",")

instance Csv.ToField Value where
  toField = encodeUtf8 . toStrict . Aeson.encodeToLazyText

-- For conversion to CSV
instance ToRecord FullTask
instance ToNamedRecord FullTask
instance DefaultOrdered FullTask

-- For conversion to JSON
instance ToJSON FullTask

instance Hashable FullTask

instance Pretty FullTask where
  pretty = pretty
    . T.dropEnd 1 -- Drop trailing newline to maybe add it later
    . decodeUtf8
    . Yaml.encode


selectQuery :: Text
selectQuery = "\
  \select\n\
  \  tasks_view.ulid as ulid, body, modified_utc, awake_utc, ready_utc,\n\
  \  waiting_utc, review_utc, due_utc, closed_utc, state,\n\
  \  group_ulid, repetition_duration, recurrence_duration,\n\
  \  tags, notes, priority, user, metadata\n\
  \"
