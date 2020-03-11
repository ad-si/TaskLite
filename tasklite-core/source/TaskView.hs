module TaskView where

import Protolude as P

import Data.Aeson as Aeson
import Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Yaml as Yaml
import Database.Beam
import Database.Beam.Sqlite.Connection

import Note (Note(..))
import Task

-- | Record for storing entries of the `tasks_view` table
-- TODO: Use Beam instead of SQLite.Simple
data TaskViewT f = TaskView
  { ulid :: Columnar f Text -- TODO: Ulid
  , body :: Columnar f Text
  , modified_utc :: Columnar f Text
  , awake_utc :: Columnar f (Maybe Text)
  , ready_utc :: Columnar f (Maybe Text)
  , waiting_utc :: Columnar f (Maybe Text)
  , review_utc :: Columnar f (Maybe Text)
  , due_utc :: Columnar f (Maybe Text)
  , closed_utc :: Columnar f (Maybe Text)
  , state :: Columnar f (Maybe TaskState)
  , group_ulid :: Columnar f (Maybe Text)
  , repetition_duration :: Columnar f (Maybe Text)
  , recurrence_duration :: Columnar f (Maybe Text)
  , tags :: Columnar f (Maybe [Text])
  , notes :: Columnar f (Maybe [Note])
  , priority :: Columnar f (Maybe Float)
  , user :: Columnar f Text
  , metadata :: Columnar f (Maybe Aeson.Value)
  } deriving Generic

type TaskView = TaskViewT Identity
type TaskViewUlid = PrimaryKey TaskViewT Identity

deriving instance Show TaskViewUlid
deriving instance Show TaskView
-- deriving instance Eq TaskView

-- | Not used as tags are loaded separately
instance FromBackendRow Sqlite [Text] where
  fromBackendRow = (\(_ :: Text) -> ([] :: [Text])) <$> fromBackendRow

-- | Not used as notes are loaded separately
instance FromBackendRow Sqlite [Note] where
  fromBackendRow = (\(_ :: Text) -> ([] :: [Note])) <$> fromBackendRow

instance Beamable TaskViewT

instance Table TaskViewT where
  data PrimaryKey TaskViewT f = TaskViewUlid (Columnar f Text)
    deriving (Generic, Beamable)
  primaryKey = TaskViewUlid . TaskView.ulid

-- For conversion to JSON
instance ToJSON TaskView
instance ToJSON (PrimaryKey TaskViewT Identity)

instance Pretty TaskView where
  pretty = pretty
    . T.dropEnd 1 -- Drop trailing newline to maybe add it later
    . decodeUtf8
    . Yaml.encode
