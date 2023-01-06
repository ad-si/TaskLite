module TaskView where

import Protolude as P (
  Float,
  Maybe,
  Show,
  decodeUtf8,
  (.),
  (<$>),
 )

import Data.Aeson as Aeson (ToJSON, Value)
import Data.Text as T (Text, dropEnd)
import Data.Yaml as Yaml (encode)
import Database.Beam (
  Beamable,
  Columnar,
  FromBackendRow (fromBackendRow),
  Generic,
  Identity,
  Table (..),
 )
import Database.Beam.Sqlite.Connection (Sqlite)
import Prettyprinter (Pretty (pretty))

import Note (Note (..))
import Task (
  Task,
  TaskState,
  TaskT (
    awake_utc,
    closed_utc,
    due_utc,
    modified_utc,
    ready_utc,
    review_utc,
    state,
    ulid,
    waiting_utc
  ),
  zeroTask,
 )


{-|
Record for storing entries of the `tasks_view` table
TODO: Use Beam instead of SQLite.Simple
-}
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
  }
  deriving (Generic)


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
  pretty =
    pretty
      . T.dropEnd 1 -- Drop trailing newline to maybe add it later
      . decodeUtf8
      . Yaml.encode


cpTimesAndState :: TaskView -> Task
cpTimesAndState (TaskView{..}) =
  zeroTask
    { Task.ulid
    , Task.modified_utc
    , Task.awake_utc
    , Task.ready_utc
    , Task.waiting_utc
    , Task.review_utc
    , Task.due_utc
    , Task.closed_utc
    , Task.state
    }
