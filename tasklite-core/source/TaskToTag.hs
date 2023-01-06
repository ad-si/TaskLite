module TaskToTag where

import Protolude as P (
  Generic,
  Identity,
  Show,
  Text,
  decodeUtf8,
  (.),
 )

import Data.Text as T (dropEnd)
import Data.Yaml as Yaml (ToJSON, encode)
import Database.Beam (
  Beamable,
  Columnar,
  Table (..),
 )
import Prettyprinter (Pretty (pretty))
import Task (TaskT)


-- | Record for storing entries of the `task_to_tag` table
data TaskToTagT f = TaskToTag
  { ulid :: Columnar f Text -- Ulid
  , task_ulid :: PrimaryKey TaskT f
  , tag :: Columnar f Text
  }
  deriving (Generic)


type TaskToTag = TaskToTagT Identity
type TaskToTagId = PrimaryKey TaskToTagT Identity


deriving instance Show TaskToTag


-- deriving instance Eq TaskToTag

instance Beamable TaskToTagT


instance Table TaskToTagT where
  data PrimaryKey TaskToTagT f = TaskToTagId (Columnar f Text)
    deriving (Generic)
  primaryKey = TaskToTagId . ulid
instance Beamable (PrimaryKey TaskToTagT)


-- For conversion to JSON
instance ToJSON TaskToTag


instance Pretty TaskToTag where
  pretty =
    pretty
      . T.dropEnd 1 -- Drop trailing newline to maybe add it later
      . decodeUtf8
      . Yaml.encode
