module TaskToNote where

import Protolude as P

import Data.Yaml as Yaml
import Database.Beam
import Task (TaskT)
import Data.Text as T
import Data.Text.Prettyprint.Doc hiding ((<>))


-- | Record for storing entries of the `task_to_note` table
data TaskToNoteT f = TaskToNote
  { ulid :: Columnar f Text -- Ulid
  , task_ulid :: PrimaryKey TaskT f
  , note :: Columnar f Text
  } deriving Generic

type TaskToNote = TaskToNoteT Identity
type TaskToNoteId = PrimaryKey TaskToNoteT Identity

deriving instance Show TaskToNote
-- deriving instance Eq TaskToNote

instance Beamable TaskToNoteT

instance Table TaskToNoteT where
  data PrimaryKey TaskToNoteT f = TaskToNoteId (Columnar f Text)
    deriving Generic
  primaryKey = TaskToNoteId . ulid
instance Beamable (PrimaryKey TaskToNoteT)

-- For conversion to JSON
instance ToJSON TaskToNote

instance Pretty TaskToNote where
  pretty = pretty
    . T.dropEnd 1 -- Drop trailing newline to maybe add it later
    . decodeUtf8
    . Yaml.encode
