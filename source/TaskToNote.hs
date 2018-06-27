module TaskToNote where

import Protolude as P

import Database.Beam
import Task (TaskT)
-- import qualified FullTask as FullTask


-- | Record for storing entries of the `task_to_note` table
data TaskToNoteT f = TaskToNote
  { ulid :: Columnar f Text -- Ulid
  , task_ulid :: PrimaryKey TaskT f
  , note :: Columnar f Text
  } deriving Generic

type TaskToNote = TaskToNoteT Identity
type TaskToNoteId = PrimaryKey TaskToNoteT Identity

-- FIXME: Probably doesn't work because of `PrimaryKey TaskT f`
-- deriving instance Show TaskToNote
-- deriving instance Eq TaskToNote

instance Beamable TaskToNoteT

instance Table TaskToNoteT where
  data PrimaryKey TaskToNoteT f = TaskToNoteId (Columnar f Text)
    deriving Generic
  primaryKey = TaskToNoteId . ulid
instance Beamable (PrimaryKey TaskToNoteT)
