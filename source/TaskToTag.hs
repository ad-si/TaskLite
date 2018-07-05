module TaskToTag where

import Protolude as P

import Database.Beam
import Task (TaskT)


-- | Record for storing entries of the `task_to_tag` table
data TaskToTagT f = TaskToTag
  { _ttUlid :: Columnar f Text -- Ulid
  , _ttTaskUlid :: PrimaryKey TaskT f
  , _ttTag :: Columnar f Text
  } deriving Generic

type TaskToTag = TaskToTagT Identity
type TaskToTagId = PrimaryKey TaskToTagT Identity

deriving instance Show TaskToTag
-- deriving instance Eq TaskToTag

instance Beamable TaskToTagT

instance Table TaskToTagT where
  data PrimaryKey TaskToTagT f = TaskToTagId (Columnar f Text)
    deriving Generic
  primaryKey = TaskToTagId . _ttUlid
instance Beamable (PrimaryKey TaskToTagT)
