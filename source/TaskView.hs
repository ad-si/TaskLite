module TaskView where

import Protolude as P

import Database.Beam

import Task
import TaskToNote
import TaskToTag

-- | Record for storing entries of the `tasks_view` table
-- TODO: Use Beam instead of SQLite.Simple
data TaskView f = TaskView
  { task :: TaskT f
  , tag :: TaskToTagT f
  , note :: TaskToNoteT f
  } deriving Generic
instance Beamable TaskView
