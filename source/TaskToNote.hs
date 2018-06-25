module TaskToNote where

import Protolude as P

import Data.Aeson as Aeson
import Data.Hourglass
import Codec.Crockford as Crock
import Data.Csv as Csv
import Data.Text as T
import qualified Data.Text.IO as T
import Data.ULID
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
import System.Directory
import Time.System
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Text.Prettyprint.Doc.Render.Terminal
import Unsafe (unsafeHead)
import Utils
import qualified SqlUtils as SqlU
import Task (TaskT)
-- import qualified FullTask as FullTask


-- | Record for storing entries of the `task_to_tag` table
data TaskToNoteT f = TaskToNote
  { ulid :: Columnar f Text -- Ulid
  , taskUlid :: PrimaryKey TaskT f
  , tag :: Columnar f Text
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
