module TaskToTask where

import Protolude as P (
  Eq,
  Generic,
  Show,
  Text,
  decodeUtf8,
  (.),
  (<$>),
  (<*>),
 )

import Data.Generics (Data)
import Data.Text as T (dropEnd)
import Data.Yaml as Yaml (ToJSON, encode)
import Database.SQLite.Simple (FromRow, ToRow, field, fromRow, toRow)
import Database.SQLite.Simple.ToField (toField)
import Prettyprinter (Pretty (pretty))


-- | Record for storing entries of the `task_to_task` table
data TaskToTask = TaskToTask
  { ulid :: Text -- Ulid
  , source_task_ulid :: Text
  , target_task_ulid :: Text
  , relation :: Text
  }
  deriving (Show, Eq, Generic, Data)


instance FromRow TaskToTask where
  fromRow =
    TaskToTask
      <$> field
      <*> field
      <*> field
      <*> field


instance ToRow TaskToTask where
  toRow TaskToTask{..} =
    [ toField ulid
    , toField source_task_ulid
    , toField target_task_ulid
    , toField relation
    ]


instance ToJSON TaskToTask


instance Pretty TaskToTask where
  pretty =
    pretty
      . T.dropEnd 1 -- Drop trailing newline to maybe add it later
      . decodeUtf8
      . Yaml.encode
