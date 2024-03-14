module TaskToNote where

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
import Database.SQLite.Simple (FromRow, field, fromRow, toRow)
import Database.SQLite.Simple.ToField (toField)
import Database.SQLite.Simple.ToRow (ToRow)
import Prettyprinter (Pretty (pretty))


-- | Record for storing entries of the `task_to_note` table
data TaskToNote = TaskToNote
  { ulid :: Text -- Ulid
  , task_ulid :: Text
  , note :: Text
  }
  deriving (Show, Eq, Generic, Data)


instance FromRow TaskToNote where
  fromRow =
    TaskToNote
      <$> field
      <*> field
      <*> field


instance ToRow TaskToNote where
  toRow TaskToNote{..} =
    [ toField ulid
    , toField task_ulid
    , toField note
    ]


instance ToJSON TaskToNote


instance Pretty TaskToNote where
  pretty =
    pretty
      . T.dropEnd 1 -- Drop trailing newline to maybe add it later
      . decodeUtf8
      . Yaml.encode
