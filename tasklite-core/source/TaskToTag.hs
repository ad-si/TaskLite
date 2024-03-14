module TaskToTag where

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


-- | Record for storing entries of the `task_to_tag` table
data TaskToTag = TaskToTag
  { ulid :: Text -- Ulid
  , task_ulid :: Text
  , tag :: Text
  }
  deriving (Show, Eq, Generic, Data)


instance FromRow TaskToTag where
  fromRow =
    TaskToTag
      <$> field
      <*> field
      <*> field


instance ToRow TaskToTag where
  toRow TaskToTag{..} =
    [ toField ulid
    , toField task_ulid
    , toField tag
    ]


instance ToJSON TaskToTag


instance Pretty TaskToTag where
  pretty =
    pretty
      . T.dropEnd 1 -- Drop trailing newline to maybe add it later
      . decodeUtf8
      . Yaml.encode
