module TaskToComment where

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


-- | Record for storing entries of the `task_to_comment` table
data TaskToComment = TaskToComment
  { ulid :: Text -- Ulid
  , task_ulid :: Text
  , comment :: Text
  , user :: Text -- Author of the comment
  }
  deriving (Show, Eq, Generic, Data)


instance FromRow TaskToComment where
  fromRow =
    TaskToComment
      <$> field
      <*> field
      <*> field
      <*> field


instance ToRow TaskToComment where
  toRow TaskToComment{..} =
    [ toField ulid
    , toField task_ulid
    , toField comment
    , toField user
    ]


instance ToJSON TaskToComment


instance Pretty TaskToComment where
  pretty =
    pretty
      . T.dropEnd 1 -- Drop trailing newline to maybe add it later
      . decodeUtf8
      . Yaml.encode
