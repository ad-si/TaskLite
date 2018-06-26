module FullTask where

import Protolude as P

import Data.Aeson as Aeson
import Data.Aeson.Text as Aeson
import Data.Yaml as Yaml
import qualified Data.ByteString as BS
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
import Task as Task
import Note (Note(..))


-- | Final user-facing format of tasks
data FullTask = FullTask
  { ulid :: Text -- Ulid
  , body :: Text
  , state :: TaskState
  , due_utc :: Maybe Text
  , closed_utc :: Maybe Text
  , modified_utc :: Text
  , tags :: Maybe [Text]
  , notes :: Maybe [Note]
  , priority :: Maybe Float
  , metadata :: Maybe Aeson.Value
  } deriving (Generic, Show)


-- For conversion from SQLite with SQLite.Simple
instance FromRow FullTask where
  fromRow = FullTask
    <$> field <*> field <*> field
    <*> field <*> field <*> field
    <*> field <*> field <*> field
    <*> field

instance Sql.FromField.FromField [Text] where
  fromField (Field (SQLText txt) _) = Ok $ split (== ',') txt
  fromField f = returnError ConversionFailed f "expecting SQLText column type"

instance Sql.FromField.FromField [Note] where
  fromField (Field (SQLText txt) _) =
    let notes = split (== ',') txt
    in Ok $ notes <$$> (\noteTxt -> Note "" noteTxt)
  fromField f = returnError ConversionFailed f "expecting SQLText column type"


instance Csv.ToField [Text] where
  toField = encodeUtf8 . (T.intercalate ",")

instance Csv.ToField Value where
  toField = encodeUtf8 . toStrict . Aeson.encodeToLazyText

-- For conversion to CSV
instance ToRecord FullTask
instance ToNamedRecord FullTask
instance DefaultOrdered FullTask

-- For conversion to JSON
instance ToJSON FullTask

instance Hashable FullTask

instance Pretty FullTask where
  pretty = pretty
    . decodeUtf8
    . Yaml.encode
