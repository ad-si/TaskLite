{-|
Data type to represent tasks from the `tasks_view`
-}
module FullTask where

import Protolude as P (
  Applicative ((<*>)),
  Eq ((==)),
  Float,
  Generic,
  Hashable,
  Maybe,
  Show,
  decodeUtf8,
  encodeUtf8,
  toStrict,
  ($),
  (.),
  (<$>),
  (<&>),
 )

import Data.Aeson as Aeson (Value)
import Data.Aeson.Text as Aeson (encodeToLazyText)
import Data.Csv as Csv (
  DefaultOrdered,
  ToField (..),
  ToNamedRecord,
  ToRecord,
 )
import Data.Text as T (Text, dropEnd, intercalate, split)
import Data.Yaml as Yaml (ToJSON, encode)
import Database.SQLite.Simple as Sql (
  FromRow (..),
  ResultError (ConversionFailed),
  SQLData (SQLText),
  field,
 )
import Database.SQLite.Simple.FromField as Sql.FromField (
  FromField (..),
  returnError,
 )
import Database.SQLite.Simple.Internal (Field (Field))
import Database.SQLite.Simple.Ok (Ok (Ok))
import Note (Note (..))
import Prettyprinter (Pretty (pretty))
import Task (TaskState)


-- | Final user-facing format of tasks
data FullTask = FullTask
  { ulid :: Text -- Ulid
  , body :: Text
  , modified_utc :: Text
  , awake_utc :: Maybe Text
  , ready_utc :: Maybe Text
  , waiting_utc :: Maybe Text
  , review_utc :: Maybe Text
  , due_utc :: Maybe Text
  , closed_utc :: Maybe Text
  , state :: Maybe TaskState
  , group_ulid :: Maybe Text
  , repetition_duration :: Maybe Text
  , recurrence_duration :: Maybe Text
  , tags :: Maybe [Text]
  , notes :: Maybe [Note]
  , priority :: Maybe Float
  , user :: Text
  , metadata :: Maybe Aeson.Value
  }
  deriving (Generic, Show, Eq)


-- For conversion from SQLite with SQLite.Simple
instance FromRow FullTask where
  fromRow =
    FullTask
      <$> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field
      <*> field


instance Sql.FromField.FromField [Text] where
  fromField (Field (SQLText txt) _) = Ok $ split (== ',') txt
  fromField f = returnError ConversionFailed f "expecting SQLText column type"


instance Sql.FromField.FromField [Note] where
  fromField (Field (SQLText txt) _) =
    let notes = split (== ',') txt
    in  Ok $ notes <&> Note ""
  fromField f = returnError ConversionFailed f "expecting SQLText column type"


instance Csv.ToField [Text] where
  toField = encodeUtf8 . T.intercalate ","


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
  pretty =
    pretty
      . T.dropEnd 1 -- Drop trailing newline to maybe add it later
      . decodeUtf8
      . Yaml.encode


selectQuery :: Text
selectQuery =
  "\
  \select\n\
  \  tasks_view.ulid as ulid, body, modified_utc, awake_utc, ready_utc,\n\
  \  waiting_utc, review_utc, due_utc, closed_utc, state,\n\
  \  group_ulid, repetition_duration, recurrence_duration,\n\
  \  tags, notes, priority, user, metadata\n\
  \"
