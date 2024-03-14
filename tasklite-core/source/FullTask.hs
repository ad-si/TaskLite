{-# LANGUAGE QuasiQuotes #-}

{-|
Data type to represent tasks from the `tasks_view`
-}
module FullTask where

import Protolude (
  Applicative ((<*>)),
  Eq ((==)),
  Float,
  Generic,
  Hashable,
  Maybe (Just, Nothing),
  Show,
  decodeUtf8,
  encodeUtf8,
  show,
  toStrict,
  ($),
  (.),
  (<$>),
  (<&>),
  (<>),
 )

import Data.Aeson as Aeson (ToJSON, Value (Object))
import Data.Aeson.Text as Aeson (encodeToLazyText)
import Data.Csv as Csv (
  DefaultOrdered,
  ToField (..),
  ToNamedRecord,
  ToRecord,
 )
import Data.Text as T (Text, dropEnd, intercalate, split, splitOn)
import Data.Yaml as Yaml (encode)
import Database.SQLite.Simple (
  FromRow (..),
  Query,
  ResultError (ConversionFailed),
  SQLData (SQLNull, SQLText),
  field,
 )
import Database.SQLite.Simple.FromField (FromField (..), fieldData, returnError)
import Database.SQLite.Simple.FromRow (fieldWith)
import Database.SQLite.Simple.Internal (Field (Field))
import Database.SQLite.Simple.Ok (Ok (Errors, Ok))
import Database.SQLite.Simple.QQ (sql)
import GHC.Exception (errorCallException)
import Prettyprinter (Pretty (pretty))

import Note (Note (..))
import Task (
  Task (
    awake_utc,
    closed_utc,
    due_utc,
    modified_utc,
    ready_utc,
    review_utc,
    state,
    ulid,
    waiting_utc
  ),
  TaskState,
  zeroTask,
 )


-- | Final user-facing format of tasks
data FullTask = FullTask
  { ulid :: Text -- TODO: Use Ulid type
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
      -- Tags
      <*> fieldWith
        ( \value -> case fieldData value of
            SQLText txt ->
              -- Parse tags as a comma-separated list
              -- TODO: Parse tags as a JSON array
              Ok (Just (T.splitOn "," txt))
            SQLNull -> Ok Nothing
            val ->
              Errors
                [ errorCallException $
                    "Expected a string, but got: " <> show val
                ]
        )
      -- Notes
      <*> fieldWith
        ( \value -> case fieldData value of
            SQLText _txt ->
              -- TODO: Parse notes as a JSON array
              Ok Nothing
            SQLNull -> Ok Nothing
            val ->
              Errors
                [ errorCallException $
                    "Expected a string, but got: " <> show val
                ]
        )
      <*> field
      <*> field
      <*> ( field <&> \case
              Just (Object obj) -> Just $ Object obj
              _ -> Nothing
          )


instance FromField [Text] where
  fromField (Field (SQLText txt) _) = Ok $ split (== ',') txt
  fromField f = returnError ConversionFailed f "expecting SQLText column type"


instance FromField [Note] where
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


instance ToJSON FullTask


instance Hashable FullTask


instance Pretty FullTask where
  pretty =
    pretty
      . T.dropEnd 1 -- Drop trailing newline to maybe add it later
      . decodeUtf8
      . Yaml.encode


emptyFullTask :: FullTask
emptyFullTask =
  FullTask
    { ulid = ""
    , body = ""
    , modified_utc = ""
    , awake_utc = Nothing
    , ready_utc = Nothing
    , waiting_utc = Nothing
    , review_utc = Nothing
    , due_utc = Nothing
    , closed_utc = Nothing
    , state = Nothing
    , group_ulid = Nothing
    , repetition_duration = Nothing
    , recurrence_duration = Nothing
    , tags = Nothing
    , notes = Nothing
    , priority = Nothing
    , user = ""
    , metadata = Nothing
    }


selectQuery :: Query
selectQuery =
  [sql|
    SELECT
      tasks_view.ulid AS ulid,
      body,
      modified_utc,
      awake_utc,
      ready_utc,
      waiting_utc,
      review_utc,
      due_utc,
      closed_utc,
      state,
      group_ulid,
      repetition_duration,
      recurrence_duration,
      tags,
      notes,
      priority,
      user,
      metadata
  |]


cpTimesAndState :: FullTask -> Task
cpTimesAndState (FullTask{..}) =
  zeroTask
    { Task.ulid
    , Task.modified_utc
    , Task.awake_utc
    , Task.ready_utc
    , Task.waiting_utc
    , Task.review_utc
    , Task.due_utc
    , Task.closed_utc
    , Task.state
    }
