{-# LANGUAGE QuasiQuotes #-}

{-|
Data type to represent tasks from the `tasks_view`
-}
module FullTask where

import Protolude (
  Applicative (pure, (<*>)),
  Either (Left, Right),
  Eq,
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

import Data.Aeson as Aeson (
  FromJSON,
  ToJSON,
  Value (Object),
  eitherDecodeStrictText,
 )
import Data.Aeson.Text as Aeson (encodeToLazyText)
import Data.Csv as Csv (
  DefaultOrdered,
  ToField (..),
  ToNamedRecord,
  ToRecord,
 )
import Data.Text as T (Text, dropEnd, intercalate, unpack)
import Data.Yaml as Yaml (encode)
import Database.SQLite.Simple (
  FromRow (..),
  Query,
  SQLData (SQLNull, SQLText),
  field,
 )
import Database.SQLite.Simple.FromField (FieldParser, fieldData)
import Database.SQLite.Simple.FromRow (fieldWith)
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
  emptyTask,
 )
import Utils (ulidText2utc)


-- | Final user-facing format of tasks
data FullTask = FullTask
  { ulid :: Text -- TODO: Use Ulid type
  , body :: Text
  , created_utc :: Maybe Text
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


-- | Parse a JSON array field from SQLite, returning Nothing for NULL values
parseJsonArrayField :: (FromJSON a) => Text -> FieldParser (Maybe [a])
parseJsonArrayField fieldName value = case fieldData value of
  SQLText txt ->
    case Aeson.eitherDecodeStrictText txt of
      Right parsed -> Ok (Just parsed)
      Left err ->
        Errors
          [ errorCallException $
              "Failed to parse " <> T.unpack fieldName <> " JSON array: " <> err
          ]
  SQLNull -> Ok Nothing
  val ->
    Errors
      [ errorCallException $
          "Expected a string for " <> T.unpack fieldName <> ", but got: " <> show val
      ]


-- For conversion from SQLite with SQLite.Simple
instance FromRow FullTask where
  fromRow = do
    ulidValue <- field
    FullTask ulidValue
      <$> field -- body
      <*> pure (ulidText2utc ulidValue) -- created_utc (derived from ULID)
      <*> field -- modified_utc
      <*> field -- awake_utc
      <*> field -- ready_utc
      <*> field -- waiting_utc
      <*> field -- review_utc
      <*> field -- due_utc
      <*> field -- closed_utc
      <*> field -- state
      <*> field -- group_ulid
      <*> field -- repetition_duration
      <*> field -- recurrence_duration
      <*> fieldWith (parseJsonArrayField "tags")
      <*> fieldWith (parseJsonArrayField "notes")
      <*> field -- priority
      <*> field -- user
      <*> ( field <&> \case
              Just (Object obj) -> Just $ Object obj
              _ -> Nothing
          )


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
    , created_utc = Nothing
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
  emptyTask
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
