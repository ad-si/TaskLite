{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use uncurry" #-}

{-|
Datatype to represent a task as stored in the `tasks` table
-}
module Task where

import Protolude (
  Applicative ((<*>)),
  Either (Left, Right),
  Enum (toEnum),
  Eq ((==)),
  Float,
  Functor (fmap),
  Generic,
  Hashable,
  Maybe (..),
  Ord ((<), (>)),
  Read,
  Semigroup ((<>)),
  Show,
  decodeUtf8,
  fst,
  otherwise,
  pure,
  show,
  snd,
  ($),
  (&),
  (&&),
  (.),
  (<$>),
  (<&>),
 )
import Protolude qualified as P

import Data.Aeson as Aeson (
  FromJSON,
  ToJSON,
  Value (Object),
  eitherDecodeStrictText,
  encode,
 )
import Data.Aeson.Key as Key (fromText)
import Data.Aeson.KeyMap as KeyMap (fromList, insert)
import Data.ByteString.Lazy qualified as BSL
import Data.Csv qualified as Csv
import Data.Generics (Data)
import Data.Hourglass (DateTime, timePrint)
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Yaml as Yaml (encode)
import Database.SQLite.Simple as Sql (
  FromRow (..),
  ResultError (ConversionFailed),
  SQLData (SQLText),
  ToRow (toRow),
  field,
 )
import Database.SQLite.Simple.FromField as Sql.FromField (
  FromField (..),
  returnError,
 )
import Database.SQLite.Simple.Internal (Field (Field))
import Database.SQLite.Simple.Ok (Ok (Ok))
import Database.SQLite.Simple.ToField as Sql.ToField (ToField (..), toField)
import Generic.Random (genericArbitrary, genericArbitraryU, (%))
import Prettyprinter (Pretty (pretty))
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.QuickCheck.Instances.Text ()

import Config (defaultConfig, utcFormat)
import Control.Arrow ((>>>))
import Database.SQLite.Simple (
  Connection,
  Only (Only),
  SQLData (SQLNull),
  query,
 )
import Database.SQLite.Simple.QQ (sql)


data TaskState
  = Done
  | Obsolete
  | Deletable
  deriving (Eq, Enum, Generic, Ord, Read, Show, Data)


instance Arbitrary TaskState where
  arbitrary = genericArbitrary (4 % 2 % 1 % ())


instance Sql.FromField.FromField TaskState where
  fromField f@(Field (SQLText txt) _) =
    case textToTaskState txt of
      Just val -> Ok val
      Nothing ->
        returnError ConversionFailed f $
          T.unpack $
            "expecting a valid TaskState and not \"" <> txt <> "\""
  fromField f =
    returnError ConversionFailed f "expecting SQLText column type"


instance Sql.ToField.ToField TaskState where
  toField = SQLText . show


instance FromJSON TaskState
instance ToJSON TaskState


instance Csv.ToRecord TaskState
instance Csv.ToNamedRecord TaskState


instance Csv.ToField TaskState where
  toField = show


instance Hashable TaskState


stateDefault :: TaskState
stateDefault = toEnum 0


stateOptions :: Text
stateOptions =
  T.intercalate "," $
    fmap (("'" <>) . (<> "'") . show) [stateDefault ..]


textToTaskState :: Text -> Maybe TaskState
textToTaskState txt =
  let
    func t
      | t `P.elem` ["done", "completed", "finished", "fixed"] = Just Done
      | t `P.elem` ["obsolete"] = Just Obsolete
      | t
          `P.elem` [ "deletable"
                   , "deleted"
                   , "removable"
                   , "removed"
                   ] =
          Just Deletable
      | otherwise = Nothing
    txtLower = T.toLower txt
  in
    func txtLower


data DerivedState
  = IsOpen
  | IsClosed
  | IsAsleep
  | IsAwake
  | IsReady
  | IsWaiting
  | IsReview
  | IsDone
  | IsObsolete
  | IsDeletable
  | IsBlocked
  deriving (Eq, Generic, Show)


instance Arbitrary DerivedState where
  arbitrary = genericArbitraryU


{-| A tuple of (Primary State, Secondary State)
 | Check out tasklite.org/concepts for a
 | detailed explanation of the different states
 | and how they relate to each other
-}
type StateHierarchy = (DerivedState, DerivedState)


instance {-# OVERLAPS #-} Pretty StateHierarchy where
  pretty stateH =
    ( if fst stateH == snd stateH
        then show $ fst stateH
        else
          [fst stateH, snd stateH]
            <&> show
            & T.intercalate " and "
    )
      & T.replace "Is" ""
      & pretty


textToDerivedState :: Text -> Maybe DerivedState
textToDerivedState =
  T.toLower >>> \case
    "open" -> Just IsOpen
    "closed" -> Just IsClosed
    "asleep" -> Just IsAsleep
    "awake" -> Just IsAwake
    "ready" -> Just IsReady
    "waiting" -> Just IsWaiting
    "review" -> Just IsReview
    "done" -> Just IsDone
    "obsolete" -> Just IsObsolete
    "deletable" -> Just IsDeletable
    "blocked" -> Just IsBlocked
    _ -> Nothing


-- TODO: This should be based on the SQL views
derivedStateToQuery :: DerivedState -> Text
derivedStateToQuery = \case
  IsOpen ->
    "closed_utc IS NULL"
  IsClosed ->
    "closed_utc IS NOT NULL"
  IsAsleep ->
    "closed_utc IS NULL \
    \AND awake_utc > datetime('now') \
    \AND (ready_utc IS NULL OR ready_utc > datetime('now'))"
  IsAwake ->
    "closed_utc IS NULL \
    \AND awake_utc < datetime('now') \
    \AND (ready_utc IS NULL OR ready_utc > datetime('now'))"
  IsReady ->
    "closed_utc IS NULL \
    \AND ( \
    \  review_utc <= datetime('now') \
    \  OR ready_utc <= datetime('now') \
    \  OR ( \
    \    ready_utc IS NULL \
    \    AND (awake_utc IS NULL OR awake_utc <= datetime('now')) \
    \    AND (waiting_utc IS NULL OR waiting_utc > datetime('now')) \
    \    AND (review_utc IS NULL OR review_utc > datetime('now')) \
    \  ) \
    \)"
  IsWaiting ->
    "closed_utc IS NULL \
    \AND waiting_utc IS NOT NULL \
    \AND (review_utc IS NULL OR review_utc > datetime('now'))"
  IsReview ->
    "closed_utc IS NULL \
    \AND waiting_utc IS NOT NULL \
    \AND (review_utc IS NULL OR review_utc < datetime('now'))"
  IsDone ->
    "closed_utc IS NOT NULL AND state IS 'Done'"
  IsObsolete ->
    "closed_utc IS NOT NULL AND state IS 'Obsolete'"
  IsDeletable ->
    "closed_utc IS NOT NULL AND state IS 'Deletable'"
  IsBlocked -> "" -- TODO


getStateHierarchy :: DateTime -> Task -> StateHierarchy
getStateHierarchy now task = do
  let
    nowTxt = pack $ timePrint (utcFormat defaultConfig) now

  case Task.state task of
    Just Done -> (IsClosed, IsDone)
    Just Obsolete -> (IsClosed, IsObsolete)
    Just Deletable -> (IsClosed, IsDeletable)
    Nothing -> case closed_utc task of
      Just _ -> (IsClosed, IsClosed)
      Nothing -> case review_utc task of
        Just val ->
          if val > nowTxt
            then (IsOpen, IsWaiting)
            else (IsOpen, IsReview)
        Nothing -> case waiting_utc task of
          Just _ -> (IsOpen, IsWaiting)
          Nothing -> case (ready_utc task, awake_utc task) of
            (Just readyUtc, Just awakeUtc) ->
              if readyUtc < nowTxt && awakeUtc < nowTxt
                then (IsOpen, IsReady)
                else
                  if readyUtc > nowTxt && awakeUtc < nowTxt
                    then (IsOpen, IsAwake)
                    else (IsOpen, IsAsleep)
            (Just readyUtc, Nothing) | readyUtc < nowTxt -> (IsOpen, IsReady)
            (Nothing, Just awakeUtc) | awakeUtc < nowTxt -> (IsOpen, IsAwake)
            (Nothing, Just awakeUtc) | awakeUtc > nowTxt -> (IsOpen, IsAsleep)
            _ -> (IsOpen, IsOpen)


newtype Ulid = Ulid Text


data Task = Task
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
  , priority_adjustment :: Maybe Float
  , user :: Text
  , metadata :: Maybe Aeson.Value
  }
  deriving (Generic, Data, Show, Eq)


-- For conversion from SQLite with SQLite.Simple
instance FromRow Task where
  fromRow =
    Task
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
      <*> ( field <&> \case
              Just (Object obj) -> Just $ Object obj
              _ -> Nothing
          )


instance ToRow Task where
  toRow task =
    [ Sql.ToField.toField task.ulid
    , Sql.ToField.toField task.body
    , Sql.ToField.toField task.modified_utc
    , Sql.ToField.toField task.awake_utc
    , Sql.ToField.toField task.ready_utc
    , Sql.ToField.toField task.waiting_utc
    , Sql.ToField.toField task.review_utc
    , Sql.ToField.toField task.due_utc
    , Sql.ToField.toField task.closed_utc
    , Sql.ToField.toField task.state
    , Sql.ToField.toField task.group_ulid
    , Sql.ToField.toField task.repetition_duration
    , Sql.ToField.toField task.recurrence_duration
    , Sql.ToField.toField task.priority_adjustment
    , Sql.ToField.toField task.user
    , case task.metadata of
        Nothing -> SQLNull
        Just val -> SQLText $ val & (Aeson.encode >>> BSL.toStrict >>> decodeUtf8)
    ]


instance Hashable Task


instance Sql.FromField.FromField Value where
  fromField aField@(Field (SQLText txt) _) =
    case eitherDecodeStrictText txt of
      Left error -> returnError ConversionFailed aField error
      Right value -> Ok value
  fromField f = returnError ConversionFailed f "expecting SQLText column type"


-- For conversion to JSON
instance ToJSON Task


instance Pretty Task where
  pretty =
    pretty
      . T.dropEnd 1 -- Drop trailing newline to maybe add it later
      . decodeUtf8
      . Yaml.encode


instance Arbitrary Task where
  arbitrary = genericArbitraryU


emptyTask :: Task
emptyTask =
  Task
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
    , priority_adjustment = Nothing
    , user = ""
    , metadata = Nothing
    }


setMetadataField :: Text -> Value -> Task -> Task
setMetadataField fieldNameText value task =
  task
    { metadata =
        case metadata task of
          Just (Object obj) ->
            Just $ Object $ KeyMap.insert (Key.fromText fieldNameText) value obj
          Nothing ->
            Just $ Object $ fromList [(Key.fromText fieldNameText, value)]
          _ -> metadata task
    }


{-| Convert a task to a Markdown string with YAML frontmatter that can be edited
| and then converted back to a task.
| Tags and notes are commented out, so they are not accidentally added again.
-}
taskToEditableMarkdown :: Connection -> Task -> P.IO P.ByteString
taskToEditableMarkdown conn task = do
  (tags :: [[P.Text]]) <-
    if T.null task.ulid
      then pure []
      else
        query
          conn
          [sql|
            SELECT tag
            FROM task_to_tag
            WHERE task_ulid == ?
          |]
          (Only task.ulid)

  (notes :: [[P.Text]]) <-
    if T.null task.ulid
      then pure []
      else
        query
          conn
          [sql|
            SELECT note
            FROM task_to_note
            WHERE task_ulid == ?
          |]
          (Only task.ulid)

  let
    indentNoteContent noteContent =
      noteContent
        & T.strip
        & T.lines
        <&> T.stripEnd
        & T.intercalate "\n#     "

    taskWithEmptyBody = task{body = ""}
    frontmatterYaml =
      ( taskWithEmptyBody
          & Yaml.encode
          & P.decodeUtf8
          & T.replace "\nbody: ''\n" "\n"
      )
        <> "\n# | Existing tags and notes can't be edited here, \
           \but new ones can be added\n\n"
        <> (("# tags: " :: Text) <> P.show (P.concat tags) <> "\n")
        <> "tags: []\n"
        <> ( ("\n# notes:\n" :: Text)
              <> ( notes
                    & P.concat
                    <&> (\note -> "# - " <> indentNoteContent note)
                    & T.unlines
                 )
           )
        <> "notes: []\n"

  pure $
    ("---\n" <> frontmatterYaml <> "...\n\n" <> body task)
      & P.encodeUtf8
