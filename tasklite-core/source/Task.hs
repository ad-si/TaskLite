{-|
Datatype to represent a task as stored in the `tasks` table
-}

module Task where

import Protolude as P hiding ((%))

import Data.Aeson as Aeson
import Data.Aeson.Text as Aeson
import qualified Data.HashMap.Lazy as HM
import Data.Hourglass (DateTime, timePrint)
import Data.Yaml as Yaml
import qualified Data.ByteString.Lazy as BSL
import Data.Csv as Csv
import Data.Text as T
import Data.Text.Prettyprint.Doc hiding ((<>))
import qualified Data.Vector as V
import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Sqlite.Connection
import Database.Beam.Sqlite.Syntax (SqliteValueSyntax)
import Database.SQLite.Simple as Sql
import Database.SQLite.Simple.FromField as Sql.FromField
import Database.SQLite.Simple.ToField as Sql.ToField
import Database.SQLite.Simple.Internal hiding (result)
import Database.SQLite.Simple.Ok
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()
import Generic.Random

import Config (utcFormat, defaultConfig)


-- From https://gist.github.com/chrisdone/7b0c4ebb5b9b94514959206df8992076
instance Arbitrary Aeson.Value where
  arbitrary = sized sizedArbitraryValue

sizedArbitraryValue :: Int -> Gen Aeson.Value
sizedArbitraryValue n
  | n <= 0 = oneof [pure Aeson.Null, jsonBool, jsonNumber, jsonString]
  | otherwise = resize nHalf $
      oneof [ pure Aeson.Null, jsonBool, jsonNumber
            , jsonString, jsonArray, jsonObject]
  where
    nHalf = n `div` 2
    jsonBool = Aeson.Bool <$> arbitrary
    jsonNumber =
      (Aeson.Number . fromRational . toRational :: Double -> Aeson.Value)
      <$> arbitrary
    jsonString = (Aeson.String . T.pack) <$> arbitrary
    jsonArray = (Aeson.Array . V.fromList) <$> arbitrary
    jsonObject =
      (Aeson.Object . HM.fromList . P.map (first T.pack))
      <$> arbitrary


data TaskState
  = Done
  | Obsolete
  | Deletable
  deriving (Eq, Enum, Generic, Ord, Read, Show)

instance Arbitrary TaskState where
  arbitrary = genericArbitrary (4 % 2 % 1 % ())

instance Sql.FromField.FromField TaskState where
  fromField f@(Field (SQLText txt) _) = case (textToTaskState txt) of
    Just val -> Ok val
    Nothing -> returnError ConversionFailed f $ T.unpack $
      "expecting a valid TaskState and not \"" <> txt <> "\""
  fromField f = returnError ConversionFailed f "expecting SQLText column type"

instance Sql.ToField.ToField TaskState where
  toField = SQLText . show

instance HasSqlValueSyntax be [Char] => HasSqlValueSyntax be TaskState where
  sqlValueSyntax = autoSqlValueSyntax

instance FromBackendRow Sqlite TaskState

instance HasSqlEqualityCheck Sqlite TaskState

instance FromJSON TaskState
instance ToJSON TaskState

instance ToRecord TaskState
instance ToNamedRecord TaskState

instance Csv.ToField TaskState where
  toField = show

instance Hashable TaskState

stateDefault :: TaskState
stateDefault = (toEnum 0) :: TaskState

stateOptions :: Text
stateOptions = T.intercalate "," $
  fmap (("'" <>) . (<> "'") . show) [stateDefault ..]

textToTaskState :: Text -> Maybe TaskState
textToTaskState txt =
  let
    func t
      | t `elem` ["done", "completed", "finished", "fixed"] = Just Done
      | t `elem` ["obsolete"] = Just Obsolete
      | t `elem` [ "deletable", "deleted"
                 , "removable", "removed"] = Just Deletable
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


-- | A tuple of (Primary State, Secondary State)
-- | Check out tasklite.org/concepts for a
-- | detailed explanation of the different states
-- | and how they relate to each other
type StateHierachy = (DerivedState, DerivedState)

instance {-# OVERLAPS #-} Pretty StateHierachy where
  pretty stateH = (
    if fst stateH == snd stateH
    then show $ fst stateH
    else [fst stateH, snd stateH]
      <&> show
      & T.intercalate " and "
    )
      & T.replace "Is" ""
      & pretty


textToDerivedState :: Text -> Maybe DerivedState
textToDerivedState = \case
  "open"      -> Just IsOpen
  "closed"    -> Just IsClosed
  "asleep"    -> Just IsAsleep
  "awake"     -> Just IsAwake
  "ready"     -> Just IsReady
  "waiting"   -> Just IsWaiting
  "review"    -> Just IsReview
  "done"      -> Just IsDone
  "obsolete"  -> Just IsObsolete
  "deletable" -> Just IsDeletable
  "blocked"   -> Just IsBlocked
  _ -> Nothing


derivedStateToQuery :: DerivedState -> Text
derivedStateToQuery = \case
  IsOpen      -> "closed_utc is null"
  IsClosed    -> "closed_utc is not null"
  IsAsleep    -> "awake_utc > datetime('now') and \
                  \(ready_utc is null or ready_utc > datetime('now')) \
                  \and closed_utc is null"
  IsAwake     -> "awake_utc < datetime('now') and \
                  \(ready_utc is null or ready_utc > datetime('now')) \
                  \and closed_utc is null"
  IsReady     -> "(awake_utc is null or awake_utc < datetime('now')) \
                  \and ready_utc < datetime('now') \
                  \and closed_utc is null"
  IsWaiting   -> "waiting_utc is not null and \
                  \(review_utc is null or review_utc > datetime('now')) \
                  \and closed_utc is null"
  IsReview    -> "waiting_utc is not null and \
                  \(review_utc is null or review_utc < datetime('now')) \
                  \and closed_utc is null"
  IsDone      -> "closed_utc is not null and state is 'Done'"
  IsObsolete  -> "closed_utc is not null and state is 'Obsolete'"
  IsDeletable -> "closed_utc is not null and state is 'Deletable'"
  IsBlocked   -> "" -- TODO


getStateHierarchy :: DateTime -> Task -> StateHierachy
getStateHierarchy now task =
  let
    nowTxt = pack $ timePrint (utcFormat defaultConfig) now
  in
    case Task.state task of
      Just Done -> (IsClosed, IsDone)
      Just Obsolete -> (IsClosed, IsObsolete)
      Just Deletable -> (IsClosed, IsDeletable)
      Nothing -> case closed_utc task of
        Just _ -> (IsClosed, IsClosed)
        Nothing -> case review_utc task of
          Just val -> if val > nowTxt
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

-- | Uses _ to match Beam's defaults
-- | (http://tathougies.github.io/beam/user-guide/models/#defaults)
data TaskT f = Task
  { ulid :: Columnar f Text -- Ulid
  , body :: Columnar f Text
  , modified_utc :: Columnar f Text
  , awake_utc :: Columnar f (Maybe Text)
  , ready_utc :: Columnar f (Maybe Text)
  , waiting_utc :: Columnar f (Maybe Text)
  , review_utc :: Columnar f (Maybe Text)
  , due_utc :: Columnar f (Maybe Text)
  , closed_utc :: Columnar f (Maybe Text)
  , state :: Columnar f (Maybe TaskState)
  , group_ulid :: Columnar f (Maybe Text)
  , repetition_duration :: Columnar f (Maybe Text)
  , recurrence_duration :: Columnar f (Maybe Text)
  , priority_adjustment :: Columnar f (Maybe Float)
  , user :: Columnar f Text
  , metadata :: Columnar f (Maybe Aeson.Value)
  } deriving Generic


-- Beam related instances
type Task = TaskT Identity
type TaskUlid = PrimaryKey TaskT Identity

deriving instance Show TaskUlid
deriving instance Show Task
deriving instance Eq Task

instance HasSqlValueSyntax SqliteValueSyntax Value where
  sqlValueSyntax =
    sqlValueSyntax . toStrict . Aeson.encodeToLazyText

instance FromBackendRow Sqlite Value

instance Beamable TaskT

instance Table TaskT where
  data PrimaryKey TaskT f = TaskUlid (Columnar f Text) deriving Generic
  primaryKey = TaskUlid . ulid
instance Beamable (PrimaryKey TaskT)

-- For conversion from SQLite with SQLite.Simple
instance FromRow Task where
  fromRow = Task
    <$> field <*> field <*> field
    <*> field <*> field <*> field
    <*> field <*> field <*> field
    <*> field <*> field <*> field
    <*> field <*> field <*> field
    <*> field

instance Hashable Task

instance Sql.FromField.FromField Value where
  fromField aField@(Field (SQLText txt) _) =
    case Aeson.eitherDecode $ BSL.fromStrict $ encodeUtf8 txt of
      Left error -> returnError ConversionFailed aField error
      Right value -> Ok value
  fromField f = returnError ConversionFailed f "expecting SQLText column type"

-- For conversion to JSON
instance ToJSON Task
instance ToJSON (PrimaryKey TaskT Identity)

instance Pretty Task where
  pretty = pretty
    . T.dropEnd 1 -- Drop trailing newline to maybe add it later
    . decodeUtf8
    . Yaml.encode

instance Arbitrary Task where
  arbitrary = genericArbitraryU


zeroTask :: Task
zeroTask = Task
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
