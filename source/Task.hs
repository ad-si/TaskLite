module Task where

import Protolude as P

import Data.Aeson as Aeson
import Data.Aeson.Text as Aeson
import Data.Yaml as Yaml
import qualified Data.ByteString.Lazy as BSL
import Data.Csv as Csv
import Data.Text as T
import Data.Text.Prettyprint.Doc hiding ((<>))
import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Sqlite.Connection
import Database.Beam.Sqlite.Syntax (SqliteExpressionSyntax, SqliteValueSyntax)
import Database.SQLite.Simple as Sql
import Database.SQLite.Simple.FromField as Sql.FromField
import Database.SQLite.Simple.ToField as Sql.ToField
import Database.SQLite.Simple.Internal hiding (result)
import Database.SQLite.Simple.Ok


data TaskState
  = Open
  | Waiting
  | Done
  | Obsolete
  deriving (Eq, Enum, Generic, Ord, Read, Show)

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

instance HasSqlEqualityCheck SqliteExpressionSyntax TaskState

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
      | t `elem` ["open", "pending", "recurring"] = Just Open
      | t == "waiting" = Just Waiting
      | t `elem` ["done", "completed", "finished", "fixed"] = Just Done
      | t `elem` ["obsolete", "deleted"] = Just Obsolete
      | otherwise = Nothing
    txtLower = T.toLower txt
  in
    func txtLower


newtype Ulid = Ulid Text

-- | Uses _ to match Beam's defaults
-- | (http://tathougies.github.io/beam/user-guide/models/#defaults)
data TaskT f = Task
  { ulid :: Columnar f Text -- Ulid
  , body :: Columnar f Text
  , state :: Columnar f TaskState
  , due_utc :: Columnar f (Maybe Text)
  , closed_utc :: Columnar f (Maybe Text)
  , modified_utc :: Columnar f Text
  , priority_adjustment :: Columnar f (Maybe Float)
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
    <*> field <*> field

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
