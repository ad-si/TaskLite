{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Task where

import Protolude as P

import Data.Aeson as Aeson
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
  } deriving Generic


-- Beam related instances
type Task = TaskT Identity
type TaskUlid = PrimaryKey TaskT Identity

deriving instance Show Task
deriving instance Eq Task

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
    <*> field

instance Hashable Task
