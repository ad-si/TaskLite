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


module FullTask where

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
import Task as Task


-- | Final user-facing format of tasks
data FullTask = FullTask
  { ulid :: Text -- Ulid
  , body :: Text
  , state :: TaskState
  , due_utc :: Maybe Text
  , closed_utc :: Maybe Text
  , modified_utc :: Text
  , tags :: Maybe [Text]
  , priority :: Maybe Float
  } deriving (Generic, Show)


-- For conversion from SQLite with SQLite.Simple
instance FromRow FullTask where
  fromRow = FullTask
    <$> field <*> field <*> field
    <*> field <*> field <*> field
    <*> field <*> field

instance Sql.FromField.FromField [Text] where
  fromField (Field (SQLText txt) _) = Ok $ split (== ',') txt
  fromField f = returnError ConversionFailed f "expecting SQLText column type"


instance Csv.ToField [Text] where
  toField = encodeUtf8 . (T.intercalate ",")

-- For conversion to CSV
instance ToRecord FullTask
instance ToNamedRecord FullTask
instance DefaultOrdered FullTask

-- For conversion to JSON
instance ToJSON FullTask

instance Hashable FullTask
