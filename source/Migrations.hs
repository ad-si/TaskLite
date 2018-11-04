{-|
Migrations of SQLite database for new versions
-}

module Migrations where

import Protolude as P
import Database.SQLite.Simple
import Language.SQL.SimpleSQL.Syntax
import DbSetup
import Data.Text.Prettyprint.Doc hiding ((<>))
import SqlUtils


data MigrateDirection = MigrateUp | MigrateDown


_2018_11_04_add_user :: MigrateDirection -> Query
_2018_11_04_add_user = \case
  MigrateUp -> ""
  MigrateDown -> ""


_2018_11_04_add_deleted :: MigrateDirection -> Query
_2018_11_04_add_deleted = \case
  MigrateUp -> ""
  MigrateDown -> ""


runMigrations :: Connection -> IO (Doc ann)
runMigrations connection = do
  m1 <- runMigration connection $ _2018_11_04_add_user MigrateUp
  m2 <- runMigration connection $ _2018_11_04_add_deleted MigrateUp

  pure $
    m1 <> m2
