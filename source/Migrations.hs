{-|
Migrations of SQLite database for new versions
-}

module Migrations where

import Protolude as P

import Data.Text as T
import Database.SQLite.Simple
import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.Parse
import Data.Text.Prettyprint.Doc hiding ((<>))
import SqlUtils

-- | List of queries for one migration
type QuerySet = [Query]

data MigrateDirection = MigrateUp | MigrateDown
data Migration = Migration
  { id :: Int
  , querySet :: QuerySet
  }



_2018_11_04_add_user :: MigrateDirection -> Migration
_2018_11_04_add_user =
  let
    base = Migration
      { id = 20181104
      , querySet = []
      }
  in \case
    MigrateUp -> base {
        Migrations.querySet =
          ["alter table tasks add column user text"]
      }

    MigrateDown -> base {
        Migrations.querySet =
          [ "create table tasks_temp"
          , "insert into tasks_temp \
            \select ulid, body, state, due_utc, closed_utc, \
            \  modified_utc, priority_adjustment, metadata from tasks"
          , "drop table tasks"
          , "alter table tasks_temp rename to tasks"
        ]
      }


_2018_12_01_add_deleted :: MigrateDirection -> Migration
_2018_12_01_add_deleted =
  let
    base = Migration
      { id = 20181201
      , querySet = []
      }
  in \case
    MigrateUp -> base { Migrations.querySet = [] }
    MigrateDown -> base { Migrations.querySet = [] }


wrapQuery :: QuerySet -> QuerySet
wrapQuery querySet =
  querySet <>
  [ "pragma foreign_key_check"
  , "pragma user_version = 123"
  ]


lintQuery :: Query -> Either Text Query
lintQuery sqlQuery =
  let
    queryStr = T.unpack $ fromQuery sqlQuery
    result = parseStatements ansi2011 "migration" Nothing queryStr
  in case result of
    Left error -> Left (show error)
    Right _ -> Right sqlQuery


lintQuerySet :: QuerySet -> Either Text QuerySet
lintQuerySet queries =
  sequence $ fmap lintQuery queries


runMigrations :: Connection -> IO (Doc ann)
runMigrations connection = do
  let
    migrations =
      [ _2018_11_04_add_user
      -- , _2018_11_04_add_deleted
      ]
    lintedMigrations :: Either Text [QuerySet]
    lintedMigrations = migrations
      <&> ($ MigrateUp)
      <&> (lintQuerySet . Migrations.querySet)
      <&> fmap wrapQuery
      & sequence

  case lintedMigrations of
    Left error -> pure $ pretty error
    Right querySets ->
      querySets
        <&> runMigration connection
        & sequence
        <&> P.fold
