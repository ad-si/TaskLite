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


data MigrateDirection = MigrateUp | MigrateDown


_2018_11_04_add_user :: MigrateDirection -> Query
_2018_11_04_add_user = \case
  MigrateUp -> "\
    \alter table tasks \
    \add column user text not null; \
    \"

  MigrateDown -> "\
    \create table tasks_temp; \
    \\
    \insert into tasks_temp \
    \select ulid, body, state, due_utc, closed_utc, \
    \  modified_utc, priority_adjustment, metadata from tasks; \
    \\
    \drop table tasks; \
    \\
    \alter table tasks_temp rename to tasks; \
    \"


_2018_11_04_add_deleted :: MigrateDirection -> Query
_2018_11_04_add_deleted = \case
  MigrateUp -> "\
    \\
    \"

  MigrateDown -> "\
    \\
    \"


wrapQuery :: Query -> Query
wrapQuery (Query queryText) = Query $ "\
  \pragma foreign_keys=OFF; \
  \begin transaction; "
  <> queryText <> "\
  \pragma foreign_key_check; \
  \pragma user_version = 1; \
  \end transaction; \
  \pragma foreign_keys=ON; \
  \"


lintQuery :: Query -> Either Text Query
lintQuery sqlQuery =
  let
    queryStr = T.unpack $ fromQuery sqlQuery
    result = parseStatements ansi2011 "migration" Nothing queryStr
  in case result of
    Left error -> Left (show error)
    Right _ -> Right sqlQuery


runMigrations :: Connection -> IO (Doc ann)
runMigrations connection = do
  let
    migrations =
      [ _2018_11_04_add_user
      -- , _2018_11_04_add_deleted
      ]
    lintedMigrations :: Either Text [Query]
    lintedMigrations = migrations
      <&> ($ MigrateUp)
      <&> lintQuery
      <&> fmap wrapQuery
      & sequence

  -- pure $ pretty $ (show lintedMigrations :: Text)

  case lintedMigrations of
    Left error -> pure $ pretty error
    Right queries -> pure $ pretty $ (show queries :: Text)
      -- queries
      --   <&> runMigration connection
      --   & sequence
      --   <&> P.fold
