{-|
Migrations of SQLite database for new versions
-}

module Migrations where

import Protolude as P

import Database.SQLite.Simple
import Data.Text.Prettyprint.Doc hiding ((<>))
import DbSetup
import Config


newtype UserVersion = UserVersion Int
  deriving (Eq, Ord, Read, Show)

instance FromRow UserVersion where
  fromRow = UserVersion <$> field


-- | List of queries for one migration
type QuerySet = [Query]


data MigrateDirection = MigrateUp | MigrateDown
data Migration = Migration
  { id :: UserVersion
  , querySet :: QuerySet
  }


-- | Add field "user"
_1_ :: MigrateDirection -> Migration
_1_ =
  let
    base = Migration
      { id = UserVersion 1
      , querySet = []
      }
  in \case
    MigrateUp -> base { Migrations.querySet =
          ["alter table tasks add column user text"]
      }

    -- TODO: Fix the invalid create table statement
    MigrateDown -> base { Migrations.querySet =
          [ "create table tasks_temp"
          , "insert into tasks_temp \
              \select ulid, body, state, due_utc, closed_utc, \
              \  modified_utc, priority_adjustment, metadata from tasks"
          , "drop table tasks"
          , "alter table tasks_temp rename to tasks"
        ]
      }


-- | Make state optional and add state "Deleted", add field "sleep_utc"
_2_ :: MigrateDirection -> Migration
_2_ =
  let
    base = Migration
      { id = UserVersion 2
      , querySet = []
      }
    createTempTableQueryUp = Query "\
      \create table tasks_temp ( \n\
      \  ulid text not null primary key, \n\
      \  body text not null, \n\
      \  state text check(state in (NULL, 'Done', 'Obsolete', 'Deleted')), \n\
      \  due_utc text, \n\
      \  sleep_utc text, \n\
      \  closed_utc text, \n\
      \  modified_utc text not null, \n\
      \  priority_adjustment float, \n\
      \  metadata text, \n\
      \  user text \n\
      \) \n\
      \"

    -- TODO: Finish query
    createTempTableQueryDown = Query "create table tasks_temp"

  in \case
    MigrateUp -> base { Migrations.querySet = (
        createTempTableQueryUp :
        "insert into tasks_temp \
          \select ulid, body, nullif(nullif(state,'Open'),'Waiting') as state, \
          \due_utc, NULL, closed_utc, \
          \modified_utc, priority_adjustment, metadata, user \
          \from tasks" :
        "drop table tasks" :
        "alter table tasks_temp rename to tasks" :
        [])
      }

    MigrateDown -> base { Migrations.querySet = (
        createTempTableQueryDown :
        "insert into tasks_temp \
          \select ulid, body, state, due_utc, closed_utc, \
          \  modified_utc, priority_adjustment, metadata, user from tasks" :
        "drop table tasks" :
        "alter table tasks_temp rename to tasks" :
        [])
      }


-- | Add fields awake_utc, ready_utc, waiting_utc, review_utc, closed_utc,
-- | group_ulid, repetition_duration, recurrence_duration,
_3_ :: MigrateDirection -> Migration
_3_ =
  let
    base = Migration
      { id = UserVersion 3
      , querySet = []
      }
    createTempTableQueryUp = Query "\
      \create table tasks_temp ( \n\
      \  ulid text not null primary key, \n\
      \  body text not null, \n\
      \  modified_utc text not null, \n\
      \  awake_utc text, \n\
      \  ready_utc text, \n\
      \  waiting_utc text, \n\
      \  review_utc text, \n\
      \  due_utc text, \n\
      \  closed_utc text, \n\
      \  state text check(state in (NULL, 'Done', 'Obsolete', 'Deletable')), \n\
      \  group_ulid text, \n\
      \  repetition_duration text, \n\
      \  recurrence_duration text, \n\
      \  priority_adjustment float, \n\
      \  user text, \n\
      \  metadata text \n\
      \) \n\
      \"

    -- TODO: Finish query
    createTempTableQueryDown = Query "create table tasks_temp"

  in \case
    MigrateUp -> base { Migrations.querySet = (
        createTempTableQueryUp :
        "insert into tasks_temp \
          \select ulid, body, modified_utc, sleep_utc, NULL, NULL, NULL, \
          \due_utc, closed_utc, state, NULL, NULL, NULL, \
          \priority_adjustment, user, metadata \
          \from tasks" :
        "drop table tasks" :
        "alter table tasks_temp rename to tasks" :
        [])
      }

    MigrateDown -> base { Migrations.querySet = (
        createTempTableQueryDown :
        "insert into tasks_temp \
          \select ulid, body, state, due_utc, closed_utc, \
          \  modified_utc, priority_adjustment, metadata, user from tasks" :
        "drop table tasks" :
        "alter table tasks_temp rename to tasks" :
        [])
      }


-- | Fixes activation condition of task closed trigger.
-- | FIXME: This empty migration is a hack to run an update of all triggers.
_4_ :: MigrateDirection -> Migration
_4_ =
  let
    base = Migration
      { id = UserVersion 4
      , querySet = []
      }
  in \case
    MigrateUp -> base { Migrations.querySet = [] }
    MigrateDown -> base { Migrations.querySet = [] }


hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs) =
  x `elem` xs || hasDuplicates xs


wrapQuery :: UserVersion -> QuerySet -> QuerySet
wrapQuery (UserVersion userVersion) querySet =
  querySet <>
  [ "pragma foreign_key_check"
  , "pragma user_version = " <> (Query $ show userVersion)
  ]


wrapMigration :: Migration -> Migration
wrapMigration migration =
  migration { querySet =
    wrapQuery (Migrations.id migration) (Migrations.querySet migration)
  }


lintQuery :: Query -> Either Text Query
lintQuery = Right
  -- TODO: Reactivate after
  --   https://github.com/JakeWheat/simple-sql-parser/issues/20 is fixed
  -- let
  --   queryStr = T.unpack $ fromQuery sqlQuery
  --   result = parseStatements ansi2011 "migration" Nothing queryStr
  -- in case result of
  --   Left error -> Left (show error)
  --   Right _ -> Right sqlQuery


lintMigration :: Migration -> Either Text Migration
lintMigration migration =
  either
    Left
    (\_ -> Right migration)
    (mapM lintQuery (Migrations.querySet migration))


runMigration :: Connection -> [Query] -> IO (Either SQLError [()])
runMigration connection querySet = do
  withTransaction connection $ do
    -- | For debuging: Print querySet of migrations
    -- putText $ "Result: " <> show querySet
    try $ mapM (execute_ connection) querySet


runMigrations :: Config -> Connection -> IO (Doc ann)
runMigrations conf connection = do
  currentVersionList <- (query_ connection
    "pragma user_version" :: IO [UserVersion])

  let
    migrations = (
        _1_ :
        _2_ :
        _3_ :
        _4_ :
      [])

    migrationsUp = fmap ($ MigrateUp) migrations
    (UserVersion userVersionMax) = migrationsUp
      <&> Migrations.id
      & P.maximum

    migrationsUpLinted :: Either Text [Migration]
    migrationsUpLinted = do
      currentVersion <- maybeToEither
        "'pragma user_verison' does not return current version"
        (P.head currentVersionList)

      -- | Check if duplicate user versions are defined
      _ <- if migrationsUp <&> Migrations.id & hasDuplicates
          then Left "Your migrations contain duplicate user versions"
          else Right []

      -- | Get new migrations, lint and wrap them
      migrationsUp
        & P.filter (\m -> (Migrations.id m) > currentVersion)
        <&> lintMigration
        <&> fmap wrapMigration
        & sequence

  case migrationsUpLinted of
    Left error -> pure $ pretty error
    Right [] -> pure ""
    Right migsUpLinted -> do
      result <- migsUpLinted
        <&> Migrations.querySet
        <&> runMigration connection
        & sequence

      case sequence result of
        Left error -> pure $ pretty (show error :: Text)
        _ -> do
          execute_ connection $
            Query $ "pragma user_version = " <> (show userVersionMax)
          status <- replaceViewsAndTriggers conf connection
          pure $ (
            "Replaced views and triggers: "
            <> status
            <> "Migration succeeded. New user-version:"
            <> (pretty userVersionMax))
            <> hardline
