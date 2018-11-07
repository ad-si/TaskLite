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
import DbSetup


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


_1_add_user :: MigrateDirection -> Migration
_1_add_user =
  let
    base = Migration
      { id = UserVersion 1
      , querySet = []
      }
  in \case
    MigrateUp -> base { Migrations.querySet =
          ["alter table tasks add column user text"]
      }

    MigrateDown -> base { Migrations.querySet =
          [ "create table tasks_temp"
          , "insert into tasks_temp \
              \select ulid, body, state, due_utc, closed_utc, \
              \  modified_utc, priority_adjustment, metadata from tasks"
          , "drop table tasks"
          , "alter table tasks_temp rename to tasks"
        ]
      }


-- _2_add_deleted :: MigrateDirection -> Migration
-- _2_add_deleted =
--   let
--     base = Migration
--       { id = UserVersion 2
--       , querySet = []
--       }
--   in \case
--     MigrateUp -> base { Migrations.querySet =
--         ["alter table tasks add column deleted boolean"]
--       }
--     MigrateDown -> base { Migrations.querySet = [] }


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
lintQuery sqlQuery =
  let
    queryStr = T.unpack $ fromQuery sqlQuery
    result = parseStatements ansi2011 "migration" Nothing queryStr
  in case result of
    Left error -> Left (show error)
    Right _ -> Right sqlQuery


lintMigration :: Migration -> Either Text Migration
lintMigration migration =
  either
    (\leftVal -> Left leftVal)
    (\_ -> Right migration)
    (sequence $ fmap lintQuery (Migrations.querySet migration))


runMigration :: Connection -> [Query] -> IO (Either SQLError [()])
runMigration connection querySet = do
  withTransaction connection $ do
    result <- try $ sequence $ fmap (execute_ connection) querySet

    -- | For debuging: Print querySet of migrations
    -- putText $ "Result: " <> show querySet

    pure result


runMigrations :: Connection -> IO (Doc ann)
runMigrations connection = do
  currentVersionList <- (query_ connection
    "pragma user_version" :: IO [UserVersion])

  let
    migrations =
      [ _1_add_user
      -- , _2_add_deleted
      ]
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
      case migrationsUp
        <&> Migrations.id
        & hasDuplicates of
          True -> Left "Your migrations contain duplicate user versions"
          False -> Right []

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
        Left error -> pure $ pretty $ (show error :: Text)
        _ -> do
          execute_ connection $
            Query $ "pragma user_version = " <> (show userVersionMax)
          status <- replaceViewsAndTriggers connection
          pure $ (
            "Replaced views and triggers: "
            <> status
            <> "Migration succeeded. New user-version:"
            <> (pretty userVersionMax))
            <> hardline
