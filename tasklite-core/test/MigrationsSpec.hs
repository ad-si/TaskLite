module MigrationsSpec where

import Protolude (Either (..), IO, Maybe (..), Text, die, pure, show, ($))

import Data.Text (unpack)
import Database.SQLite.Simple qualified as Sql
import Test.Hspec (
  Spec,
  it,
  shouldBe,
  shouldNotContain,
  shouldStartWith,
 )

import Config (defaultConfig)
import Migrations (
  MigrateDirection (MigrateDown, MigrateUp),
  Migration,
  querySet,
  runMigration,
  runMigrations,
  wrapMigration,
  _11_,
  _12_,
 )


runMigration_ :: Sql.Connection -> Migration -> IO ()
runMigration_ conn migration = do
  result <- runMigration conn $ querySet $ wrapMigration migration
  case result of
    Left error -> die $ show error
    Right _ -> pure ()


spec :: Spec
spec = do
  it "creates tables on initial run migrates tables to latest version" $ do
    Sql.withConnection ":memory:" $ \memConn -> do
      migrationStatus <- runMigrations defaultConfig memConn
      unpack (show migrationStatus) `shouldStartWith` "Migration succeeded"

  it "migrates notes to comments with the user of their task as author" $ do
    Sql.withConnection ":memory:" $ \memConn -> do
      _ <- runMigrations defaultConfig memConn
      Sql.execute_
        memConn
        "INSERT INTO tasks (ulid, body, modified_utc, user) \
        \VALUES ('01hs68z7mdg4ktpxbv0yfafznq', 'Task', '', 'jane')"
      Sql.execute_
        memConn
        "INSERT INTO task_to_comment \
        \VALUES ('01hs68z7mdg4ktpxbv0yfafznr', \
        \'01hs68z7mdg4ktpxbv0yfafznq', 'A comment', 'john')"

      runMigration_ memConn $ _11_ MigrateDown
      notes :: [(Text, Text)] <-
        Sql.query_ memConn "SELECT ulid, note FROM task_to_note"
      notes `shouldBe` [("01hs68z7mdg4ktpxbv0yfafznr", "A comment")]

      runMigration_ memConn $ _11_ MigrateUp
      comments :: [(Text, Text, Text)] <-
        Sql.query_ memConn "SELECT ulid, comment, user FROM task_to_comment"
      comments
        `shouldBe` [("01hs68z7mdg4ktpxbv0yfafznr", "A comment", "jane")]
      viewComments :: [Sql.Only Text] <-
        Sql.query_ memConn "SELECT comments FROM tasks_view"
      viewComments
        `shouldBe` [ Sql.Only
                       "[{\"ulid\":\"01hs68z7mdg4ktpxbv0yfafznr\",\
                       \\"body\":\"A comment\",\"user\":\"jane\"}]"
                   ]

  it "initializes created_utc from the ULID without changing modified_utc" $ do
    Sql.withConnection ":memory:" $ \memConn -> do
      _ <- runMigrations defaultConfig memConn
      runMigration_ memConn $ _12_ MigrateDown
      Sql.execute_
        memConn
        "INSERT INTO tasks (ulid, body, modified_utc) \
        \VALUES ('01hq68smfe0r9entg3x4rb9441', 'Task', '2024-03-01 10:00:00')"

      runMigration_ memConn $ _12_ MigrateUp
      tasks :: [(Text, Text)] <-
        Sql.query_ memConn "SELECT created_utc, modified_utc FROM tasks"
      tasks `shouldBe` [("2024-02-21 16:43:17.358", "2024-03-01 10:00:00")]
      viewTasks :: [Sql.Only Text] <-
        Sql.query_ memConn "SELECT created_utc FROM tasks_view"
      viewTasks `shouldBe` [Sql.Only "2024-02-21 16:43:17.358"]

      runMigration_ memConn $ _12_ MigrateDown
      columns :: [Sql.Only Text] <-
        Sql.query_ memConn "SELECT name FROM pragma_table_info('tasks')"
      columns `shouldNotContain` [Sql.Only "created_utc"]

  it "sets created_utc of tasks inserted without one via SQL trigger" $ do
    Sql.withConnection ":memory:" $ \memConn -> do
      _ <- runMigrations defaultConfig memConn
      Sql.execute_
        memConn
        "INSERT INTO tasks (ulid, body, modified_utc) \
        \VALUES ('01hq68smfe0r9entg3x4rb9441', 'Task', '2024-03-01 10:00:00')"
      tasks :: [(Maybe Text, Text)] <-
        Sql.query_ memConn "SELECT created_utc, modified_utc FROM tasks"
      tasks
        `shouldBe` [(Just "2024-02-21 16:43:17.358", "2024-03-01 10:00:00")]
