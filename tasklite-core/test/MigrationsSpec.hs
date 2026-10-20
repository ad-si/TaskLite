module MigrationsSpec where

import Protolude (Either (..), IO, Text, die, pure, show, ($))

import Data.Text (unpack)
import Database.SQLite.Simple qualified as Sql
import Test.Hspec (
  Spec,
  it,
  shouldBe,
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
