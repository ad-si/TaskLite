{-|
Unit and integration tests
-}

import Protolude as P

import Test.Hspec
import qualified Database.SQLite.Simple as Sql
import Data.Hourglass
import Data.Text as T
import System.IO.Temp
import System.IO.Error
import Time.System
import Data.List ((!!))

import Config (Config(..), defaultConfig)
import Lib
import Utils
import DbSetup
import Migrations


-- | The tests build up upon each other
-- | and therefore the order must not be changed
testSuite :: Config -> DateTime -> Sql.Connection -> SpecWith ()
testSuite conf now connection = do
  describe "TaskLite" $ do
    let
      -- TODO: Make function more generic
      getUlidFromBody = (!! 19) . T.words . pack . show


    it "creates necessary tables on initial run" $ do
      tableStatus <- createTables conf connection
      unpack (show tableStatus) `shouldBe`
        -- TODO: Improve formatting "create trigger"
        "🆕 Create table \"tasks\"\n\
        \🆕 Create table \"task_to_tag\"\n\
        \🆕 Create table \"task_to_note\"\n\
        \🆕 \"create trigger `set_modified_utc_after_update`\\n\
          \after… \n\
        \🆕 \"create trigger `set_closed_utc_after_update`\\n\
          \after… \n\
        \🆕 Create table \"tasks_view\"\n\
        \🆕 Create table \"tags\"\n"


    it "migrates tables to latest version" $ do
      migrationStatus <- runMigrations conf connection
      unpack (show migrationStatus) `shouldStartWith`
        "Replaced views and triggers:"


    it "initially contains no tasks" $ do
      tasks <- headTasks conf now connection
      unpack (show tasks) `shouldStartWith` "No tasks available"


    it "adds a task" $ do
      result <- addTask conf connection ["Just a test"]
      unpack (show result) `shouldStartWith`
        "🆕 Added task \"Just a test\" with id"


    context "When a task exists" $ do
      it "lists next task" $ do
        result <- nextTask connection
        unpack (show result) `shouldStartWith` "\
          \awake_utc: null\n\
          \review_utc: null\n\
          \state: null\n\
          \repetition_duration: null\n\
          \priority: 0\n\
          \recurrence_duration: null\n\
          \body: Just a test\n\
          \user: adrian\n\
          \"
        unpack (show result) `shouldEndWith` "\
          \group_ulid: null\n\
          \closed_utc: null\n\
          \metadata: null\n\
          \notes: null\n\
          \waiting_utc: null\n\
          \ready_utc: null\n\
          \tags: null\n\
          \due_utc: null"


      it "adds a tag" $ do
        result <- nextTask connection
        let ulidText = getUlidFromBody result

        tagResult <- addTag conf connection "test" [ulidText]
        unpack (show tagResult) `shouldStartWith`
          "🏷  Added tag \"test\" to task"


      it "adds a note" $ do
        result <- nextTask connection
        let ulidText = getUlidFromBody result

        tagResult <- addNote conf connection
                      "Just a test note" [ulidText]
        unpack (show tagResult) `shouldStartWith`
          "🗒  Added a note to task"


      it "sets due UTC" $ do
        resultTask <- nextTask connection
        let ulidText = getUlidFromBody resultTask

        case (parseUtc "2087-03-21 17:43") of
          Nothing -> throwIO $ userError "Invalid UTC string"
          Just utcStamp -> do
            result <- setDueUtc conf connection utcStamp [ulidText]
            unpack (show result) `shouldStartWith`
              "📅 Set due UTC of task \"Just a test\" with id"


      it "completes it" $ do
        result <- nextTask connection
        let ulidText = getUlidFromBody result

        doResult <- doTasks conf connection Nothing [ulidText]
        unpack (show doResult) `shouldStartWith` "✅ Finished task"


    it "adds a task with metadata and deletes it" $ do
      _ <- addTask conf connection
            ["Just a test +tag due:2082-10-03 +what"]
      result <- nextTask connection
      let ulidText = getUlidFromBody result

      deleteResult <- deleteTasks conf connection [ulidText]
      unpack (show deleteResult) `shouldStartWith` "❌ Deleted task"


    context "When a task was logged" $ do
      it "logs a task" $ do
        result <- logTask conf connection ["Just a test"]
        unpack (show result) `shouldStartWith`
          "📝 Logged task \"Just a test\" with id"


main :: IO ()
main = do
  withSystemTempFile "main.db" $ \filePath _ -> do
    connection <- Sql.open filePath
    nowElapsed <- timeCurrentP
    let now = timeFromElapsedP nowElapsed :: DateTime
    hspec $ testSuite defaultConfig now connection

  -- | Does not delete database after tests for debugging
  -- filePath <- emptySystemTempFile "main.db"
  -- putStrLn $ "\nFilepath: " <> filePath
  -- conf connection <- Sql.open filePath
  -- nowElapsed <- timeCurrentP
  -- let now = timeFromElapsedP nowElapsed :: DateTime
  -- hspec $ testSuite now conf connection
