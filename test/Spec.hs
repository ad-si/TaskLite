import Protolude as P

import Test.Hspec
import qualified Database.SQLite.Simple as Sql
import Data.Text as T
import System.IO.Temp
import System.IO.Error

import Lib
import Utils
import DbSetup


setupTestConnection :: FilePath -> IO Sql.Connection
setupTestConnection filePath = do
    connection <- Sql.open filePath

    createTaskTable connection
    createTagsTable connection
    createNotesTable connection
    createTaskView connection

    pure connection


-- | The tests build up upon each other
-- | and therefore the order must not be changed
testSuite :: Sql.Connection -> SpecWith ()
testSuite connection = do
  describe "TaskLite" $ do
    let
      taskStart = "state: Open\npriority: 0\nbody: Just a test\nulid: "
      getUlidFromBody = T.take 26 . T.drop (P.length taskStart) . pack . show


    it "creates necessary tables on initial run" $ do
      tasks <- headTasks connection
      (unpack $ show tasks) `shouldStartWith` "No tasks available"


    it "adds a task" $ do
      result <- addTask connection ["Just a test"]
      (unpack $ show result) `shouldStartWith`
        "🆕 Added task \"Just a test\" with ulid"


    context "When a task exists" $ do
      it "lists next task" $ do
        result <- nextTask connection
        (unpack $ show result) `shouldStartWith` taskStart


      it "adds a tag" $ do
        result <- nextTask connection
        let ulidText = getUlidFromBody result

        tagResult <- addTag connection "test" [ulidText]
        (unpack $ show tagResult) `shouldStartWith`
          "🏷  Added tag \"test\" to task"


      it "adds a note" $ do
        result <- nextTask connection
        let ulidText = getUlidFromBody result

        tagResult <- addNote connection "Just a test note" [ulidText]
        (unpack $ show tagResult) `shouldStartWith`
          "🗒  Added a note to task"


      it "sets due UTC" $ do
        resultTask <- nextTask connection
        let ulidText = getUlidFromBody resultTask

        case (parseUtc "2087-03-21 17:43") of
          Nothing -> throwIO $ userError "Invalid UTC string"
          Just utcStamp -> do
            result <- setDueUtc connection utcStamp [ulidText]
            (unpack $ show result) `shouldStartWith`
              "📅 Set due UTC to \"2087-03-21 17:43:00\" of task"


      it "completes it" $ do
        result <- nextTask connection
        let ulidText = getUlidFromBody result

        doResult <- doTasks connection [ulidText]
        (unpack $ show doResult) `shouldStartWith` "✅ Finished task"


    it "adds a taks and deletes it" $ do
      _ <- addTask connection ["Just a test"]
      result <- nextTask connection
      let ulidText = getUlidFromBody result

      deleteResult <- deleteTasks connection [ulidText]
      (unpack $ show deleteResult) `shouldStartWith` "❌ Deleted task"


    context "When a task was logged" $ do

      it "logs a task" $ do
        result <- logTask connection ["Just a test"]
        (unpack $ show result) `shouldStartWith`
          "📝 Logged task \"Just a test\" with ulid"


main :: IO ()
main = do
  withSystemTempFile "main.db" $ \filePath _ -> do
    connection <- setupTestConnection filePath
    hspec $ testSuite connection

  -- | Do not delete database after tests for debugging
  -- filePath <- emptySystemTempFile "main.db"
  -- putText ""
  -- print filePath
  -- connection <- setupTestConnection filePath
  -- hspec $ testSuite connection
