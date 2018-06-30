import Protolude as P

import Test.Hspec
import qualified Database.SQLite.Simple as Sql
import Data.Text as T
import System.IO.Temp
import System.IO.Error
import Lib
import Utils


setupTestConnection :: FilePath -> IO Sql.Connection
setupTestConnection filePath = do
    connection <- Sql.open filePath

    createTaskTable connection
    createTagsTable connection
    createNotesTable connection
    createTaskView connection

    pure connection


testSuite :: Sql.Connection -> SpecWith ()
testSuite connection = do
  describe "TaskLite" $ do
    it "Creates necessary tables on initial run" $ do
      tasks <- headTasks connection
      (show tasks) `shouldBe` ("No tasks available" :: Text)


    it "Adds a task" $ do
      result <- addTask connection ["Just a test"]
      (unpack $ show result) `shouldStartWith`
        "🆕 Added task \"Just a test\" with ulid"


    context "When a task exists" $ do
      let
        taskStart = "state: Open\npriority: 0\nbody: Just a test\nulid: "
        getUlidFromBody = T.take 26 . T.drop (P.length taskStart) . pack . show


      it "Lists next task" $ do
        result <- nextTask connection
        (unpack $ show result) `shouldStartWith` taskStart


      it "Adds a tag" $ do
        result <- nextTask connection
        let ulidText = getUlidFromBody result

        -- it "Adds a tag to a task" $ do
        tagResult <- addTag connection "test" [ulidText]
        (unpack $ show tagResult) `shouldStartWith`
          "🏷  Added tag \"test\" to task"


      it "Set due UTC" $ do
        resultTask <- nextTask connection
        let ulidText = getUlidFromBody resultTask

        case (parseUtc "2087-03-21 17:43") of
          Nothing -> throwIO $ userError "Invalid UTC string"
          Just utcStamp -> do
            result <- setDueUtc connection utcStamp [ulidText]
            (unpack $ show result) `shouldStartWith`
              "📅 Set due UTC to \"2087-03-21 17:43:00\" of task"


      it "Completes it" $ do
        result <- nextTask connection
        let ulidText = (T.take 26 . T.drop (P.length taskStart) . pack)
              (show result)

        doResult <- doTask connection ulidText
        (unpack $ show doResult) `shouldStartWith` ("✅ Finished task")


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
