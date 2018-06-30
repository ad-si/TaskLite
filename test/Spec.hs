import Protolude as P

import Test.Hspec
import qualified Database.SQLite.Simple as Sql
import Data.Text as T
import System.IO.Temp
import Lib


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

    it "Lists next task and completes it" $ do
      result <- nextTask connection
      let taskString = unpack $ show result
      taskString `shouldStartWith`
        "state: Open\npriority: 0\nbody: Just a test\nulid: "

      let ulidText = (pack . P.drop 51 . P.take 26) taskString

      result <- doTask connection ulidText
      (unpack $ show result) `shouldStartWith`
        ("✅ Finished task")


main :: IO ()
main = do
  withSystemTempFile "main.db" $ \filePath _ -> do
    connection <- setupTestConnection filePath
    hspec $ testSuite connection
