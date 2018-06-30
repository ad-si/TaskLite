import Protolude

import Test.Hspec
import qualified Database.SQLite.Simple as Sql
import System.IO.Temp
import Lib


setupTestConnection :: IO Sql.Connection
setupTestConnection = do
  withSystemTempFile "main.db" $ \filePath _ -> do
    connection <- Sql.open filePath

    createTaskTable connection
    createTagsTable connection
    createNotesTable connection
    createTaskView connection

    pure connection


main :: IO ()
main = do
  connection <- setupTestConnection

  hspec $ do
    describe "TaskLite" $ do
      it "creates necessary tables on initial run" $ do
        tasks <- headTasks connection
        (show tasks) `shouldBe` ("No tasks available" :: Text)
