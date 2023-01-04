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
import Text.ParserCombinators.ReadP as ReadP
import Time.System

import Config (Config(..), defaultConfig)
import Lib
import Utils
import Migrations


base32Alphabet :: [Char]
base32Alphabet = "0123456789abcdefghjkmnpqrstvwxyz"


ulidParser :: ReadP [Char]
ulidParser = do
  _ <- many1 ReadP.get
  _ <- string "ULID: "
  ulid <- munch1 (\character -> P.any (character ==) base32Alphabet)
  _ <- manyTill ReadP.get eof
  return ulid


getUlidFromBody :: [Char] -> Maybe [Char]
getUlidFromBody body =
  case readP_to_S ulidParser body of
    [(ulid, "")] -> Just ulid
    _ -> Nothing


withUlidFromBody :: (Show a) => a -> (Text -> IO b) -> IO b
withUlidFromBody body test =
  case getUlidFromBody (show body) of
    Nothing -> throwIO $ userError "Body does not contain a ULID"
    Just ulidText -> test (T.pack ulidText)


-- | The tests build up upon each other
-- | and therefore the order must not be changed
testSuite :: Config -> DateTime -> Sql.Connection -> SpecWith ()
testSuite conf now connection = do
  describe "Utils" $ do
    it "correctly parses beginning of UNIX epoch" $ do
      (parseUlidUtcSection "0000000000")
      `shouldBe`
      (Just $ timeGetDateTimeOfDay $ Elapsed 0)

    it "correctly parses 36 ms after UNIX epoch" $ do
      (parseUlidUtcSection "0000000014")
      `shouldBe`
      (Just $ timeGetDateTimeOfDay $ ElapsedP 0 36000000)

    it "correctly parses a ULID string" $ do
      let ulidText = "0000000014T4R3JR7HMQNREEW8" :: Text

      (fmap show $ parseUlidText ulidText) `shouldBe` (Just ulidText)


  describe "TaskLite" $ do
    it "creates tables on initial run migrates tables to latest version" $ do
      migrationStatus <- runMigrations conf connection
      unpack (show migrationStatus) `shouldStartWith` "Migration succeeded"


    it "initially contains no tasks" $ do
      tasks <- headTasks conf now connection
      unpack (show tasks) `shouldStartWith` "No tasks available"


    it "adds a task" $ do
      result <- addTask conf connection ["Just a test"]
      unpack (show result) `shouldStartWith`
        "🆕 Added task \"Just a test\" with id"


    context "When a task exists" $ do
      it "lists next task" $ do
        result <- nextTask conf connection
        unpack (show result) `shouldContain` "Just a test"


      it "adds a tag" $ do
        result <- nextTask conf connection

        withUlidFromBody result $ \ulidText -> do
          tagResult <- addTag conf connection "test" [ulidText]
          unpack (show tagResult) `shouldStartWith`
              "🏷  Added tag \"test\" to task"


      it "adds a note" $ do
        result <- nextTask conf connection

        withUlidFromBody result $ \ulidText -> do
          tagResult <- addNote conf connection
                        "Just a test note" [ulidText]
          unpack (show tagResult) `shouldStartWith`
            "🗒  Added a note to task"


      it "sets due UTC" $ do
        resultTask <- nextTask conf connection

        withUlidFromBody resultTask $ \ulidText -> do
          case (parseUtc "2087-03-21 17:43") of
            Nothing -> throwIO $ userError "Invalid UTC string"
            Just utcStamp -> do
              result <- setDueUtc conf connection utcStamp [ulidText]
              unpack (show result) `shouldStartWith`
                "📅 Set due UTC of task \"Just a test\" with id"


      it "completes it" $ do
        result <- nextTask conf connection

        withUlidFromBody result $ \ulidText -> do
          doResult <- doTasks conf connection Nothing [ulidText]
          unpack (show doResult) `shouldStartWith` "✅ Finished task"


    it "adds a task with metadata and deletes it" $ do
      _ <- addTask conf connection
            ["Just a test +tag due:2082-10-03 +what"]
      result <- nextTask conf connection

      withUlidFromBody result $ \ulidText -> do
        deleteResult <- deleteTasks conf connection [ulidText]
        unpack (show deleteResult) `shouldStartWith` "❌ Deleted task"


    it "logs a task" $ do
      result <- logTask conf connection ["Just a test"]
      unpack (show result) `shouldStartWith`
        "📝 Logged task \"Just a test\" with id"


    it "dies on invalid filter expressions" $ do
      (runFilter conf now connection [" "]) `shouldThrow` (== ExitFailure 1)


main :: IO ()
main = do
  withSystemTempFile "main.db" $ \filePath _ -> do
    connection <- Sql.open filePath
    nowElapsed <- timeCurrentP
    let now = timeFromElapsedP nowElapsed :: DateTime
    hspec $ testSuite defaultConfig now connection

  -- -- Does not delete database after tests for debugging
  -- filePath <- emptySystemTempFile "main.db"
  -- putStrLn $ "\nFilepath: " <> filePath
  -- connection <- Sql.open filePath
  -- nowElapsed <- timeCurrentP
  -- let now = timeFromElapsedP nowElapsed :: DateTime
  -- hspec $ testSuite defaultConfig now connection
