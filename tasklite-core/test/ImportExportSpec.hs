module ImportExportSpec where

import Protolude (
  Either (Left, Right),
  Eq ((==)),
  Maybe (..),
  Text,
  show,
  ($),
  (&),
  (/=),
  (<&>),
  (<>),
 )
import Protolude qualified as P

import Data.Aeson (decode, eitherDecode, eitherDecodeStrictText)
import Data.Hourglass (timePrint, toFormat)
import Data.Text (unpack)
import Data.Text qualified as T
import Database.SQLite.Simple (query_)
import Test.Hspec (
  SpecWith,
  describe,
  it,
  shouldBe,
  shouldSatisfy,
  shouldStartWith,
 )

import Config (Config (..))
import FullTask (FullTask, emptyFullTask)
import FullTask qualified
import ImportExport (insertImportTask)
import TaskToNote (TaskToNote)
import TaskToNote qualified
import TestUtils (withMemoryDb)
import Utils (parseUtc, ulid2utc)


spec :: Config -> SpecWith ()
spec conf = do
  describe "Import & Export" $ do
    it "parses any sensible datetime string" $ do
      -- TODO: Maybe keep microseconds and nanoseconds
      -- , ("YYYY-MM-DDTH:MI:S.msusZ", "2024-03-15T22:20:05.637913Z")
      -- , ("YYYY-MM-DDTH:MI:S.msusnsZ", "2024-03-15T22:20:05.637913438Z")

      let dateMap :: [(Text, Text)] =
            [ ("YYYY-MM-DD", "2024-03-15")
            , ("YYYY-MM-DD H:MI", "2024-03-15 22:20")
            , ("YYYY-MM-DDTH:MIZ", "2024-03-15T22:20Z")
            , ("YYYY-MM-DD H:MI:S", "2024-03-15 22:20:05")
            , ("YYYY-MM-DDTH:MI:SZ", "2024-03-15T22:20:05Z")
            , ("YYYYMMDDTHMIS", "20240315T222005")
            , ("YYYY-MM-DDTH:MI:S.msZ", "2024-03-15T22:20:05.637Z")
            , ("YYYY-MM-DDTH:MI:S.msZ", "2024-03-15T22:20:05.637123Z")
            , ("YYYY-MM-DDTH:MI:S.msZ", "2024-03-15T22:20:05.637123456Z")
            ]

      P.forM_ dateMap $ \(formatTxt, utcTxt) -> do
        case parseUtc utcTxt of
          Nothing -> P.die "Invalid UTC string"
          Just utcStamp ->
            let timeFmt = formatTxt & T.unpack & toFormat
            in  (utcStamp & timePrint timeFmt)
                  `shouldBe` T.unpack
                    ( utcTxt
                        & T.replace "123" ""
                        & T.replace "456" ""
                    )

      let
        utcTxt = "2024-03-15T22:20:05.386777444Z"
        printFmt = "YYYY-MM-DDTH:MI:S.ms" & T.unpack & toFormat
        -- Truncates microseconds and nanoseconds
        expected = "2024-03-15T22:20:05.386"

      (utcTxt & parseUtc <&> timePrint printFmt) `shouldBe` Just expected

    it "imports a JSON task" $ do
      withMemoryDb conf $ \memConn -> do
        let jsonTask = "{\"body\":\"Just a test\", \"notes\":[\"A note\"]}"

        case eitherDecode jsonTask of
          Left error ->
            P.die $ "Error decoding JSON: " <> show error
          Right importTaskRecord -> do
            result <- insertImportTask memConn importTaskRecord

            unpack (show result)
              `shouldStartWith` "📥 Imported task \"Just a test\" with ulid "

            taskToNotes :: [TaskToNote] <-
              query_ memConn "SELECT * FROM task_to_note"
            case taskToNotes of
              [taskToNote] -> do
                taskToNote `shouldSatisfy` (\task -> task.ulid /= "")
                taskToNote `shouldSatisfy` (\task -> task.task_ulid /= "")
                taskToNote `shouldSatisfy` (\task -> task.note == "A note")
              _ -> P.die "More than one task_to_note row found"

            tasks :: [FullTask] <- query_ memConn "SELECT * FROM tasks_view"

            case tasks of
              [updatedTask] -> do
                updatedTask `shouldSatisfy` (\task -> task.ulid /= "")
                updatedTask `shouldSatisfy` (\task -> task.modified_utc /= "")
                updatedTask `shouldSatisfy` (\task -> task.user /= "")
                updatedTask
                  { FullTask.ulid = ""
                  , FullTask.modified_utc = ""
                  , FullTask.user = ""
                  }
                  `shouldBe` emptyFullTask
                    { FullTask.body = "Just a test"
                    , -- TODO: Fix after notes are returned as a JSON array
                      FullTask.notes = Just []
                    , FullTask.priority = Just 1.0
                    , FullTask.metadata = decode jsonTask
                    }
              _ -> P.die "More than one task found"

    it "imports a JSON task with an ISO8601 created_at field" $ do
      withMemoryDb conf $ \memConn -> do
        let
          utc = "2024-03-15T10:32:51.386777444Z"
          -- ULID only has millisecond precision:
          utcFromUlid = "2024-03-15 10:32:51.387"
          jsonTask =
            "{\"body\":\"Just a test\",\"created_at\":\"{{utc}}\"}"
              & T.replace "{{utc}}" utc

        case eitherDecodeStrictText jsonTask of
          Left error ->
            P.die $ "Error decoding JSON: " <> show error
          Right importTaskRecord -> do
            _ <- insertImportTask memConn importTaskRecord
            tasks :: [FullTask] <- query_ memConn "SELECT * FROM tasks_view"
            case tasks of
              [updatedTask] ->
                ulid2utc updatedTask.ulid `shouldBe` Just utcFromUlid
              _ -> P.die "More than one task found"
