module TaskwarriorSpec where

import Protolude (
  Bool (..),
  Eq ((/=)),
  Maybe (..),
  ($),
  (&),
 )
import Protolude qualified as P

import Data.Aeson (Value (..), decode, encode)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Test.Hspec (
  Spec,
  describe,
  it,
  shouldBe,
  shouldSatisfy,
 )

import FullTask (emptyFullTask)
import FullTask qualified
import Note (Note (..))
import Task (TaskState (..))
import Taskwarrior (
  convertDateFormat,
  fullTaskToTwJson,
  priorityToTwPriority,
  taskStateToTwStatus,
  ulidToUuid,
 )


spec :: Spec
spec = do
  describe "Taskwarrior" $ do
    describe "ulidToUuid" $ do
      it "converts a ULID to UUID format" $ do
        -- Example ULID: 01GSKNQ51SADQ1RK2P3GM0ZA3V
        -- This should decode to a proper 128-bit hex representation
        let ulid = "01GSKNQ51SADQ1RK2P3GM0ZA3V"
            uuid = ulidToUuid ulid
        -- UUID format: 8-4-4-4-12 hex chars
        T.length uuid `shouldBe` 36
        T.index uuid 8 `shouldBe` '-'
        T.index uuid 13 `shouldBe` '-'
        T.index uuid 18 `shouldBe` '-'
        T.index uuid 23 `shouldBe` '-'

      it "converts a lowercase ULID to UUID format" $ do
        let ulid = "01gsknq51sadq1rk2p3gm0za3v"
            uuid = ulidToUuid ulid
        T.length uuid `shouldBe` 36

      it "produces consistent UUIDs for the same ULID" $ do
        let ulid = "01JCZSZ9C328FM7XYDCWXBMV6N"
        ulidToUuid ulid `shouldBe` ulidToUuid ulid

      it "produces different UUIDs for different ULIDs" $ do
        let ulid1 = "01JCZSZ9C328FM7XYDCWXBMV6N"
            ulid2 = "01JCZSZRA25EC48PAGZCW5QW6J"
        ulidToUuid ulid1 `shouldSatisfy` (/= ulidToUuid ulid2)

      it "returns fallback for invalid input" $ do
        let invalid = "not-a-ulid"
            uuid = ulidToUuid invalid
        T.length uuid `shouldBe` 36

    describe "convertDateFormat" $ do
      it "converts TaskLite date format to Taskwarrior format" $ do
        convertDateFormat "2024-03-15 10:32:51.386"
          `shouldBe` Just "20240315T103251Z"

      it "handles dates without milliseconds" $ do
        convertDateFormat "2024-03-15 10:32:51"
          `shouldBe` Just "20240315T103251Z"

      it "returns Nothing for invalid dates" $ do
        convertDateFormat "not-a-date" `shouldBe` Nothing

    describe "taskStateToTwStatus" $ do
      it "maps Done to completed" $ do
        taskStateToTwStatus (Just Done) Nothing `shouldBe` "completed"

      it "maps Obsolete to deleted" $ do
        taskStateToTwStatus (Just Obsolete) Nothing `shouldBe` "deleted"

      it "maps Deletable to deleted" $ do
        taskStateToTwStatus (Just Deletable) Nothing `shouldBe` "deleted"

      it "maps Nothing with closed_utc to completed" $ do
        taskStateToTwStatus Nothing (Just "2024-01-01") `shouldBe` "completed"

      it "maps Nothing without closed_utc to pending" $ do
        taskStateToTwStatus Nothing Nothing `shouldBe` "pending"

    describe "priorityToTwPriority" $ do
      it "maps high priority (> 2) to H" $ do
        priorityToTwPriority 3.0 `shouldBe` Just "H"
        priorityToTwPriority 5.0 `shouldBe` Just "H"

      it "maps medium priority (> 0, <= 2) to M" $ do
        priorityToTwPriority 1.0 `shouldBe` Just "M"
        priorityToTwPriority 2.0 `shouldBe` Just "M"

      it "maps low priority (< 0) to L" $ do
        priorityToTwPriority (-1.0) `shouldBe` Just "L"
        priorityToTwPriority (-5.0) `shouldBe` Just "L"

      it "maps zero priority to Nothing" $ do
        priorityToTwPriority 0.0 `shouldBe` Nothing

    describe "fullTaskToTwJson" $ do
      it "converts a basic task to Taskwarrior JSON" $ do
        let task =
              emptyFullTask
                { FullTask.ulid = "01JCZSZ9C328FM7XYDCWXBMV6N"
                , FullTask.body = "Buy milk"
                , FullTask.modified_utc = "2024-09-24 22:31:09.123"
                }
            twJson = fullTaskToTwJson task

        case twJson of
          Object obj -> do
            KeyMap.lookup "uuid" obj `shouldSatisfy` \case
              Just (String _) -> True
              _ -> False
            KeyMap.lookup "status" obj `shouldBe` Just (String "pending")
            KeyMap.lookup "description" obj `shouldBe` Just (String "Buy milk")
          _ -> P.die "Expected JSON object"

      it "includes tags when present" $ do
        let task =
              emptyFullTask
                { FullTask.ulid = "01JCZSZ9C328FM7XYDCWXBMV6N"
                , FullTask.body = "Task with tags"
                , FullTask.modified_utc = "2024-09-24 22:31:09.123"
                , FullTask.tags = Just ["work", "urgent"]
                }
            twJson = fullTaskToTwJson task
            jsonText = twJson & encode & TL.decodeUtf8 & TL.toStrict

        jsonText `shouldSatisfy` T.isInfixOf "\"tags\":"
        jsonText `shouldSatisfy` T.isInfixOf "work"
        jsonText `shouldSatisfy` T.isInfixOf "urgent"

      it "converts notes to annotations" $ do
        let task =
              emptyFullTask
                { FullTask.ulid = "01JCZSZ9C328FM7XYDCWXBMV6N"
                , FullTask.body = "Task with notes"
                , FullTask.modified_utc = "2024-09-24 22:31:09.123"
                , FullTask.notes =
                    Just
                      [ Note "01JCZSZRA25EC48PAGZCW5QW6J" "First note"
                      , Note "01JD000000000000000000000" "Second note"
                      ]
                }
            twJson = fullTaskToTwJson task
            jsonText = twJson & encode & TL.decodeUtf8 & TL.toStrict

        jsonText `shouldSatisfy` T.isInfixOf "\"annotations\":"
        jsonText `shouldSatisfy` T.isInfixOf "First note"
        jsonText `shouldSatisfy` T.isInfixOf "Second note"

      it "sets status to completed for Done tasks" $ do
        let task =
              emptyFullTask
                { FullTask.ulid = "01JCZSZ9C328FM7XYDCWXBMV6N"
                , FullTask.body = "Completed task"
                , FullTask.modified_utc = "2024-09-24 22:31:09.123"
                , FullTask.state = Just Done
                }
            twJson = fullTaskToTwJson task

        case twJson of
          Object obj ->
            KeyMap.lookup "status" obj `shouldBe` Just (String "completed")
          _ -> P.die "Expected JSON object"

      it "includes due date when present" $ do
        let task =
              emptyFullTask
                { FullTask.ulid = "01JCZSZ9C328FM7XYDCWXBMV6N"
                , FullTask.body = "Task with due date"
                , FullTask.modified_utc = "2024-09-24 22:31:09.123"
                , FullTask.due_utc = Just "2024-12-31 23:59:59.000"
                }
            twJson = fullTaskToTwJson task
            jsonText = twJson & encode & TL.decodeUtf8 & TL.toStrict

        jsonText `shouldSatisfy` T.isInfixOf "\"due\":"
        jsonText `shouldSatisfy` T.isInfixOf "20241231T235959Z"

      it "extracts project from metadata" $ do
        let task =
              emptyFullTask
                { FullTask.ulid = "01JCZSZ9C328FM7XYDCWXBMV6N"
                , FullTask.body = "Task with project"
                , FullTask.modified_utc = "2024-09-24 22:31:09.123"
                , FullTask.metadata =
                    decode "{\"project\": \"home-renovation\"}"
                }
            twJson = fullTaskToTwJson task
            jsonText = twJson & encode & TL.decodeUtf8 & TL.toStrict

        jsonText `shouldSatisfy` T.isInfixOf "\"project\":"
        jsonText `shouldSatisfy` T.isInfixOf "home-renovation"
