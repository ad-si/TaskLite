module LibSpec where

import Protolude (
  Bool (True),
  Maybe (..),
  Text,
  pure,
  show,
  ($),
  (&),
  (<),
  (<>),
 )
import Protolude qualified as P

import Data.Hourglass (DateTime)
import Data.List.Utils (subIndex)
import Data.Text qualified as T
import Test.Hspec (
  Spec,
  describe,
  it,
  shouldBe,
  shouldContain,
  shouldEndWith,
  shouldNotContain,
 )

import Config (defaultConfig)
import ImportExport (PreEdit (ApplyPreEdit), editTaskByTask)
import Lib (
  addTag,
  countTasks,
  deleteNote,
  infoTask,
  insertRecord,
  insertTags,
  listNotes,
  newTasks,
 )
import Task (Task (body, closed_utc, state, ulid), TaskState (Done), zeroTask)
import TaskToNote (TaskToNote (TaskToNote, ulid))
import TaskToNote qualified
import TestUtils (withMemoryDb)


task1 :: Task
task1 =
  zeroTask
    { Task.ulid = "01hs68z7mdg4ktpxbv0yfafznq"
    , Task.body = "New task 1"
    }


taskMultiLine :: Task
taskMultiLine =
  zeroTask
    { Task.ulid = "01hx48cnjhp18mts3c44zk3gen"
    , Task.body =
        "New task\n\
        \with several lines\n\
        \and line breaks"
    }


spec :: DateTime -> Spec
spec now = do
  describe "Lib" $ do
    it "counts tasks" $ do
      withMemoryDb defaultConfig $ \memConn -> do
        let
          task2 =
            zeroTask
              { Task.ulid = "01hs690f9hkzk9z7zews9j2k1d"
              , Task.body = "New task 2"
              }

        count0 <- countTasks defaultConfig memConn P.mempty
        show count0 `shouldBe` ("0" :: Text)

        insertRecord "tasks" memConn task1
        count1 <- countTasks defaultConfig memConn P.mempty
        show count1 `shouldBe` ("1" :: Text)

        insertRecord "tasks" memConn task2
        count2 <- countTasks defaultConfig memConn P.mempty
        show count2 `shouldBe` ("2" :: Text)

        warnings <- insertTags memConn Nothing task2 ["test"]
        P.show warnings `shouldBe` T.empty
        countWithTag <- countTasks defaultConfig memConn (Just ["+test"])
        show countWithTag `shouldBe` ("1" :: Text)

        pure ()

    it "gets new tasks" $ do
      withMemoryDb defaultConfig $ \memConn -> do
        let
          task2 =
            zeroTask
              { Task.ulid = "01hs6zsf3c0vqx6egfnmbqtmvy"
              , Task.body = "New task 2"
              , Task.closed_utc = Just "2024-04-10T18:54:10Z"
              , Task.state = Just Done
              }

        insertRecord "tasks" memConn task1
        insertRecord "tasks" memConn task2

        cliOutput <- newTasks defaultConfig now memConn (Just ["state:done"])
        show cliOutput `shouldContain` "New task 2"
        show cliOutput `shouldNotContain` "New task 1"

    it "shows warning if a tag is duplicated" $ do
      withMemoryDb defaultConfig $ \memConn -> do
        let newTag = "test"
        insertRecord "tasks" memConn task1
        warnings <- insertTags memConn Nothing task1 [newTag]
        P.show warnings `shouldBe` T.empty

        cliOutput <- addTag defaultConfig memConn newTag [task1.ulid]
        show cliOutput `shouldEndWith` "Tag \"test\" is already assigned"

    it "lets you edit a task and shows warning if a tag was duplicated" $ do
      withMemoryDb defaultConfig $ \memConn -> do
        let existTag = "existing-tag"
        insertRecord "tasks" memConn task1
        warnings <- insertTags memConn Nothing task1 [existTag]
        P.show warnings `shouldBe` T.empty

        cliOutput <-
          editTaskByTask
            (ApplyPreEdit (<> ("\ntags: " <> P.show [existTag, "new-tag"])))
            memConn
            task1
        let errMsg = "Tag \"" <> T.unpack existTag <> "\" is already assigned"
        show cliOutput `shouldContain` errMsg

    it "keeps line breaks of multi-line tasks in info view" $ do
      withMemoryDb defaultConfig $ \memConn -> do
        insertRecord "tasks" memConn taskMultiLine

        cliOutput <- infoTask defaultConfig memConn taskMultiLine.ulid
        show cliOutput
          `shouldContain` "New task\n\
                          \with several lines\n\
                          \and line breaks"

    it "lists all notes descending by creation UTC" $ do
      withMemoryDb defaultConfig $ \memConn -> do
        insertRecord "tasks" memConn task1
        let
          taskToNote1 =
            TaskToNote
              { TaskToNote.ulid = "01hx4eyxxvs5b75ynxrztcz87f"
              , TaskToNote.task_ulid = task1.ulid
              , TaskToNote.note = "The first note"
              }
          note1Id = taskToNote1.ulid & T.takeEnd 3 & T.unpack
          taskToNote2 =
            TaskToNote
              { TaskToNote.ulid = "01hx4f3f764sma7n8bahvwjeed"
              , TaskToNote.task_ulid = task1.ulid
              , TaskToNote.note = "The second note"
              }
          note2Id = taskToNote2.ulid & T.takeEnd 3 & T.unpack

        insertRecord "task_to_note" memConn taskToNote1
        insertRecord "task_to_note" memConn taskToNote2

        cliOutput <- listNotes defaultConfig memConn

        show cliOutput `shouldContain` note1Id
        show cliOutput `shouldContain` note2Id

        let
          posUlid1 = subIndex note1Id (show cliOutput)
          posUlid2 = subIndex note2Id (show cliOutput)

        --  Newer notes should be listed first
        (posUlid2 < posUlid1) `shouldBe` True

    it "lets you delete a note" $ do
      withMemoryDb defaultConfig $ \memConn -> do
        insertRecord "tasks" memConn task1
        let noteId = "01hwcqk9nnwjypzw9kr646nqce"
        insertRecord
          "task_to_note"
          memConn
          TaskToNote
            { TaskToNote.ulid = noteId
            , TaskToNote.task_ulid = task1.ulid
            , TaskToNote.note = "The note content"
            }

        cliOutput <- deleteNote defaultConfig memConn noteId

        (show cliOutput :: Text)
          `shouldBe` "\128165 Deleted note \"01hwcqk9nnwjypzw9kr646nqce\" \
                     \of task \"01hs68z7mdg4ktpxbv0yfafznq\""
