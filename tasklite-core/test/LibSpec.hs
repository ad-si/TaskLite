module LibSpec where

import Protolude (
  Bool (True),
  ExitCode (ExitFailure),
  Maybe (..),
  Text,
  isJust,
  pure,
  show,
  ($),
  (&),
  (/=),
  (<),
  (<>),
  (==),
 )
import Protolude qualified as P

import Data.Aeson (decode)
import Data.List.Utils (subIndex)
import Data.Text (unpack)
import Data.Text qualified as T
import Data.Time.ISO8601.Duration qualified as Iso
import Database.SQLite.Simple (SQLData (SQLNull), query_)
import Test.Hspec (
  Spec,
  context,
  it,
  shouldBe,
  shouldContain,
  shouldEndWith,
  shouldNotBe,
  shouldNotContain,
  shouldNotSatisfy,
  shouldSatisfy,
  shouldStartWith,
  shouldThrow,
 )

import Config (defaultConfig)
import Control.Arrow ((>>>))
import FullTask (FullTask, emptyFullTask)
import FullTask qualified
import ImportExport (EditMode (ApplyPreEdit), editTaskByTask)
import Lib (
  addNote,
  addTag,
  addTask,
  countTasks,
  deleteNote,
  deleteTag,
  deleteTasks,
  doTasks,
  duplicateTasks,
  headTasks,
  infoTask,
  insertRecord,
  insertTags,
  listNotes,
  logTask,
  newTasks,
  nextTask,
  repeatTasks,
  runFilter,
  setDueUtc,
  setReadyUtc,
  updateTask,
 )
import Note (Note)
import System.Hourglass (dateCurrent)
import Task (
  Task (
    body,
    closed_utc,
    due_utc,
    group_ulid,
    metadata,
    modified_utc,
    ready_utc,
    repetition_duration,
    state,
    ulid,
    user
  ),
  TaskState (Done),
  emptyTask,
 )
import TaskToNote (TaskToNote (TaskToNote, ulid))
import TaskToNote qualified
import TaskToTag (TaskToTag)
import TaskToTag qualified
import TestUtils (withMemoryDb)
import Utils (parseUtc, zeroTime, zeroUlidTxt)


exampleTask :: Task
exampleTask =
  emptyTask
    { Task.ulid = "01hq68smfe0r9entg3x4rb9441"
    , Task.body = "Buy milk"
    , Task.state = Nothing
    , Task.modified_utc = "2024-02-21 16:43:17"
    , Task.due_utc = Just "2025-07-08 10:22:56"
    , Task.user = "john_vdg1c2hz"
    , Task.metadata = "{\"source\":\"fridge\"}" & decode
    }


task1 :: Task
task1 =
  emptyTask
    { Task.ulid = "01hs68z7mdg4ktpxbv0yfafznq"
    , Task.body = "New task 1"
    , Task.modified_utc = "2024-03-17 13:17:44.461"
    }


taskMultiLine :: Task
taskMultiLine =
  emptyTask
    { Task.ulid = "01hx48cnjhp18mts3c44zk3gen"
    , Task.body =
        "New task\n\
        \with several lines\n\
        \and line breaks"
    }


normTask :: Task -> Task
normTask task =
  task
    { Task.ulid = ""
    , Task.modified_utc = ""
    , Task.due_utc = Nothing
    , Task.user = ""
    }


replaceBs :: P.ByteString -> P.ByteString -> P.ByteString -> P.ByteString
replaceBs needle replacement haystack = do
  haystack
    & P.decodeUtf8
    & T.replace (P.decodeUtf8 needle) (P.decodeUtf8 replacement)
    & P.encodeUtf8


spec :: Spec
spec = do
  let
    now = "2024-05-08 10:04:17" & parseUtc & P.fromMaybe zeroTime
    conf = defaultConfig

  it "initially contains no tasks" $ do
    withMemoryDb conf $ \memConn -> do
      tasks <- headTasks conf now memConn
      unpack (show tasks) `shouldStartWith` "No tasks available"

  it "inserts a task" $ do
    withMemoryDb conf $ \memConn -> do
      let task =
            emptyTask
              { Task.ulid = "01hrvhc0h1pncbczxym16642mm"
              , Task.body = "Directly inserted task"
              , Task.state = Just Done
              }

      insertRecord "tasks" memConn task
      tasks :: [Task] <- query_ memConn "SELECT * FROM tasks"
      tasks `shouldBe` [task]

  it "adds a new task" $ do
    withMemoryDb conf $ \memConn -> do
      result <- addTask conf memConn ["Just a test"]
      unpack (show result)
        `shouldStartWith` "🆕 Added task \"Just a test\" with id"

  context "When a task exists" $ do
    it "updates a task" $ do
      withMemoryDb conf $ \memConn -> do
        let initialTask =
              emptyTask
                { Task.ulid = "01hrvhdddfwsrnp6dd8h7tp8h4"
                , Task.body = "New task"
                , Task.state = Just Done
                }
            newTask = initialTask{body = "Updated task"}

        insertRecord "tasks" memConn initialTask
        updateTask memConn newTask
        tasks :: [Task] <- query_ memConn "SELECT * FROM tasks"

        case tasks of
          [updatedTask] -> do
            -- Task should have a different `modified_utc` value
            updatedTask `shouldSatisfy` (\task -> task.modified_utc /= "")
            updatedTask{modified_utc = ""} `shouldBe` newTask
          _ ->
            P.die "Found more than one task"

    it "lists next task" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn exampleTask
        result <- nextTask conf memConn
        unpack (show result) `shouldContain` "Buy milk"

    it "adds a tag" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn exampleTask
        tagResult <- addTag conf memConn "test" [exampleTask.ulid]
        unpack (show tagResult)
          `shouldStartWith` "🏷  Added tag \"test\" to task"
        taskToTags :: [TaskToTag] <-
          query_ memConn "SELECT * FROM task_to_tag"
        case taskToTags of
          [taskToTag] -> do
            taskToTag.ulid `shouldNotBe` ""
            taskToTag.task_ulid `shouldNotBe` ""
            taskToTag.tag `shouldBe` "test"
          _ -> P.die "More than one task_to_tag row found"

    it "strips leading + when adding a tag" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn exampleTask
        tagResult <- addTag conf memConn "+test" [exampleTask.ulid]
        unpack (show tagResult)
          `shouldStartWith` "🏷  Added tag \"test\" to task"
        taskToTags :: [TaskToTag] <-
          query_ memConn "SELECT * FROM task_to_tag"
        case taskToTags of
          [taskToTag] -> taskToTag.tag `shouldBe` "test"
          _ -> P.die "More than one task_to_tag row found"

    it "deletes a tag" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn exampleTask
        _ <- addTag conf memConn "test" [exampleTask.ulid]
        delResult <- deleteTag conf memConn "test" [exampleTask.ulid]
        unpack (show delResult)
          `shouldStartWith` "💥 Removed tag \"test\" of task"
        taskToTags :: [TaskToTag] <-
          query_ memConn "SELECT * FROM task_to_tag"
        taskToTags `shouldBe` []

    it "doesn't delete a tag that does not exist" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn exampleTask
        delResult <- deleteTag conf memConn "test" [exampleTask.ulid]
        unpack (show delResult) `shouldContain` "not set"

    it "adds a note" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn exampleTask
        noteResult <- addNote conf memConn "A test note" [exampleTask.ulid]
        unpack (show noteResult)
          `shouldStartWith` "🗒  Added a note to task"

    -- it "correctly wraps multiline notes" $ do
    --   See error:
    --   tl info x795vqf

    it "sets due UTC" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn exampleTask
        let utcTxt = "2087-03-21 17:43:00"
        case parseUtc utcTxt of
          Nothing -> P.die "Invalid UTC string"
          Just utcStamp -> do
            result <- setDueUtc conf memConn utcStamp [exampleTask.ulid]
            unpack (show result)
              `shouldStartWith` ( "📅 Set due UTC of task \""
                                    <> T.unpack exampleTask.body
                                    <> "\" with id \""
                                    <> T.unpack exampleTask.ulid
                                    <> "\" to \""
                                    <> T.unpack utcTxt
                                    <> "\""
                                )

    it "sets ready UTC" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn exampleTask
        let utcTxt = "2059-07-11 04:55:16"
        case parseUtc utcTxt of
          Nothing -> P.die "Invalid UTC string"
          Just utcStamp -> do
            result <- setReadyUtc conf memConn utcStamp [exampleTask.ulid]
            unpack (show result)
              `shouldStartWith` ( "📅 Set ready UTC of task \""
                                    <> T.unpack exampleTask.body
                                    <> "\" with id \""
                                    <> T.unpack exampleTask.ulid
                                    <> "\" to \""
                                    <> T.unpack utcTxt
                                    <> "\""
                                )
            tasks :: [Task] <- query_ memConn "SELECT * FROM tasks"
            case tasks of
              [updatedTask] -> do
                updatedTask `shouldSatisfy` (\task -> isJust task.ready_utc)
              _ -> P.die "Found more than one task"

    it "completes it" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn exampleTask
        doResult <- doTasks conf memConn Nothing [exampleTask.ulid]
        unpack (show doResult) `shouldStartWith` "✅ Finished task"
        tasks :: [Task] <- query_ memConn "SELECT * FROM tasks"
        case tasks of
          [updatedTask] -> do
            updatedTask `shouldSatisfy` (\task -> task.state == Just Done)
            updatedTask `shouldSatisfy` (\task -> isJust task.closed_utc)
          _ -> P.die "Found more than one task"

    it "deletes it" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn exampleTask
        deleteResult <- deleteTasks conf memConn [exampleTask.ulid]
        unpack (show deleteResult) `shouldStartWith` "❌ Deleted task"
        tasks :: [Task] <- query_ memConn "SELECT * FROM tasks"
        tasks `shouldBe` []
        tags :: [TaskToTag] <- query_ memConn "SELECT * FROM task_to_tag"
        tags `shouldBe` []
        notes :: [Note] <- query_ memConn "SELECT * FROM task_to_note"
        notes `shouldBe` []

    it "duplicates a task" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn exampleTask
        duplicationResult <- duplicateTasks conf memConn [exampleTask.ulid]
        unpack (show duplicationResult)
          `shouldStartWith` "👯  Created a duplicate of task \"Buy milk\""
        tasks :: [Task] <- query_ memConn "SELECT * FROM tasks"
        case tasks of
          [taskA, taskB] -> do
            normTask taskA `shouldBe` normTask exampleTask
            normTask taskB `shouldBe` normTask exampleTask
          _ -> P.die "Must have exactly two tasks"

    it "deletes obsolete fields on duplication" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn exampleTask
        let
          zeroDur = Iso.DurationWeek (Iso.DurWeek 0)
          oneMonth = "P1M" & Iso.parseDuration & P.fromRight zeroDur
        _ <- repeatTasks conf memConn oneMonth [exampleTask.ulid]
        _ <- duplicateTasks conf memConn [exampleTask.ulid]
        tasks :: [Task] <-
          query_ memConn "SELECT * FROM tasks ORDER BY ulid DESC"
        case tasks of
          [dupe, original] -> do
            original.group_ulid `shouldSatisfy` P.isJust
            original.repetition_duration `shouldSatisfy` P.isJust
            dupe.group_ulid `shouldSatisfy` P.isNothing
            dupe.repetition_duration `shouldSatisfy` P.isNothing
            dupe.user `shouldNotBe` original.user
            normTask dupe `shouldBe` normTask exampleTask
          _ -> P.die "Must have exactly two tasks"

  it "adds a task with tags and due date" $ do
    withMemoryDb conf $ \memConn -> do
      _ <- addTask conf memConn ["Just a test +tag due:2082-10-03 +what"]
      (tasks :: [FullTask]) <- query_ memConn "SELECT * FROM tasks_view"
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
              , FullTask.due_utc = Just "2082-10-03 00:00:00"
              , FullTask.priority = Just 2.0
              , FullTask.tags = Just ["tag", "what"]
              }
        _ -> P.die "Found more than one task"

  it "deduplicates tags when adding a task" $ do
    withMemoryDb conf $ \memConn -> do
      _ <- addTask conf memConn ["Buy milk +drink +drink"]
      (tasks :: [FullTask]) <- query_ memConn "SELECT * FROM tasks_view"
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
              { FullTask.body = "Buy milk"
              , FullTask.priority = Just 2.0
              , FullTask.tags = Just ["drink"]
              }
        _ -> P.die "Found more than one task"

  it "logs a task" $ do
    withMemoryDb conf $ \memConn -> do
      result <- logTask conf memConn ["Just a test"]
      unpack (show result)
        `shouldStartWith` "📝 Logged task \"Just a test\" with id"

  it "dies on invalid filter expressions" $ do
    withMemoryDb conf $ \memConn -> do
      runFilter conf now memConn [" "] `shouldThrow` (== ExitFailure 1)

  it "counts tasks" $ do
    withMemoryDb defaultConfig $ \memConn -> do
      let
        task2 =
          emptyTask
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
          emptyTask
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

  context "Editing a task" $ do
    it "shows warning if a tag was duplicated" $ do
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

    it "lets you change the closed_utc" $ do
      withMemoryDb defaultConfig $ \memConn -> do
        insertRecord "tasks" memConn task1

        cliOutput <-
          editTaskByTask
            ( ApplyPreEdit $
                replaceBs
                  "state: null"
                  "state: Done"
                  >>> replaceBs
                    "closed_utc: null"
                    "closed_utc: 2024-05-08 10:04"
            )
            memConn
            task1
        show cliOutput `shouldContain` "Edited task"
        tasks :: [Task] <- query_ memConn "SELECT * FROM tasks"
        case tasks of
          [task] -> do
            task.closed_utc `shouldBe` Just "2024-05-08 10:04:00"
            task.state `shouldBe` Just Done
            now_ <- dateCurrent
            let today = now_ & show & T.take 10
            task.modified_utc `shouldSatisfy` (today `T.isPrefixOf`)
          _ -> P.die "Found more than one task"

    it "lets you add notes" $ do
      withMemoryDb defaultConfig $ \memConn -> do
        insertRecord "tasks" memConn task1
        cliOutput <-
          editTaskByTask
            (ApplyPreEdit (<> ("\nnotes: " <> P.show ["A short note" :: Text])))
            memConn
            task1
        show cliOutput `shouldStartWith` "✏️  Edited task \"New task 1\""

        taskToNotes :: [TaskToNote] <-
          query_ memConn "SELECT * FROM task_to_note"
        case taskToNotes of
          [taskToNote] -> do
            taskToNote.ulid
              `shouldNotSatisfy` (\ulid -> zeroUlidTxt `T.isPrefixOf` ulid)
            taskToNote.note `shouldBe` "A short note"
          _ -> P.die "Found more than one task_to_tag row"

    it "de-duplicates notes" $ do
      withMemoryDb defaultConfig $ \memConn -> do
        insertRecord "tasks" memConn task1
        let note = "A short note"
        insertRecord
          "task_to_note"
          memConn
          TaskToNote
            { TaskToNote.ulid = "01hxsjgzmdx48yzk39v852razr"
            , TaskToNote.task_ulid = task1.ulid
            , TaskToNote.note = note
            }
        taskInfo <- infoTask defaultConfig memConn task1.ulid
        show taskInfo `shouldContain` T.unpack note
        cliOutput <-
          editTaskByTask
            ( ApplyPreEdit $
                replaceBs "notes: []" $
                  "notes: " <> P.show [note, note]
            )
            memConn
            task1
        show cliOutput `shouldStartWith` "✏️  Edited task \"New task 1\""

        taskToNotes :: [TaskToNote] <-
          query_ memConn "SELECT * FROM task_to_note"
        case taskToNotes of
          [taskToNoteA, taskToNoteB] -> do
            taskToNoteA.ulid
              `shouldNotSatisfy` (\ulid -> zeroUlidTxt `T.isPrefixOf` ulid)
            taskToNoteB.ulid
              `shouldNotSatisfy` (\ulid -> zeroUlidTxt `T.isPrefixOf` ulid)
            taskToNoteA.note `shouldBe` "A short note"
            taskToNoteB.note `shouldBe` "A short note"
          _ -> P.die "Found more than one task_to_tag row"

    it "lets you set metadata to null" $ do
      withMemoryDb defaultConfig $ \memConn -> do
        insertRecord "tasks" memConn task1{metadata = Just "{\"a\":\"b\"}"}
        cliOutput <-
          editTaskByTask
            (ApplyPreEdit (<> "\nmetadata: null"))
            memConn
            task1
        show cliOutput `shouldStartWith` "✏️  Edited task \"New task 1\""
        tasks :: [[SQLData]] <- query_ memConn "SELECT metadata FROM tasks"
        case tasks of
          [[metadata]] -> do
            metadata `shouldBe` SQLNull
          _ -> P.die "Found more than one task"

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
