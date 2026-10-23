{-# LANGUAGE QuasiQuotes #-}

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

import Control.Arrow ((>>>))
import Data.Aeson (decode)
import Data.List.Utils (subIndex)
import Data.Text (unpack)
import Data.Text qualified as T
import Data.Time.ISO8601.Duration qualified as Iso
import Database.SQLite.Simple (
  NamedParam ((:=)),
  Only (Only),
  Query,
  SQLData (SQLNull),
  SQLError,
  executeNamed,
  query_,
 )
import Database.SQLite.Simple.QQ (sql)
import Test.Hspec (
  Spec,
  context,
  it,
  shouldBe,
  shouldContain,
  shouldEndWith,
  shouldMatchList,
  shouldNotBe,
  shouldNotContain,
  shouldNotSatisfy,
  shouldSatisfy,
  shouldStartWith,
  shouldThrow,
 )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (NonNegative (..))

import Comment (Comment)
import Config (
  Hook (Hook),
  HookSet (HookSet),
  defaultConfig,
 )
import Config qualified
import Data.Hourglass (Seconds (Seconds), TimeFormat (..), timeDiff, timePrint)
import FullTask (FullTask, emptyFullTask)
import FullTask qualified
import ImportExport (EditMode (ApplyPreEdit), editTaskByTask)
import Lib (
  addComment,
  addTag,
  addTask,
  blockTasks,
  countTasks,
  deleteComment,
  deleteTag,
  deleteTagDescription,
  deleteTasks,
  describeTag,
  doTasks,
  duplicateTasks,
  endTasks,
  headTasks,
  infoTask,
  insertRecord,
  insertTags,
  listComments,
  listTags,
  listUlids,
  logTask,
  newTasks,
  nextTask,
  recurTasks,
  repeatTasks,
  runFilter,
  setDueUtc,
  setReadyUtc,
  startTasks,
  stopTasks,
  trashTasks,
  unblockTasks,
  unrepeatTasks,
  updateTask,
  wasListTruncated,
 )
import Task (
  DerivedState (IsAsleep, IsAwake, IsOpen, IsReady),
  Task (
    awake_utc,
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
  getStateHierarchy,
 )
import TaskToComment (TaskToComment (TaskToComment, ulid))
import TaskToComment qualified
import TaskToTag (TaskToTag)
import TaskToTag qualified
import TaskToTask (TaskToTask)
import TaskToTask qualified
import TestUtils (withMemoryDb)
import Time.System (dateCurrent)
import Utils (parseUtc, ulidText2utc, zeroTime, zeroUlidTxt)


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


task2 :: Task
task2 =
  emptyTask
    { Task.ulid = "01hs68z7mdg4ktpxbv0yfafzr2"
    , Task.body = "New task 2"
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
      tasks <- headTasks conf now memConn Nothing
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

  context "Awake and ready UTC (like Taskwarrior's wait and scheduled)" $ do
    let
      mkTask ulid awakeUtc readyUtc =
        emptyTask
          { Task.ulid = ulid
          , Task.body = "Task " <> ulid
          , Task.awake_utc = awakeUtc
          , Task.ready_utc = readyUtc
          }
      asleepTask =
        mkTask "01hs68z7mdg4ktpxbv0yfafzr3" (Just "2999-01-01") (Just "2000-01-01")
      awakeTask =
        mkTask "01hs68z7mdg4ktpxbv0yfafzr4" (Just "2000-01-01") (Just "2999-01-01")
      readyTask =
        mkTask "01hs68z7mdg4ktpxbv0yfafzr5" (Just "2000-01-01") (Just "2000-01-02")
      plainTask =
        mkTask "01hs68z7mdg4ktpxbv0yfafzr6" Nothing Nothing

    it "derives the states" $ do
      let stateOf = getStateHierarchy now >>> P.snd
      stateOf asleepTask `shouldBe` IsAsleep
      stateOf awakeTask `shouldBe` IsAwake
      stateOf readyTask `shouldBe` IsReady
      stateOf plainTask `shouldBe` IsOpen
      stateOf (mkTask "" Nothing (Just "2999-01-01")) `shouldBe` IsAwake
      stateOf (mkTask "" (Just "2000-01-01") Nothing) `shouldBe` IsReady

    it "lists only awake tasks whose ready UTC passed as ready" $ do
      withMemoryDb conf $ \memConn -> do
        P.forM_ [asleepTask, awakeTask, readyTask, plainTask] $
          insertRecord "tasks" memConn
        (ulids :: [Only Text]) <-
          query_ memConn "SELECT ulid FROM tasks_ready ORDER BY ulid"
        ulids `shouldBe` [Only readyTask.ulid, Only plainTask.ulid]

  context "When tasks have the same priority" $ do
    let
      taskWithDue =
        emptyTask
          { Task.ulid = "01hs68z7mdg4ktpxbv0yfafzr1"
          , Task.body = "Task with due date"
          , Task.due_utc = Just "2082-10-03 00:00:00"
          }
      taskWithoutDue =
        emptyTask
          { Task.ulid = "01hs68z7mdg4ktpxbv0yfafzr2"
          , Task.body = "Task without due date"
          }

    it "sorts tasks without due date last in views" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn taskWithDue
        insertRecord "tasks" memConn taskWithoutDue
        let viewNames :: [Query] = ["tasks_open", "tasks_ready", "tasks_notag"]
        P.forM_ viewNames $ \viewName -> do
          (ulids :: [Only Text]) <-
            query_ memConn $ "SELECT ulid FROM " <> viewName
          ulids `shouldBe` [Only taskWithDue.ulid, Only taskWithoutDue.ulid]

    it "lists tasks without due date last" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn taskWithDue
        insertRecord "tasks" memConn taskWithoutDue
        result <- headTasks conf now memConn Nothing
        T.breakOn "Task without due date" (show result)
          & P.fst
          & (`shouldSatisfy` T.isInfixOf "Task with due date")

    it "lists task with due date as next task" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn taskWithDue
        insertRecord "tasks" memConn taskWithoutDue
        result <- nextTask conf memConn
        unpack (show result) `shouldContain` "Task with due date"

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

    it "describes a tag" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn exampleTask
        _ <- addTag conf memConn "test" [exampleTask.ulid]
        descResult <- describeTag conf memConn "+test" "  A test tag "
        unpack (show descResult)
          `shouldBe` "📝 Set description of tag \"test\""
        descs :: [(P.Text, P.Text)] <-
          query_ memConn "SELECT tag, description FROM tags"
        descs `shouldBe` [("test", "A test tag")]
        tagsResult <- listTags conf memConn
        unpack (show tagsResult) `shouldContain` "Description"
        unpack (show tagsResult) `shouldContain` "A test tag"

    it "replaces the description of a tag" $ do
      withMemoryDb conf $ \memConn -> do
        _ <- describeTag conf memConn "test" "Old"
        _ <- describeTag conf memConn "test" "New"
        descs :: [[P.Text]] <-
          query_ memConn "SELECT description FROM tags"
        descs `shouldBe` [["New"]]

    it "warns when describing an unused tag" $ do
      withMemoryDb conf $ \memConn -> do
        descResult <- describeTag conf memConn "unused" "Some description"
        unpack (show descResult) `shouldContain` "not assigned to any task"

    it "rejects an empty tag description" $ do
      withMemoryDb conf $ \memConn -> do
        descResult <- describeTag conf memConn "test" "  "
        unpack (show descResult) `shouldContain` "must not be empty"
        descs :: [[P.Text]] <- query_ memConn "SELECT tag FROM tags"
        descs `shouldBe` []

    it "deletes the description of a tag" $ do
      withMemoryDb conf $ \memConn -> do
        _ <- describeTag conf memConn "test" "A test tag"
        delResult <- deleteTagDescription conf memConn "test"
        unpack (show delResult)
          `shouldBe` "💥 Deleted description of tag \"test\""
        descs :: [[Maybe P.Text]] <-
          query_ memConn "SELECT description FROM tags"
        descs `shouldBe` [[Nothing]]
        delAgainResult <- deleteTagDescription conf memConn "test"
        unpack (show delAgainResult) `shouldContain` "has no description"

    it "blocks one task by another" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn task1
        insertRecord "tasks" memConn task2
        result <- blockTasks conf memConn task1.ulid task2.ulid
        unpack (show result) `shouldStartWith` "🔗 Linked task"
        rows :: [TaskToTask] <-
          query_ memConn "SELECT * FROM task_to_task"
        case rows of
          [row] -> do
            row.source_task_ulid `shouldBe` task1.ulid
            row.target_task_ulid `shouldBe` task2.ulid
            row.relation `shouldBe` "blocks"
          _ -> P.die "Expected exactly one task_to_task row"

    it "rejects self-blocking" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn task1
        result <- blockTasks conf memConn task1.ulid task1.ulid
        unpack (show result) `shouldContain` "invalid or already exists"
        rows :: [TaskToTask] <-
          query_ memConn "SELECT * FROM task_to_task"
        rows `shouldBe` []

    it "rejects duplicate blocks relations" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn task1
        insertRecord "tasks" memConn task2
        _ <- blockTasks conf memConn task1.ulid task2.ulid
        result <- blockTasks conf memConn task1.ulid task2.ulid
        unpack (show result) `shouldContain` "invalid or already exists"
        rows :: [TaskToTask] <-
          query_ memConn "SELECT * FROM task_to_task"
        P.length rows `shouldBe` 1

    it "unblocks two tasks" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn task1
        insertRecord "tasks" memConn task2
        _ <- blockTasks conf memConn task1.ulid task2.ulid
        result <- unblockTasks conf memConn task1.ulid task2.ulid
        unpack (show result) `shouldStartWith` "💥 Removed"
        rows :: [TaskToTask] <-
          query_ memConn "SELECT * FROM task_to_task"
        rows `shouldBe` []

    it "warns when unblocking a non-existent relation" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn task1
        insertRecord "tasks" memConn task2
        result <- unblockTasks conf memConn task1.ulid task2.ulid
        unpack (show result) `shouldContain` "does not exist"

    it "parses blocks: token when adding a task" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn task1
        _ <- addTask conf memConn ["Child", "blocks:" <> task1.ulid]
        rows :: [TaskToTask] <-
          query_ memConn "SELECT * FROM task_to_task"
        case rows of
          [row] -> do
            row.target_task_ulid `shouldBe` task1.ulid
            row.relation `shouldBe` "blocks"
            row.source_task_ulid `shouldNotBe` task1.ulid
          _ -> P.die "Expected exactly one task_to_task row"

    it "rejects malformed ULIDs at the SQL layer" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn task1
        insertRecord "tasks" memConn task2
        let
          insertRow :: Text -> Text -> P.IO ()
          insertRow rowUlid relation =
            executeNamed
              memConn
              [sql|
                INSERT INTO task_to_task
                  (ulid, source_task_ulid, target_task_ulid, relation)
                VALUES (:ulid, :src, :tgt, :rel)
              |]
              [ ":ulid" := rowUlid
              , ":src" := task1.ulid
              , ":tgt" := task2.ulid
              , ":rel" := relation
              ]
        -- Uppercase letters and Crockford-excluded chars (i/l/o/u)
        -- must be rejected.
        insertRow "01HS68Z7MDG4KTPXBV0YFAFZNI" "blocks"
          `shouldThrow` (\(_ :: SQLError) -> True)
        -- Wrong length must be rejected.
        insertRow "01hs68z7mdg4ktpxbv0yfafzn" "blocks"
          `shouldThrow` (\(_ :: SQLError) -> True)
        -- Empty relation must be rejected.
        insertRow "01hs68z7mdg4ktpxbv0yfafzns" ""
          `shouldThrow` (\(_ :: SQLError) -> True)
        rows :: [TaskToTask] <-
          query_ memConn "SELECT * FROM task_to_task"
        rows `shouldBe` []

    it "parses blocked-by: token when adding a task" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn task1
        _ <- addTask conf memConn ["Parent", "blocked-by:" <> task1.ulid]
        rows :: [TaskToTask] <-
          query_ memConn "SELECT * FROM task_to_task"
        case rows of
          [row] -> do
            row.source_task_ulid `shouldBe` task1.ulid
            row.relation `shouldBe` "blocks"
            row.target_task_ulid `shouldNotBe` task1.ulid
          _ -> P.die "Expected exactly one task_to_task row"

    it "adds a comment" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn exampleTask
        commentResult <- addComment conf memConn "A test comment" [exampleTask.ulid]
        unpack (show commentResult)
          `shouldStartWith` "🗒  Added a comment to task"

    let postModifyLuaHook =
          Hook
            { Config.filePath = Nothing
            , Config.interpreter = "lua"
            , Config.body = "print('{\"message\":\"post-modify ran\"}')"
            }
        confWithPostModify =
          conf
            { Config.hooks =
                conf.hooks
                  { Config.modify =
                      HookSet
                        { pre = []
                        , post = [postModifyLuaHook]
                        }
                  }
            }

    it "fires post-modify hook on `start`" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn exampleTask
        result <-
          startTasks confWithPostModify memConn [exampleTask.ulid]
        unpack (show result) `shouldContain` "post-modify ran"

    it "fires post-modify hook on `stop`" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn exampleTask
        result <-
          stopTasks confWithPostModify memConn [exampleTask.ulid]
        unpack (show result) `shouldContain` "post-modify ran"

    it "fires post-modify hook on `do`" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn exampleTask
        result <-
          doTasks confWithPostModify memConn Nothing Nothing [exampleTask.ulid]
        unpack (show result) `shouldContain` "post-modify ran"

    it "fires post-modify hook on `tag`" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn exampleTask
        result <-
          addTag confWithPostModify memConn "test" [exampleTask.ulid]
        unpack (show result) `shouldContain` "post-modify ran"

    -- it "correctly wraps multiline comments" $ do
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
        doResult <- doTasks conf memConn Nothing Nothing [exampleTask.ulid]
        unpack (show doResult) `shouldStartWith` "✅ Finished task"
        tasks :: [Task] <- query_ memConn "SELECT * FROM tasks"
        case tasks of
          [updatedTask] -> do
            updatedTask `shouldSatisfy` (\task -> task.state == Just Done)
            updatedTask `shouldSatisfy` (\task -> isJust task.closed_utc)
          _ -> P.die "Found more than one task"

    it "completes it with an explicit closed UTC via --utc" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn exampleTask
        let utcTxt = "2020-01-15 08:30:00"
        case parseUtc utcTxt of
          Nothing -> P.die "Invalid UTC string"
          Just utcStamp -> do
            doResult <-
              doTasks conf memConn (Just utcStamp) Nothing [exampleTask.ulid]
            unpack (show doResult) `shouldStartWith` "✅ Finished task"
            tasks :: [Task] <- query_ memConn "SELECT * FROM tasks"
            case tasks of
              [updatedTask] -> do
                updatedTask.state `shouldBe` Just Done
                updatedTask.closed_utc `shouldBe` Just "2020-01-15 08:30:00"
              _ -> P.die "Found more than one task"

    it "dates the closing comment with the explicit closed UTC via --utc" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn exampleTask
        case parseUtc "2020-01-15 08:30:00" of
          Nothing -> P.die "Invalid UTC string"
          Just utcStamp -> do
            _ <-
              doTasks
                conf
                memConn
                (Just utcStamp)
                (Just ["Closing", "comment"])
                [exampleTask.ulid]
            taskToComments :: [TaskToComment] <-
              query_ memConn "SELECT * FROM task_to_comment"
            case taskToComments of
              [taskToComment] -> do
                taskToComment.comment `shouldBe` "Closing comment"
                ulidText2utc taskToComment.ulid
                  `shouldBe` Just "2020-01-15 08:30:00.000"
              _ -> P.die "Expected exactly one comment"

    it "dates the closing comment of an obsolete task with the explicit UTC" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn exampleTask
        case parseUtc "2020-01-15 08:30:00" of
          Nothing -> P.die "Invalid UTC string"
          Just utcStamp -> do
            _ <-
              endTasks
                conf
                memConn
                (Just utcStamp)
                (Just ["Not", "needed"])
                [exampleTask.ulid]
            taskToComments :: [TaskToComment] <-
              query_ memConn "SELECT * FROM task_to_comment"
            case taskToComments of
              [taskToComment] -> do
                taskToComment.comment `shouldBe` "Not needed"
                ulidText2utc taskToComment.ulid
                  `shouldBe` Just "2020-01-15 08:30:00.000"
              _ -> P.die "Expected exactly one comment"

    it "refuses to complete a task with open blockers" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn task1
        insertRecord "tasks" memConn task2
        _ <- blockTasks conf memConn task1.ulid task2.ulid
        result <- doTasks conf memConn Nothing Nothing [task2.ulid]
        unpack (show result) `shouldContain` "is blocked by"
        -- The blocked task must remain open.
        tasks :: [Task] <-
          query_ memConn "SELECT * FROM tasks WHERE state IS NOT NULL"
        tasks `shouldBe` []

    it "refuses to end (obsolete) a task with open blockers" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn task1
        insertRecord "tasks" memConn task2
        _ <- blockTasks conf memConn task1.ulid task2.ulid
        result <- endTasks conf memConn Nothing Nothing [task2.ulid]
        unpack (show result) `shouldContain` "is blocked by"

    it "refuses to trash a task with open blockers" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn task1
        insertRecord "tasks" memConn task2
        _ <- blockTasks conf memConn task1.ulid task2.ulid
        result <- trashTasks conf memConn [task2.ulid]
        unpack (show result) `shouldContain` "is blocked by"

    it "allows completing a blocked task once its blocker is closed" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn task1
        insertRecord "tasks" memConn task2
        _ <- blockTasks conf memConn task1.ulid task2.ulid
        -- Close the blocker first.
        blockerResult <- doTasks conf memConn Nothing Nothing [task1.ulid]
        unpack (show blockerResult) `shouldStartWith` "✅ Finished task"
        -- Now the originally-blocked task can be closed.
        targetResult <- doTasks conf memConn Nothing Nothing [task2.ulid]
        unpack (show targetResult) `shouldStartWith` "✅ Finished task"

    it "deletes it" $ do
      withMemoryDb conf $ \memConn -> do
        insertRecord "tasks" memConn exampleTask
        deleteResult <- deleteTasks conf memConn [exampleTask.ulid]
        unpack (show deleteResult) `shouldStartWith` "❌ Deleted task"
        tasks :: [Task] <- query_ memConn "SELECT * FROM tasks"
        tasks `shouldBe` []
        tags :: [TaskToTag] <- query_ memConn "SELECT * FROM task_to_tag"
        tags `shouldBe` []
        comments :: [Comment] <- query_ memConn "SELECT * FROM task_to_comment"
        comments `shouldBe` []

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
            , FullTask.created_utc = Nothing
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

  it "adds a task with awake, ready, and due date" $ do
    withMemoryDb conf $ \memConn -> do
      _ <-
        addTask
          conf
          memConn
          [ "Mow the lawn awake:2082-09-26 +garden \
            \ready:2082-09-30 due:2082-10-03"
          ]
      (tasks :: [Task]) <- query_ memConn "SELECT * FROM tasks"
      case tasks of
        [task] -> do
          task.body `shouldBe` "Mow the lawn"
          task.awake_utc `shouldBe` Just "2082-09-26 00:00:00"
          task.ready_utc `shouldBe` Just "2082-09-30 00:00:00"
          task.due_utc `shouldBe` Just "2082-10-03 00:00:00"
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
            , FullTask.created_utc = Nothing
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
      runFilter conf now memConn [" "] Nothing `shouldThrow` (== ExitFailure 1)

  it "counts tasks" $ do
    withMemoryDb defaultConfig $ \memConn -> do
      count0 <- countTasks defaultConfig memConn P.mempty
      show count0 `shouldBe` ("0" :: Text)

      insertRecord "tasks" memConn task1
      count1 <- countTasks defaultConfig memConn P.mempty
      show count1 `shouldBe` ("1" :: Text)

      insertRecord "tasks" memConn task2
      count2 <- countTasks defaultConfig memConn P.mempty
      show count2 `shouldBe` ("2" :: Text)

      warnings <- insertTags conf memConn Nothing task2 ["test"]
      P.show warnings `shouldBe` T.empty
      countWithTag <- countTasks defaultConfig memConn (Just ["+test"])
      show countWithTag `shouldBe` ("1" :: Text)

      pure ()

  it "lists ULIDs of tasks" $ do
    withMemoryDb defaultConfig $ \memConn -> do
      insertRecord "tasks" memConn task1
      insertRecord "tasks" memConn task2
      warnings <- insertTags conf memConn Nothing task2 ["test"]
      P.show warnings `shouldBe` T.empty

      allUlids <- listUlids defaultConfig memConn P.mempty
      T.lines (show allUlids)
        `shouldMatchList` [task1.ulid, task2.ulid]

      ulidsWithTag <- listUlids defaultConfig memConn (Just ["+test"])
      show ulidsWithTag `shouldBe` task2.ulid

      listUlids defaultConfig memConn (Just ["invalid"])
        `shouldThrow` (== ExitFailure 1)

  it "filters tasks by creation date" $ do
    withMemoryDb defaultConfig $ \memConn -> do
      insertRecord "tasks" memConn exampleTask -- 2024-02-21 16:43:17.358
      insertRecord "tasks" memConn task1 -- 2024-03-17 13:17:44.461
      let getCreated utc = listUlids defaultConfig memConn (Just ["created:" <> utc])

      onDay <- getCreated "2024-02-21"
      show onDay `shouldBe` exampleTask.ulid

      onMinute <- getCreated "2024-03-17T13:17"
      show onMinute `shouldBe` task1.ulid

      onSecond <- getCreated "2024-02-21T16:43:17"
      show onSecond `shouldBe` exampleTask.ulid

      onMilli <- getCreated "2024-02-21T16:43:17.358"
      show onMilli `shouldBe` exampleTask.ulid

      nextMilli <- getCreated "2024-02-21T16:43:17.359"
      show nextMilli `shouldBe` ("" :: Text)

      otherDay <- getCreated "2024-02-22"
      show otherDay `shouldBe` ("" :: Text)

      getCreated "not-a-date" `shouldThrow` (== ExitFailure 1)

  it "gets new tasks" $ do
    withMemoryDb defaultConfig $ \memConn -> do
      let
        doneTask =
          task2
            { Task.closed_utc = Just "2024-04-10T18:54:10Z"
            , Task.state = Just Done
            }

      insertRecord "tasks" memConn task1
      insertRecord "tasks" memConn doneTask

      cliOutput <- newTasks defaultConfig now memConn (Just ["state:done"]) Nothing
      show cliOutput `shouldContain` "New task 2"
      show cliOutput `shouldNotContain` "New task 1"

  it "shows warning if a tag is duplicated" $ do
    withMemoryDb defaultConfig $ \memConn -> do
      let newTag = "test"
      insertRecord "tasks" memConn task1
      warnings <- insertTags conf memConn Nothing task1 [newTag]
      P.show warnings `shouldBe` T.empty

      cliOutput <- addTag defaultConfig memConn newTag [task1.ulid]
      show cliOutput `shouldEndWith` "Tag \"test\" is already assigned"

  it "repeats a task and prevents recurring same task" $ do
    withMemoryDb defaultConfig $ \memConn -> do
      insertRecord "tasks" memConn task1
      let oneWeek = Iso.DurationWeek (Iso.DurWeek 1)
      repeatRes <- repeatTasks defaultConfig memConn oneWeek [task1.ulid]
      P.show repeatRes `shouldContain` "Set repeat duration of task"

      recurRes <- recurTasks defaultConfig memConn oneWeek [task1.ulid]
      P.show recurRes `shouldContain` "is already in a repetition series"

      unrepeatRes <- unrepeatTasks defaultConfig memConn [task1.ulid]
      P.show unrepeatRes `shouldContain` "Removed repetition duration"

      recurRes2 <- recurTasks defaultConfig memConn oneWeek [task1.ulid]
      P.show recurRes2 `shouldContain` "Set recurrence duration of task"

      repeatRes2 <- repeatTasks defaultConfig memConn oneWeek [task1.ulid]
      P.show repeatRes2 `shouldContain` "is already in a recurrence series"

  it "lets the next task of a repetition series sleep until it's due" $ do
    withMemoryDb defaultConfig $ \memConn -> do
      insertRecord "tasks" memConn $
        task1{Task.repetition_duration = Just "P2W"}
      _ <- doTasks defaultConfig memConn Nothing Nothing [task1.ulid]
      openTasks :: [Task] <-
        query_ memConn "SELECT * FROM tasks WHERE closed_utc IS NULL"
      case openTasks of
        [nextTask] -> do
          nextTask.due_utc `shouldSatisfy` isJust
          nextTask.awake_utc `shouldBe` nextTask.due_utc
          nextTask.ready_utc `shouldBe` Nothing
        _ -> P.die "Expected exactly one open task"

  it "wakes up the next repetition when it's ready" $ do
    withMemoryDb defaultConfig $ \memConn -> do
      insertRecord "tasks" memConn $
        task1
          { Task.repetition_duration = Just "P2W"
          , Task.due_utc = Just "2020-01-10 12:00:00"
          , Task.ready_utc = Just "2020-01-09 12:00:00"
          }
      _ <- doTasks defaultConfig memConn Nothing Nothing [task1.ulid]
      openTasks :: [Task] <-
        query_ memConn "SELECT * FROM tasks WHERE closed_utc IS NULL"
      case openTasks of
        [nextTask] -> do
          nextTask.ready_utc `shouldSatisfy` isJust
          nextTask.awake_utc `shouldBe` nextTask.ready_utc
        _ -> P.die "Expected exactly one open task"

  it "keeps offsets of awake and ready UTC to due UTC for repetitions" $ do
    withMemoryDb defaultConfig $ \memConn -> do
      insertRecord "tasks" memConn $
        task1
          { Task.repetition_duration = Just "P2W"
          , Task.due_utc = Just "2020-01-10 12:00:00"
          , Task.awake_utc = Just "2020-01-08 12:00:00"
          , Task.ready_utc = Just "2020-01-09 12:00:00"
          }
      _ <- doTasks defaultConfig memConn Nothing Nothing [task1.ulid]
      openTasks :: [Task] <-
        query_ memConn "SELECT * FROM tasks WHERE closed_utc IS NULL"
      case openTasks of
        [nextTask] -> do
          let
            parse field = nextTask & field P.>>= parseUtc
            diffToDue field =
              P.liftA2 timeDiff (parse Task.due_utc) (parse field)
          diffToDue Task.awake_utc `shouldBe` Just (Seconds (2 P.* 86400))
          diffToDue Task.ready_utc `shouldBe` Just (Seconds 86400)
        _ -> P.die "Expected exactly one open task"

  context "Editing a task" $ do
    it "keeps existing tags without warnings when adding new ones" $ do
      withMemoryDb defaultConfig $ \memConn -> do
        insertRecord "tasks" memConn task1
        warnings <- insertTags conf memConn Nothing task1 ["existing-tag"]
        P.show warnings `shouldBe` T.empty
        cliOutput <-
          editTaskByTask
            conf
            ( ApplyPreEdit $
                replaceBs "- existing-tag\n" "- existing-tag\n- new-tag\n"
            )
            memConn
            task1
        show cliOutput `shouldNotContain` "already assigned"
        (tags :: [[P.Text]]) <-
          query_ memConn "SELECT tag FROM task_to_tag ORDER BY tag"
        tags `shouldBe` [["existing-tag"], ["new-tag"]]

    it "removes tags which were deleted" $ do
      withMemoryDb defaultConfig $ \memConn -> do
        insertRecord "tasks" memConn task1
        _ <- insertTags conf memConn Nothing task1 ["tag-a", "tag-b"]
        _ <-
          editTaskByTask
            conf
            (ApplyPreEdit $ replaceBs "- tag-a\n" "")
            memConn
            task1
        (tags :: [[P.Text]]) <- query_ memConn "SELECT tag FROM task_to_tag"
        tags `shouldBe` [["tag-b"]]

    it "updates edited comments in place" $ do
      withMemoryDb defaultConfig $ \memConn -> do
        insertRecord "tasks" memConn task1
        let commentUlid = "01hxsjgzmdx48yzk39v852razr"
        insertRecord
          "task_to_comment"
          memConn
          TaskToComment
            { TaskToComment.ulid = commentUlid
            , TaskToComment.task_ulid = task1.ulid
            , TaskToComment.comment = "Old comment"
            , TaskToComment.user = "john"
            }
        _ <-
          editTaskByTask
            conf
            (ApplyPreEdit $ replaceBs "body: Old comment" "body: New comment")
            memConn
            task1
        taskToComments :: [TaskToComment] <-
          query_ memConn "SELECT * FROM task_to_comment"
        taskToComments
          `shouldBe` [ TaskToComment
                         { TaskToComment.ulid = commentUlid
                         , TaskToComment.task_ulid = task1.ulid
                         , TaskToComment.comment = "New comment"
                         , TaskToComment.user = "john"
                         }
                     ]

    it "removes comments which were deleted" $ do
      withMemoryDb defaultConfig $ \memConn -> do
        insertRecord "tasks" memConn task1
        P.forM_
          [ ("01hxsjgzmdx48yzk39v852raza", "Comment A")
          , ("01hxsjgzmdx48yzk39v852razb", "Comment B")
          ]
          $ \(commentUlid, comment) ->
            insertRecord
              "task_to_comment"
              memConn
              TaskToComment
                { TaskToComment.ulid = commentUlid
                , TaskToComment.task_ulid = task1.ulid
                , TaskToComment.comment = comment
                , TaskToComment.user = "john"
                }
        _ <-
          editTaskByTask
            conf
            ( ApplyPreEdit $
                replaceBs
                  "- body: Comment A\n  ulid: 01hxsjgzmdx48yzk39v852raza\n  user: john\n"
                  ""
            )
            memConn
            task1
        (comments :: [[P.Text]]) <- query_ memConn "SELECT comment FROM task_to_comment"
        comments `shouldBe` [["Comment B"]]

    it "deletes the task if state is set to x" $ do
      withMemoryDb defaultConfig $ \memConn -> do
        insertRecord "tasks" memConn task1
        _ <- insertTags conf memConn Nothing task1 ["some-tag"]

        cliOutput <-
          editTaskByTask
            conf
            (ApplyPreEdit $ replaceBs "state: null" "state: x")
            memConn
            task1
        show cliOutput `shouldContain` "Deleted task"

        tasks :: [Task] <- query_ memConn "SELECT * FROM tasks"
        tasks `shouldBe` []
        (tags :: [[P.Text]]) <- query_ memConn "SELECT tag FROM task_to_tag"
        tags `shouldBe` []

    it "lets you change the closed_utc" $ do
      withMemoryDb defaultConfig $ \memConn -> do
        insertRecord "tasks" memConn task1

        cliOutput <-
          editTaskByTask
            conf
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
            task.closed_utc `shouldBe` Just "2024-05-08 10:04:00.000"
            task.state `shouldBe` Just Done
            now_ <- dateCurrent
            let today = now_ & timePrint (toFormat ("YYYY-MM-DD" :: [P.Char]))
            T.unpack task.modified_utc `shouldStartWith` today
          _ -> P.die "Found more than one task"

    it
      "sets closed_utc to now when setting state to Done without explicit closed_utc"
      $ do
        withMemoryDb defaultConfig $ \memConn -> do
          insertRecord "tasks" memConn task1

          now_ <- dateCurrent
          let today = now_ & timePrint (toFormat ("YYYY-MM-DD" :: [P.Char]))

          cliOutput <-
            editTaskByTask
              conf
              ( ApplyPreEdit $
                  replaceBs
                    "state: null"
                    "state: Done"
                    -- Comment: NOT changing closed_utc - it should be set by the SQL trigger
              )
              memConn
              task1
          show cliOutput `shouldContain` "Edited task"
          tasks :: [Task] <- query_ memConn "SELECT * FROM tasks"
          case tasks of
            [task] -> do
              task.state `shouldBe` Just Done
              -- closed_utc should be set to today (by the SQL trigger), not an old date
              case task.closed_utc of
                Nothing -> P.die "closed_utc should be set"
                Just closedUtc -> T.unpack closedUtc `shouldStartWith` today
            _ -> P.die "Found more than one task"

    it "preserves closed_utc when editing an already-closed task" $ do
      withMemoryDb defaultConfig $ \memConn -> do
        let closedTask =
              task1
                { Task.state = Just Done
                , Task.closed_utc = Just "2023-06-15 12:30:00.000"
                }
        insertRecord "tasks" memConn closedTask

        cliOutput <-
          editTaskByTask
            conf
            ( ApplyPreEdit $
                replaceBs
                  "tags: []"
                  "tags: [\"test-tag\"]"
            )
            memConn
            closedTask
        show cliOutput `shouldContain` "Edited task"
        tasks :: [Task] <- query_ memConn "SELECT * FROM tasks"
        case tasks of
          [task] -> do
            task.state `shouldBe` Just Done
            -- closed_utc should be preserved from the original task
            task.closed_utc `shouldBe` Just "2023-06-15 12:30:00.000"
          _ -> P.die "Found more than one task"

    it "lets you add comments" $ do
      withMemoryDb defaultConfig $ \memConn -> do
        insertRecord "tasks" memConn task1
        cliOutput <-
          editTaskByTask
            conf
            ( ApplyPreEdit $
                replaceBs
                  "\n...\n"
                  ("\ncomments: " <> P.show ["A short comment" :: Text] <> "\n...\n")
            )
            memConn
            task1
        show cliOutput `shouldStartWith` "✏️  Edited task \"New task 1\""

        taskToComments :: [TaskToComment] <-
          query_ memConn "SELECT * FROM task_to_comment"
        case taskToComments of
          [taskToComment] -> do
            taskToComment.ulid
              `shouldNotSatisfy` (\ulid -> zeroUlidTxt `T.isPrefixOf` ulid)
            taskToComment.comment `shouldBe` "A short comment"
          _ -> P.die "Found more than one task_to_tag row"

    it "de-duplicates comments" $ do
      withMemoryDb defaultConfig $ \memConn -> do
        insertRecord "tasks" memConn task1
        let comment = "A short comment"
        insertRecord
          "task_to_comment"
          memConn
          TaskToComment
            { TaskToComment.ulid = "01hxsjgzmdx48yzk39v852razr"
            , TaskToComment.task_ulid = task1.ulid
            , TaskToComment.comment = comment
            , TaskToComment.user = "john"
            }
        taskInfo <- infoTask defaultConfig memConn task1.ulid
        show taskInfo `shouldContain` T.unpack comment
        cliOutput <-
          editTaskByTask
            conf
            -- Append plain text comments to the existing ones
            ( ApplyPreEdit $
                replaceBs "\n...\n" $
                  "\n- " <> P.encodeUtf8 comment <> "\n- " <> P.encodeUtf8 comment <> "\n...\n"
            )
            memConn
            task1
        show cliOutput `shouldStartWith` "✏️  Edited task \"New task 1\""

        taskToComments :: [TaskToComment] <-
          query_ memConn "SELECT * FROM task_to_comment"
        case taskToComments of
          [taskToCommentA, taskToCommentB] -> do
            taskToCommentA.ulid
              `shouldNotSatisfy` (\ulid -> zeroUlidTxt `T.isPrefixOf` ulid)
            taskToCommentB.ulid
              `shouldNotSatisfy` (\ulid -> zeroUlidTxt `T.isPrefixOf` ulid)
            taskToCommentA.comment `shouldBe` "A short comment"
            taskToCommentB.comment `shouldBe` "A short comment"
          _ -> P.die "Found more than one task_to_tag row"

    it "lets you set metadata to null" $ do
      withMemoryDb defaultConfig $ \memConn -> do
        insertRecord "tasks" memConn task1{metadata = Just "{\"a\":\"b\"}"}
        cliOutput <-
          editTaskByTask
            conf
            (ApplyPreEdit $ replaceBs "\n...\n" "\nmetadata: null\n...\n")
            memConn
            task1
        show cliOutput `shouldStartWith` "✏️  Edited task \"New task 1\""
        tasks :: [[SQLData]] <- query_ memConn "SELECT metadata FROM tasks"
        case tasks of
          [[metadata]] -> do
            metadata `shouldBe` SQLNull
          _ -> P.die "Found more than one task"

  it "only shows first line of multi-line tasks in task listing" $ do
    withMemoryDb defaultConfig $ \memConn -> do
      insertRecord "tasks" memConn taskMultiLine

      tasks <- headTasks conf now memConn Nothing
      show tasks `shouldContain` "New task ▼"
      show tasks `shouldNotContain` "with several lines"

  it "keeps line breaks of multi-line tasks in info view" $ do
    withMemoryDb defaultConfig $ \memConn -> do
      insertRecord "tasks" memConn taskMultiLine

      cliOutput <- infoTask defaultConfig memConn taskMultiLine.ulid
      show cliOutput
        `shouldContain` "New task\n\
                        \with several lines\n\
                        \and line breaks"

  it "lists all comments descending by creation UTC" $ do
    withMemoryDb defaultConfig $ \memConn -> do
      insertRecord "tasks" memConn task1
      let
        taskToComment1 =
          TaskToComment
            { TaskToComment.ulid = "01hx4eyxxvs5b75ynxrztcz87f"
            , TaskToComment.task_ulid = task1.ulid
            , TaskToComment.comment = "The first comment"
            , TaskToComment.user = "john"
            }
        comment1Id = taskToComment1.ulid & T.takeEnd 2 & T.unpack
        taskToComment2 =
          TaskToComment
            { TaskToComment.ulid = "01hx4f3f764sma7n8bahvwjeed"
            , TaskToComment.task_ulid = task1.ulid
            , TaskToComment.comment = "The second comment"
            , TaskToComment.user = "john"
            }
        comment2Id = taskToComment2.ulid & T.takeEnd 2 & T.unpack

      insertRecord "task_to_comment" memConn taskToComment1
      insertRecord "task_to_comment" memConn taskToComment2

      cliOutput <- listComments defaultConfig memConn

      show cliOutput `shouldContain` comment1Id
      show cliOutput `shouldContain` comment2Id

      let
        posUlid1 = subIndex comment1Id (show cliOutput)
        posUlid2 = subIndex comment2Id (show cliOutput)

      --  Newer comments should be listed first
      (posUlid2 < posUlid1) `shouldBe` True

  it "lets you delete a comment" $ do
    withMemoryDb defaultConfig $ \memConn -> do
      insertRecord "tasks" memConn task1
      let commentId = "01hwcqk9nnwjypzw9kr646nqce"
      insertRecord
        "task_to_comment"
        memConn
        TaskToComment
          { TaskToComment.ulid = commentId
          , TaskToComment.task_ulid = task1.ulid
          , TaskToComment.comment = "The comment content"
          , TaskToComment.user = "john"
          }

      cliOutput <- deleteComment defaultConfig memConn commentId

      (show cliOutput :: Text)
        `shouldBe` "\128165 Deleted comment \"01hwcqk9nnwjypzw9kr646nqce\" \
                   \of task \"01hs68z7mdg4ktpxbv0yfafznq\""

  context "wasListTruncated" $ do
    prop "returns False when limit is Nothing" $
      \(list :: [P.Int]) ->
        wasListTruncated Nothing list `shouldBe` P.False

    prop "returns False when list is shorter than limit" $
      \(NonNegative len) (NonNegative extra) ->
        let limit = len P.+ extra P.+ 1
            list = P.replicate len ()
        in  wasListTruncated (Just limit) list `shouldBe` P.False

    prop "returns True when list length >= limit" $
      \(NonNegative limit) (NonNegative extra) ->
        let list = P.replicate (limit P.+ extra) ()
        in  wasListTruncated (Just limit) list `shouldBe` True

    it "returns True for zero limit with empty list" $
      wasListTruncated (Just 0) ([] :: [()]) `shouldBe` True
