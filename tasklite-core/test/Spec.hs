import Protolude (
  Either (Left, Right),
  Eq ((==)),
  ExitCode (ExitFailure),
  Functor (fmap),
  IO,
  Maybe (..),
  Text,
  isJust,
  show,
  ($),
  (&),
  (/=),
  (<>),
 )
import Protolude qualified as P

import Data.Aeson (decode, eitherDecode)
import Data.Hourglass (
  DateTime,
  Elapsed (Elapsed),
  ElapsedP (ElapsedP),
  Time (timeFromElapsedP),
  timeGetDateTimeOfDay,
 )
import Data.Text as T (unpack)
import Database.SQLite.Simple (query_)
import Database.SQLite.Simple qualified as Sql
import Test.Hspec (
  SpecWith,
  context,
  describe,
  hspec,
  it,
  pendingWith,
  shouldBe,
  shouldContain,
  shouldSatisfy,
  shouldStartWith,
  shouldThrow,
 )
import Time.System (timeCurrentP)

import Config (Config (..), defaultConfig)
import FullTask (FullTask, emptyFullTask)
import FullTask qualified
import ImportExport (insertImportTask)
import Lib (
  addNote,
  addTag,
  addTask,
  deleteTag,
  deleteTasks,
  doTasks,
  headTasks,
  insertTask,
  logTask,
  nextTask,
  runFilter,
  setDueUtc,
  setReadyUtc,
  updateTask,
 )
import Migrations (runMigrations)
import Note (Note)
import Task (
  Task (
    body,
    closed_utc,
    due_utc,
    metadata,
    modified_utc,
    ready_utc,
    state,
    ulid,
    user
  ),
  TaskState (Done),
  zeroTask,
 )
import TaskToNote (TaskToNote)
import TaskToNote qualified
import TaskToTag (TaskToTag)
import TaskToTag qualified
import Utils (parseUlidText, parseUlidUtcSection, parseUtc)


withMemoryDb :: Config -> (Sql.Connection -> IO a) -> IO a
withMemoryDb conf action =
  Sql.withConnection ":memory:" $ \memConn -> do
    _ <- runMigrations conf memConn
    action memConn


exampleTask :: Task
exampleTask =
  zeroTask
    { ulid = "01hq68smfe0r9entg3x4rb9441"
    , body = "Buy milk"
    , state = Nothing
    , modified_utc = "2024-02-21 16:43:17"
    , due_utc = Just "2025-07-08 10:22:56"
    , user = "john"
    , metadata = "{\"source\":\"fridge\"}" & decode
    }


testSuite :: Config -> DateTime -> SpecWith ()
testSuite conf now = do
  describe "Utils" $ do
    it "correctly parses beginning of UNIX epoch" $
      do
        parseUlidUtcSection "0000000000"
        `shouldBe` Just (timeGetDateTimeOfDay $ Elapsed 0)

    it "correctly parses 36 ms after UNIX epoch" $
      do
        parseUlidUtcSection "0000000014"
        `shouldBe` Just (timeGetDateTimeOfDay $ ElapsedP 0 36000000)

    it "correctly parses a ULID string" $ do
      let ulidText = "0000000014T4R3JR7HMQNREEW8" :: Text

      fmap show (parseUlidText ulidText) `shouldBe` Just ulidText

  describe "TaskLite" $ do
    it "creates tables on initial run migrates tables to latest version" $ do
      Sql.withConnection ":memory:" $ \memConn -> do
        migrationStatus <- runMigrations conf memConn
        unpack (show migrationStatus) `shouldStartWith` "Migration succeeded"

    it "initially contains no tasks" $ do
      withMemoryDb conf $ \memConn -> do
        tasks <- headTasks conf now memConn
        unpack (show tasks) `shouldStartWith` "No tasks available"

    it "inserts a task" $ do
      withMemoryDb conf $ \memConn -> do
        let task =
              zeroTask
                { ulid = "01hrvhc0h1pncbczxym16642mm"
                , body = "Directly inserted task"
                , state = Just Done
                }

        insertTask memConn task
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
                zeroTask
                  { ulid = "01hrvhdddfwsrnp6dd8h7tp8h4"
                  , body = "New task"
                  , state = Just Done
                  }
              newTask = initialTask{body = "Updated task"}

          insertTask memConn initialTask
          updateTask memConn newTask
          tasks :: [Task] <- query_ memConn "SELECT * FROM tasks"

          case tasks of
            [updatedTask] -> do
              -- Task should have a different `modified_utc` value
              updatedTask `shouldSatisfy` (\task -> task.modified_utc /= "")
              updatedTask{modified_utc = ""} `shouldBe` newTask
            _ ->
              P.die "More than one task found"

      it "lists next task" $ do
        withMemoryDb conf $ \memConn -> do
          insertTask memConn exampleTask
          result <- nextTask conf memConn
          unpack (show result) `shouldContain` "Buy milk"

      it "adds a tag" $ do
        withMemoryDb conf $ \memConn -> do
          insertTask memConn exampleTask
          tagResult <- addTag conf memConn "test" [exampleTask.ulid]
          unpack (show tagResult)
            `shouldStartWith` "🏷  Added tag \"test\" to task"
          taskToTags :: [TaskToTag] <-
            query_ memConn "SELECT * FROM task_to_tag"
          case taskToTags of
            [taskToTag] -> do
              taskToTag `shouldSatisfy` (\t -> t.ulid /= "")
              taskToTag `shouldSatisfy` (\t -> t.task_ulid /= "")
              taskToTag `shouldSatisfy` (\t -> t.tag == "test")
            _ -> P.die "More than one task_to_tag row found"

      it "deletes a tag" $ do
        withMemoryDb conf $ \memConn -> do
          insertTask memConn exampleTask
          _ <- addTag conf memConn "test" [exampleTask.ulid]
          delResult <- deleteTag conf memConn "test" [exampleTask.ulid]
          unpack (show delResult)
            `shouldStartWith` "💥 Removed tag \"test\" of task"
          taskToTags :: [TaskToTag] <-
            query_ memConn "SELECT * FROM task_to_tag"
          taskToTags `shouldBe` []

      it "adds a note" $ do
        withMemoryDb conf $ \memConn -> do
          insertTask memConn exampleTask
          noteResult <- addNote conf memConn "A test note" [exampleTask.ulid]
          unpack (show noteResult)
            `shouldStartWith` "🗒  Added a note to task"

      it "deletes a note" $ do
        withMemoryDb conf $ \memConn -> do
          insertTask memConn exampleTask
          _ <- addNote conf memConn "A test note" [exampleTask.ulid]
          -- delResult <- Note.deleteNote conf memConn ulid
          pendingWith "Implement deleteNote"

      it "sets due UTC" $ do
        withMemoryDb conf $ \memConn -> do
          insertTask memConn exampleTask
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
          insertTask memConn exampleTask
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
                _ -> P.die "More than one task found"

      it "completes it" $ do
        withMemoryDb conf $ \memConn -> do
          insertTask memConn exampleTask
          doResult <- doTasks conf memConn Nothing [exampleTask.ulid]
          unpack (show doResult) `shouldStartWith` "✅ Finished task"
          tasks :: [Task] <- query_ memConn "SELECT * FROM tasks"
          case tasks of
            [updatedTask] -> do
              updatedTask `shouldSatisfy` (\task -> task.state == Just Done)
              updatedTask `shouldSatisfy` (\task -> isJust task.closed_utc)
            _ -> P.die "More than one task found"

      it "deletes it" $ do
        withMemoryDb conf $ \memConn -> do
          insertTask memConn exampleTask
          deleteResult <- deleteTasks conf memConn [exampleTask.ulid]
          unpack (show deleteResult) `shouldStartWith` "❌ Deleted task"
          tasks :: [Task] <- query_ memConn "SELECT * FROM tasks"
          tasks `shouldBe` []
          tags :: [TaskToTag] <- query_ memConn "SELECT * FROM task_to_tag"
          tags `shouldBe` []
          notes :: [Note] <- query_ memConn "SELECT * FROM task_to_note"
          notes `shouldBe` []

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
          _ -> P.die "More than one task found"

    it "logs a task" $ do
      withMemoryDb conf $ \memConn -> do
        result <- logTask conf memConn ["Just a test"]
        unpack (show result)
          `shouldStartWith` "📝 Logged task \"Just a test\" with id"

    it "dies on invalid filter expressions" $ do
      withMemoryDb conf $ \memConn -> do
        runFilter conf now memConn [" "] `shouldThrow` (== ExitFailure 1)

    describe "Import & Export" $ do
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

              tasks :: [FullTask] <-
                query_ memConn "SELECT * FROM tasks_view"

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
                        FullTask.notes = Nothing
                      , FullTask.priority = Just 1.0
                      , FullTask.metadata = decode jsonTask
                      }
                _ -> P.die "More than one task found"


main :: IO ()
main = do
  nowElapsed <- timeCurrentP
  let now = timeFromElapsedP nowElapsed :: DateTime
  hspec $ testSuite defaultConfig now
