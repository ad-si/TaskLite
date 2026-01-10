module ImportExportSpec where

import Protolude (
  Either (Left, Right),
  Maybe (..),
  Text,
  fromMaybe,
  show,
  ($),
  (&),
  (<&>),
  (<>),
 )
import Protolude qualified as P

import Data.Aeson (Value (Object), decode, eitherDecode, eitherDecodeStrictText)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as BSL
import Data.Hourglass (timePrint, toFormat)
import Data.Text (unpack)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.ULID (ULID)
import Data.Yaml qualified as Yaml
import Database.SQLite.Simple (query_)
import Test.Hspec (
  Spec,
  describe,
  it,
  shouldBe,
  shouldNotBe,
  shouldStartWith,
 )

import Config (defaultConfig)
import FullTask (FullTask, emptyFullTask)
import FullTask qualified
import ImportExport (getNdjsonLines, insertImportTask)
import ImportTask (
  ImportTask (ImportTask, closedUtcWasExplicit, notes, tags, task),
  setMissingFields,
 )
import Note (Note (Note, body, ulid))
import Task (Task (body, modified_utc, ulid, user), emptyTask)
import TaskToNote (TaskToNote (TaskToNote))
import TaskToNote qualified
import TestUtils (withMemoryDb)
import Utils (emptyUlid, parseUtc, setDateTime, ulidText2utc, zeroTime)


spec :: Spec
spec = do
  let conf = defaultConfig

  describe "Import" $ do
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

    it "imports a JSON task and puts unused fields into metadata" $ do
      withMemoryDb conf $ \memConn -> do
        let
          jsonTask =
            "\
            \ { 'body': 'Just a test' \
            \ , 'utc': '2024-03-15T10:32:51.853Z' \
            \ , 'tags': ['one', 'two'] \
            \ , 'notes': ['first note', 'second note'] \
            \ , 'xxx': 'yyy' \
            \ } \
            \"
              & T.replace "'" "\""

        case eitherDecodeStrictText jsonTask of
          Left error ->
            P.die $ "Error decoding JSON: " <> show error
          Right importTaskRecord -> do
            _ <- insertImportTask conf memConn importTaskRecord
            tasks :: [FullTask] <- query_ memConn "SELECT * FROM tasks_view"
            case tasks of
              [insertedTask] -> do
                insertedTask.body `shouldBe` "Just a test"
                insertedTask.tags `shouldBe` Just ["one", "two"]
                insertedTask.metadata
                  `shouldBe` Just (Object $ KeyMap.fromList [("xxx", "yyy")])
              _ -> P.die "More than one task found"

            taskToNotes :: [TaskToNote] <-
              query_ memConn "SELECT * FROM task_to_note"
            taskToNotes
              `shouldBe` [ TaskToNote
                             { ulid = "01hs0tqwwd0006vtdjjey5vdae"
                             , task_ulid = "01hs0tqwwd0004yy9et9d4wksy"
                             , note = "first note"
                             }
                         , TaskToNote
                             { ulid = "01hs0tqwwd0003pynthn2xpd5s"
                             , task_ulid = "01hs0tqwwd0004yy9et9d4wksy"
                             , note = "second note"
                             }
                         ]
    it "imports a JSON task with notes" $ do
      withMemoryDb conf $ \memConn -> do
        let jsonTask = "{\"body\":\"Just a test\", \"notes\":[\"A note\"]}"

        case eitherDecode jsonTask of
          Left error ->
            P.die $ "Error decoding JSON: " <> show error
          Right importTaskRecord -> do
            result <- insertImportTask conf memConn importTaskRecord

            unpack (show result)
              `shouldStartWith` "📥 Imported task \"Just a test\" with ulid "

            taskToNotes :: [TaskToNote] <-
              query_ memConn "SELECT * FROM task_to_note"
            case taskToNotes of
              [taskToNote] -> do
                taskToNote.ulid `shouldNotBe` ""
                taskToNote.task_ulid `shouldNotBe` ""
                taskToNote.note `shouldBe` "A note"
              _ -> P.die "More than one task_to_note row found"

            tasks :: [FullTask] <- query_ memConn "SELECT * FROM tasks_view"

            case tasks of
              [updatedTask] -> do
                updatedTask.ulid `shouldNotBe` ""
                updatedTask.modified_utc `shouldNotBe` ""
                updatedTask.user `shouldNotBe` ""
                let updatedTaskNoteUlid = case updatedTask.notes of
                      Just [note] -> note.ulid
                      _ -> ""
                updatedTask
                  { FullTask.ulid = ""
                  , FullTask.created_utc = Nothing
                  , FullTask.modified_utc = ""
                  , FullTask.user = ""
                  }
                  `shouldBe` emptyFullTask
                    { FullTask.body = "Just a test"
                    , FullTask.notes =
                        Just [Note{ulid = updatedTaskNoteUlid, body = "A note"}]
                    , FullTask.priority = Just 1.0
                    , FullTask.metadata = case decode jsonTask of
                        Just (Object keyMap) ->
                          keyMap
                            & KeyMap.delete "body"
                            & KeyMap.delete "notes"
                            & \kMap ->
                              if KeyMap.null kMap
                                then Nothing
                                else Just $ Object kMap
                        _ -> Nothing
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
            _ <- insertImportTask conf memConn importTaskRecord
            tasks :: [FullTask] <- query_ memConn "SELECT * FROM tasks_view"
            case tasks of
              [insertedTask] ->
                ulidText2utc insertedTask.ulid `shouldBe` Just utcFromUlid
              _ -> P.die "More than one task found"

    it "imports JSON task with notes and sets the created_utc for notes" $ do
      withMemoryDb conf $ \memConn -> do
        let
          utc = "2024-03-15 10:32:51"
          jsonTask =
            "{\"body\":\"Just a test\",\
            \\"created_at\":\"{{utc}}\",\
            \\"notes\": [\"Just a note\"]}"
              & T.replace "{{utc}}" utc

          expectedTaskToNote =
            TaskToNote
              { ulid =
                  emptyUlid
                    & P.flip setDateTime (utc & parseUtc & fromMaybe zeroTime)
                    & show @ULID
                    & T.toLower
              , task_ulid = "01hs0tqw1r0007h0mj78s1jntz"
              , note = "Just a note"
              }

        case eitherDecodeStrictText jsonTask of
          Left error ->
            P.die $ "Error decoding JSON: " <> show error
          Right importTaskRecord -> do
            _ <- insertImportTask conf memConn importTaskRecord
            taskToNoteList :: [TaskToNote] <-
              query_ memConn "SELECT * FROM task_to_note"
            case taskToNoteList of
              [taskToNote] -> do
                taskToNote.ulid `shouldNotBe` expectedTaskToNote.ulid
                (taskToNote.ulid & T.take 10)
                  `shouldBe` (expectedTaskToNote.ulid & T.take 10)
              _ -> P.die "Found more than one note"

    it "imports a JSON task with an updated_at field" $ do
      withMemoryDb conf $ \memConn -> do
        let
          updatedAt = "2024-03-16T14:20:30.500Z"
          -- modified_utc format:
          expectedModifiedUtc = "2024-03-16 14:20:30.500"
          jsonTask =
            "{\"body\":\"Task with updated_at\",\"updated_at\":\"{{utc}}\"}"
              & T.replace "{{utc}}" updatedAt

        case eitherDecodeStrictText jsonTask of
          Left error ->
            P.die $ "Error decoding JSON: " <> show error
          Right importTaskRecord -> do
            -- Test that setMissingFields preserves the modified_utc correctly
            importTaskNorm <- setMissingFields importTaskRecord
            importTaskNorm.task.modified_utc `shouldBe` expectedModifiedUtc
            -- Also verify it works when inserted into DB
            _ <- insertImportTask conf memConn importTaskNorm
            tasks :: [FullTask] <- query_ memConn "SELECT * FROM tasks_view"
            case tasks of
              [insertedTask] ->
                insertedTask.modified_utc `shouldBe` expectedModifiedUtc
              _ -> P.die "Expected exactly one task"

    it "imports a GitHub issue" $ do
      gitHubIssue <- BSL.readFile "test/fixtures/github-issue.json"
      withMemoryDb conf $ \memConn -> do
        let
          expectedImpTask =
            ImportTask
              { task =
                  emptyTask
                    { Task.ulid = "01hrz2qz7g0001qzskfchf4dek"
                    , Task.body = "Support getting the note body from stdin"
                    , Task.user = "ad-si"
                    , Task.modified_utc = "2024-03-14 18:14:14.000"
                    }
              , notes = []
              , tags = []
              , closedUtcWasExplicit = P.False
              }

        case eitherDecode gitHubIssue of
          Left error -> P.die $ "Error decoding JSON: " <> show error
          Right importTaskRecord -> do
            _ <- insertImportTask conf memConn importTaskRecord
            taskList :: [Task] <- query_ memConn "SELECT * FROM tasks"
            case taskList of
              [task] -> do
                task.ulid `shouldBe` expectedImpTask.task.ulid
                task.body `shouldBe` expectedImpTask.task.body
                task.user `shouldBe` expectedImpTask.task.user
                task.modified_utc `shouldBe` expectedImpTask.task.modified_utc
              _ -> P.die "Found more than one note"

    it "imports a single JSON task from array format" $ do
      withMemoryDb conf $ \memConn -> do
        let
          jsonTasks =
            "[{\"body\":\"Task 1\",\"tags\":[\"test\"]},\
            \{\"body\":\"Task 2\",\"tags\":[\"test\",\"demo\"]}]"

        case eitherDecodeStrictText jsonTasks of
          Left error ->
            P.die $ "Error decoding JSON: " <> show error
          Right (importTaskRecords :: [ImportTask]) -> do
            P.mapM_ (insertImportTask conf memConn) importTaskRecords
            tasks :: [FullTask] <-
              query_ memConn "SELECT * FROM tasks_view ORDER BY body ASC"
            case tasks of
              [task1, task2] -> do
                task1.body `shouldBe` "Task 1"
                task1.tags `shouldBe` Just ["test"]
                task2.body `shouldBe` "Task 2"
                task2.tags `shouldBe` Just ["demo", "test"]
              _ -> P.die "Expected exactly 2 tasks"

    it "imports multiple JSON tasks from array format" $ do
      withMemoryDb conf $ \memConn -> do
        let
          jsonTasks =
            "[{\"body\":\"First task\"},\
            \{\"body\":\"Second task\"},\
            \{\"body\":\"Third task\"}]"

        case eitherDecodeStrictText jsonTasks of
          Left error ->
            P.die $ "Error decoding JSON: " <> show error
          Right (importTaskRecords :: [ImportTask]) -> do
            P.mapM_ (insertImportTask conf memConn) importTaskRecords
            tasks :: [FullTask] <-
              query_ memConn "SELECT * FROM tasks_view ORDER BY body ASC"
            P.length tasks `shouldBe` 3
            case tasks of
              [task1, task2, task3] -> do
                task1.body `shouldBe` "First task"
                task2.body `shouldBe` "Second task"
                task3.body `shouldBe` "Third task"
              _ -> P.die "Expected exactly 3 tasks"

    it "imports a single JSON object (not in array)" $ do
      withMemoryDb conf $ \memConn -> do
        let
          jsonTask = "{\"body\":\"Single task\",\"tags\":[\"solo\"]}"

        case eitherDecodeStrictText jsonTask of
          Left error ->
            P.die $ "Error decoding JSON: " <> show error
          Right (importTaskRecord :: ImportTask) -> do
            _ <- insertImportTask conf memConn importTaskRecord
            tasks :: [FullTask] <- query_ memConn "SELECT * FROM tasks_view"
            case tasks of
              [task] -> do
                task.body `shouldBe` "Single task"
                task.tags `shouldBe` Just ["solo"]
              _ -> P.die "Expected exactly 1 task"

    it "imports array with tasks containing notes and tags" $ do
      withMemoryDb conf $ \memConn -> do
        let
          jsonTasks =
            "[{\"body\":\"Task with notes\",\
            \\"notes\":[\"Note 1\",\"Note 2\"],\
            \\"tags\":[\"important\"]},\
            \{\"body\":\"Another task\",\
            \\"tags\":[\"work\",\"urgent\"]}]"

        case eitherDecodeStrictText jsonTasks of
          Left error ->
            P.die $ "Error decoding JSON: " <> show error
          Right (importTaskRecords :: [ImportTask]) -> do
            P.mapM_ (insertImportTask conf memConn) importTaskRecords
            tasks :: [FullTask] <-
              query_ memConn "SELECT * FROM tasks_view ORDER BY body ASC"
            case tasks of
              [task1, task2] -> do
                task1.body `shouldBe` "Another task"
                task1.tags `shouldBe` Just ["urgent", "work"]
                task2.body `shouldBe` "Task with notes"
                task2.tags `shouldBe` Just ["important"]
              _ -> P.die "Expected exactly 2 tasks"

            taskToNotes :: [TaskToNote] <-
              query_ memConn "SELECT * FROM task_to_note"
            P.length taskToNotes `shouldBe` 2

    it "imports a single YAML object (not in array)" $ do
      withMemoryDb conf $ \memConn -> do
        let
          yamlTask =
            "body: Single YAML task\n\
            \tags:\n\
            \  - yaml\n\
            \  - test"

        case Yaml.decodeEither' (yamlTask & T.encodeUtf8) of
          Left error ->
            P.die $ "Error decoding YAML: " <> show error
          Right (importTaskRecord :: ImportTask) -> do
            _ <- insertImportTask conf memConn importTaskRecord
            tasks :: [FullTask] <- query_ memConn "SELECT * FROM tasks_view"
            case tasks of
              [task] -> do
                task.body `shouldBe` "Single YAML task"
                task.tags `shouldBe` Just ["test", "yaml"]
              _ -> P.die "Expected exactly 1 task"

    it "imports multiple YAML tasks from array format" $ do
      withMemoryDb conf $ \memConn -> do
        let
          yamlTasks =
            "- body: First YAML task\n\
            \  tags: [one]\n\
            \- body: Second YAML task\n\
            \  tags: [two]\n\
            \- body: Third YAML task\n\
            \  tags: [three]"

        case Yaml.decodeEither' (yamlTasks & T.encodeUtf8) of
          Left error ->
            P.die $ "Error decoding YAML: " <> show error
          Right (importTaskRecords :: [ImportTask]) -> do
            P.mapM_ (insertImportTask conf memConn) importTaskRecords
            tasks :: [FullTask] <-
              query_ memConn "SELECT * FROM tasks_view ORDER BY body ASC"
            P.length tasks `shouldBe` 3
            case tasks of
              [task1, task2, task3] -> do
                task1.body `shouldBe` "First YAML task"
                task1.tags `shouldBe` Just ["one"]
                task2.body `shouldBe` "Second YAML task"
                task2.tags `shouldBe` Just ["two"]
                task3.body `shouldBe` "Third YAML task"
                task3.tags `shouldBe` Just ["three"]
              _ -> P.die "Expected exactly 3 tasks"

    it "imports YAML array with tasks containing notes and tags" $ do
      withMemoryDb conf $ \memConn -> do
        let
          yamlTasks =
            "- body: YAML task with notes\n\
            \  notes:\n\
            \    - First note\n\
            \    - Second note\n\
            \  tags: [important, yaml]\n\
            \- body: Another YAML task\n\
            \  tags: [work]"

        case Yaml.decodeEither' (yamlTasks & T.encodeUtf8) of
          Left error ->
            P.die $ "Error decoding YAML: " <> show error
          Right (importTaskRecords :: [ImportTask]) -> do
            P.mapM_ (insertImportTask conf memConn) importTaskRecords
            tasks :: [FullTask] <-
              query_ memConn "SELECT * FROM tasks_view ORDER BY body ASC"
            case tasks of
              [task1, task2] -> do
                task1.body `shouldBe` "Another YAML task"
                task1.tags `shouldBe` Just ["work"]
                task2.body `shouldBe` "YAML task with notes"
                task2.tags `shouldBe` Just ["important", "yaml"]
              _ -> P.die "Expected exactly 2 tasks"

            taskToNotes :: [TaskToNote] <-
              query_ memConn "SELECT * FROM task_to_note"
            P.length taskToNotes `shouldBe` 2

    it "imports YAML with mixed single and multiple tags format" $ do
      withMemoryDb conf $ \memConn -> do
        let
          yamlTasks =
            "- body: Inline tags\n\
            \  tags: [tag1, tag2]\n\
            \- body: Block tags\n\
            \  tags:\n\
            \    - tag3\n\
            \    - tag4"

        case Yaml.decodeEither' (yamlTasks & T.encodeUtf8) of
          Left error ->
            P.die $ "Error decoding YAML: " <> show error
          Right (importTaskRecords :: [ImportTask]) -> do
            P.mapM_ (insertImportTask conf memConn) importTaskRecords
            tasks :: [FullTask] <-
              query_ memConn "SELECT * FROM tasks_view ORDER BY body ASC"
            case tasks of
              [task1, task2] -> do
                task1.body `shouldBe` "Block tags"
                task1.tags `shouldBe` Just ["tag3", "tag4"]
                task2.body `shouldBe` "Inline tags"
                task2.tags `shouldBe` Just ["tag1", "tag2"]
              _ -> P.die "Expected exactly 2 tasks"

    it "correctly parses multiple notes as JSON array with ulid and body" $ do
      withMemoryDb conf $ \memConn -> do
        let
          jsonTask =
            "{\"body\":\"Task with multiple notes\", \
            \\"notes\":[\"First note\", \"Second note\", \"Third note\"]}"

        case eitherDecodeStrictText jsonTask of
          Left error ->
            P.die $ "Error decoding JSON: " <> show error
          Right importTaskRecord -> do
            _ <- insertImportTask conf memConn importTaskRecord
            tasks :: [FullTask] <- query_ memConn "SELECT * FROM tasks_view"

            case tasks of
              [insertedTask] -> do
                insertedTask.body `shouldBe` "Task with multiple notes"
                -- Verify notes is a Just with 3 notes
                case insertedTask.notes of
                  Just notesList -> do
                    P.length notesList `shouldBe` 3
                    -- Each note should have a ulid and body
                    P.forM_ (P.zip notesList ["First note", "Second note", "Third note"]) $
                      \(note, expectedBody) -> do
                        note.ulid `shouldNotBe` ""
                        note.body `shouldBe` expectedBody
                  Nothing -> P.die "Expected notes to be Just"
              _ -> P.die "Expected exactly one task"

    it "correctly parses notes containing special characters" $ do
      withMemoryDb conf $ \memConn -> do
        let
          -- Notes with commas, quotes, and other special characters
          jsonTask =
            "{\"body\":\"Task with special notes\", \
            \\"notes\":[\"Note with, comma\", \"Note with \\\"quotes\\\"\", \"Note: with colon\"]}"

        case eitherDecodeStrictText jsonTask of
          Left error ->
            P.die $ "Error decoding JSON: " <> show error
          Right importTaskRecord -> do
            _ <- insertImportTask conf memConn importTaskRecord
            tasks :: [FullTask] <- query_ memConn "SELECT * FROM tasks_view"

            case tasks of
              [insertedTask] -> do
                case insertedTask.notes of
                  Just [note1, note2, note3] -> do
                    note1.body `shouldBe` "Note with, comma"
                    note2.body `shouldBe` "Note with \"quotes\""
                    note3.body `shouldBe` "Note: with colon"
                  Just _ -> P.die "Expected exactly 3 notes"
                  Nothing -> P.die "Expected notes to be Just"
              _ -> P.die "Expected exactly one task"

    it "correctly parses multiple tags as JSON array" $ do
      withMemoryDb conf $ \memConn -> do
        let
          jsonTask =
            "{\"body\":\"Task with multiple tags\", \
            \\"tags\":[\"urgent\", \"work\", \"project-alpha\", \"2024\"]}"

        case eitherDecodeStrictText jsonTask of
          Left error ->
            P.die $ "Error decoding JSON: " <> show error
          Right importTaskRecord -> do
            _ <- insertImportTask conf memConn importTaskRecord
            tasks :: [FullTask] <- query_ memConn "SELECT * FROM tasks_view"

            case tasks of
              [insertedTask] -> do
                insertedTask.body `shouldBe` "Task with multiple tags"
                -- Tags are sorted alphabetically in the database
                case insertedTask.tags of
                  Just tagsList -> do
                    P.length tagsList `shouldBe` 4
                    -- Verify all tags are present (order may vary due to JSON array)
                    P.sort tagsList `shouldBe` ["2024", "project-alpha", "urgent", "work"]
                  Nothing -> P.die "Expected tags to be Just"
              _ -> P.die "Expected exactly one task"

    it "handles task with both notes and tags as JSON arrays" $ do
      withMemoryDb conf $ \memConn -> do
        let
          jsonTask =
            "{\"body\":\"Full task\", \
            \\"tags\":[\"important\", \"review\"], \
            \\"notes\":[\"Remember to check\", \"Follow up needed\"]}"

        case eitherDecodeStrictText jsonTask of
          Left error ->
            P.die $ "Error decoding JSON: " <> show error
          Right importTaskRecord -> do
            _ <- insertImportTask conf memConn importTaskRecord
            tasks :: [FullTask] <- query_ memConn "SELECT * FROM tasks_view"

            case tasks of
              [insertedTask] -> do
                insertedTask.body `shouldBe` "Full task"
                -- Verify tags
                case insertedTask.tags of
                  Just tagsList -> do
                    P.length tagsList `shouldBe` 2
                    P.sort tagsList `shouldBe` ["important", "review"]
                  Nothing -> P.die "Expected tags to be Just"
                -- Verify notes
                case insertedTask.notes of
                  Just [note1, note2] -> do
                    note1.body `shouldBe` "Remember to check"
                    note2.body `shouldBe` "Follow up needed"
                  Just _ -> P.die "Expected exactly 2 notes"
                  Nothing -> P.die "Expected notes to be Just"
              _ -> P.die "Expected exactly one task"

    it "returns Nothing for tasks with no notes or tags" $ do
      withMemoryDb conf $ \memConn -> do
        let jsonTask = "{\"body\":\"Simple task\"}"

        case eitherDecodeStrictText jsonTask of
          Left error ->
            P.die $ "Error decoding JSON: " <> show error
          Right importTaskRecord -> do
            _ <- insertImportTask conf memConn importTaskRecord
            tasks :: [FullTask] <- query_ memConn "SELECT * FROM tasks_view"

            case tasks of
              [insertedTask] -> do
                insertedTask.body `shouldBe` "Simple task"
                insertedTask.tags `shouldBe` Nothing
                insertedTask.notes `shouldBe` Nothing
              _ -> P.die "Expected exactly one task"

  describe "Export" $ do
    it "exports several tasks as NDJSON including notes" $ do
      let
        task =
          emptyTask
            { Task.ulid = "01jczsz9c328fm7xydcwxbmv6n"
            , Task.body = "Buy milk"
            , Task.user = "ad-si"
            , Task.modified_utc = "2024-09-24 22:31:09.123"
            }

        importTask =
          ImportTask
            { task = task
            , notes = [Note "01jczszra25ec48pagzcw5qw6j" "Test note"]
            , tags = []
            , closedUtcWasExplicit = P.False
            }

        taskJson =
          "[{\
          \\"awake_utc\":null,\
          \\"body\":\"Buy milk\",\
          \\"closed_utc\":null,\
          \\"created_utc\":\"2024-11-18 14:14:07.491\",\
          \\"due_utc\":null,\
          \\"group_ulid\":null,\
          \\"metadata\":null,\
          \\"modified_utc\":\"2024-09-24 22:31:09.123\",\
          \\"notes\":[{\
          \\"body\":\"Test note\",\
          \\"ulid\":\"01jczsz9c35ec48pagzcw5qw6j\"\
          \}],\
          \\"priority\":1,\
          \\"ready_utc\":null,\
          \\"recurrence_duration\":null,\
          \\"repetition_duration\":null,\
          \\"review_utc\":null,\
          \\"state\":null,\
          \\"tags\":null,\
          \\"ulid\":\"01jczsz9c328fm7xydcwxbmv6n\",\
          \\"user\":\"ad-si\",\
          \\"waiting_utc\":null\
          \}]"

      res <- withMemoryDb conf $ \memConn -> do
        _ <- insertImportTask conf memConn importTask
        getNdjsonLines memConn

      (show res :: P.Text) `shouldBe` taskJson
