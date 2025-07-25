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
import Data.ULID (ULID)
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
import ImportTask (ImportTask (ImportTask, notes, tags, task))
import Note (Note (Note))
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
            }

        taskJson =
          "[{\
          \\"awake_utc\":null,\
          \\"body\":\"Buy milk\",\
          \\"closed_utc\":null,\
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
