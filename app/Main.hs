{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import Protolude

import Lib
import qualified Data.Text as T
import Options.Applicative
import Utils
import ImportExport
import Task (TaskState(..))


toParserInfo :: Parser a -> Text -> ParserInfo a
toParserInfo parser description =
  info (helper <*> parser) (fullDesc <> progDesc (T.unpack description))

type IdText = Text
type TagText = Text

data Command
  {- Modify -}
  = AddTask IdText
  -- | LogTask IdText
  | DoTask IdText
  | EndTask IdText
  | DeleteTask IdText
  | AddTag IdText TagText
  -- | Note -- Add a note
  -- | Denote -- Remove all notes
  -- | Start -- Add a note that work on task was started
  -- | Stop -- Add a note that work on task was stopped
  -- | Clone -- Clone an existing task
  -- | Edit -- Launch editor with YAML version of task
  -- | Append -- Append words to a task description
  -- | Prepend -- Prepend words to a task description
  -- | Undo -- Revert last change
  -- | Repeat -- Set repeating interval for a task

  {- I/O -}
  | Import
  | Csv
  | Ndjson
  -- | Backup -- Create a local backup of tasks database

  {- List -}
  | List (Filter TaskState)
  | Count (Filter TaskState)
  -- | Views -- List all available views
  -- | Tags -- List all used tags
  -- | Active -- Started tasks
  -- | Blocked -- Tasks that are blocked by other tasks (newest first)
  -- | Blockers -- Tasks that block other tasks (newest first)
  -- | Unblocked -- Tasks that are not blocked


  {- Misc -}
  -- | Demo -- Switch to demo mode
  -- | Version -- Show version
  -- | License -- Show license
  | Help

  deriving (Show, Eq)


-- "blocking" "blockers"
-- "annotate" "note"
-- "denotate" "denote"
-- "rm" "delete"
-- "remove" "delete"
-- "duplicate" "clone"


addParser :: Parser Command
addParser = AddTask <$>
  strArgument (metavar "BODY" <> help "Body of the task")

doneParser :: Parser Command
doneParser = DoTask <$>
  strArgument (metavar "TASK_ID" <> help "Id of the task (Ulid)")

countParser :: Parser Command
countParser = pure $ Count NoFilter


commandParser :: Parser Command
commandParser =
  (pure $ List $ Only Open) -- "List all tasks sorted by priority"
  <|>
  ( subparser
    (  commandGroup "Basic Commands:"
    <> command "add" (toParserInfo addParser "Add a new task")
    <> command "do" (toParserInfo doneParser "Mark a task as done")
    <> command "end" (toParserInfo (EndTask <$>
        strArgument (metavar "TASK_ID" <> help "Id of the task (Ulid)"))
        "Mark a task as obsolete")
    <> command "delete" (toParserInfo (DeleteTask <$>
        strArgument (metavar "TASK_ID" <> help "Id of the task (Ulid)"))
        "Delete a task from the database (Attention: Irreversible)")
    <> command "tag" (toParserInfo (AddTag
      <$> strArgument (metavar "TASK_ID" <> help "Id of the task (Ulid)")
      <*> strArgument (metavar "TAG" <> help "The tag"))
      "Add a tag to a task")
    -- <> command "info" (toParserInfo addParser "Show detailed information to a task")
    )
  <|> subparser
    (  commandGroup "List Commands:"
    <> command "all" (toParserInfo (pure $ List NoFilter)
        "List all tasks in chronological order")
    <> command "done" (toParserInfo (pure $ List $ Only Done)
        "List all done tasks")
    <> command "waiting" (toParserInfo (pure $ List $ Only Waiting)
        "List all waiting tasks")
    <> command "obsolete" (toParserInfo (pure $ List $ Only Obsolete)
        "List all obsolete tasks")
    -- <> command "next" "Show tasks with descending priority"
    -- <> command "newest" "Show all tasks (newest first)"
    -- <> command "oldest" "Show all tasks (oldest first)"
    -- <> command "overdue" -- Overdue tasks
    -- <> command "repeating" -- Open repeating tasks (soonest first)
    -- <> command "unblocked" -- Tasks that are not blocked (by priority)

  -- <|> subparser
    -- (  commandGroup "Visualizations:"
    -- <> command "burndown" -- "Burndown chart by week"
    -- <> command "calendar" -- "Calendar view of all open tasks"
    -- <> command "history" -- "History of tasks"
    -- <> command "stats" -- "Statistics of all tasks"
    -- <> command "ulids" -- "List all ULIDs"
    -- <> command "tags" -- "List all tags"
    -- <> command "progress" -- "List all tags with corresponding progress"
    -- <> command "filter" -- "Filter tasks by specified tags"


    )
  <|> subparser
    (  commandGroup "I/O Commands:"
    <> command "import" (toParserInfo (pure Import)
        "Import one JSON task")
    <> command "csv" (toParserInfo (pure Csv)
        "Export tasks in CSV format")
    <> command "ndjson" (toParserInfo (pure Ndjson)
        "Export tasks in NDJSON format")
    )
  <|> subparser
    (  commandGroup "Advanced Commands:"
    <> command "count" (toParserInfo countParser "Output total number of tasks")
    <> command "help" (toParserInfo (pure $ Help) "Display current help page")
    )
  )

commandParserInfo :: ParserInfo Command
commandParserInfo = info
  (commandParser <**> helper)
  fullDesc


main :: IO ()
main = do
  cliCommand <- execParser commandParserInfo
  case cliCommand of
    List taskFilter -> listTasks taskFilter
    Import -> importTask
    Csv -> dumpCsv
    Ndjson -> dumpNdjson
    AddTask body -> addTask body
    DoTask idSubstr -> doTask idSubstr
    EndTask idSubstr -> endTask idSubstr
    DeleteTask idSubstr -> deleteTask idSubstr
    AddTag idSubstr tagText  -> addTag idSubstr tagText
    Count taskFilter -> countTasks taskFilter
    Help -> handleParseResult . Failure $
      parserFailure defaultPrefs commandParserInfo ShowHelpText mempty
