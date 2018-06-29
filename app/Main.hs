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


data Command
  {- Modify -}
  = AddTask IdText
  -- | LogTask IdText
  | DoTask IdText
  | EndTask IdText
  | DeleteTask IdText
  | BoostTasks [IdText]
  | HushTasks [IdText]
  | Prioritize Float [IdText]
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

  {- Show -}
  | InfoTask IdText
  | NextTask
  | FindTask Text

  {- I/O -}
  | Import
  | Csv
  | Ndjson
  | Sql
  | Backup

  {- List -}
  | List (Filter TaskState)
  | ListHead
  | ListNew
  | Count (Filter TaskState)
  | QueryTasks Text
  | RunSql Text
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


-- TODO: Add aliases
-- "blocking" "blockers"
-- "annotate" "note"
-- "denotate" "denote"
-- "rm" "delete"
-- "remove" "delete"
-- "duplicate" "clone"

idVar :: Mod ArgumentFields a
idVar = metavar "TASK_ID" <> help "Id of the task (Ulid)"

addParser :: Parser Command
addParser = AddTask <$>
  strArgument (metavar "BODY" <> help "Body of the task")

doneParser :: Parser Command
doneParser = DoTask <$>
  strArgument idVar

countParser :: Parser Command
countParser = pure $ Count NoFilter


commandParser :: Parser Command
commandParser =
  (pure $ ListHead) -- "Same as "head" command"
  <|>
  ( subparser
    (  commandGroup "Basic Commands:"
    <> command "add" (toParserInfo addParser "Add a new task")

    <> command "do" (toParserInfo doneParser "Mark a task as done")

    <> command "end" (toParserInfo (EndTask <$> strArgument idVar)
        "Mark a task as obsolete")

    <> command "delete" (toParserInfo (DeleteTask <$> strArgument idVar)
        "Delete a task from the database (Attention: Irreversible)")

    <> command "boost" (toParserInfo (BoostTasks <$> some (strArgument idVar))
          "Increase priority of specified tasks by 1")

    <> command "hush" (toParserInfo (HushTasks <$> some (strArgument idVar))
          "Decrease priority of specified tasks by 1")

    <> command "prioritize" (toParserInfo (Prioritize
          <$> argument auto (metavar "VALUE"
            <> help "Value to adjust priority by")
          <*> some (strArgument idVar))
          "Adjust priority of specified tasks")

    <> command "info" (toParserInfo (InfoTask <$> strArgument idVar)
        "Show detailed information and metadata of task")

    <> command "next" (toParserInfo (pure NextTask)
        "Show the task with the highest priority")

    <> command "find" (toParserInfo (FindTask <$> strArgument
        (metavar "PATTERN" <> help "Search pattern"))
        "Fuzzy search a task")

    <> command "tag" (toParserInfo (AddTag
      <$> strArgument idVar
      <*> strArgument (metavar "TAG" <> help "The tag"))
      "Add a tag to a task")
    )

  <|> subparser
    (  commandGroup "List Commands:"

    <> command "head" (toParserInfo (pure $ ListHead) ("List "<>
        show (headCount conf) <> " most important tasks sorted by priority"))

    <> command "all" (toParserInfo (pure $ List NoFilter)
        "List all tasks in chronological order")
        "List all tasks by priority")

    <> command "new" (toParserInfo (pure $ ListNew) ("List "<>
        show (headCount conf) <> " newest tasks sorted chronologically"))

    <> command "done" (toParserInfo (pure $ List $ Only Done)
        "List all done tasks")

    <> command "waiting" (toParserInfo (pure $ List $ Only Waiting)
        "List all waiting tasks")

    <> command "obsolete" (toParserInfo (pure $ List $ Only Obsolete)
        "List all obsolete tasks")

    <> command "query" (toParserInfo (QueryTasks <$> strArgument
        (metavar "QUERY" <> help "The SQL query after the \"where\" clause"))
        "Run \"select * from tasks where QUERY\" on the database")

    <> command "runsql" (toParserInfo (RunSql <$> strArgument
        (metavar "QUERY" <> help "The SQL query"))
        "Run any SQL query and output result as CSV")

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
    -- <> command "find" -- "Filter tasks by specified tags"

    )
  <|> subparser
    (  commandGroup "I/O Commands:"

    <> command "import" (toParserInfo (pure Import)
        "Import one JSON task from stdin")

    <> command "csv" (toParserInfo (pure Csv)
        "Export tasks in CSV format")

    <> command "ndjson" (toParserInfo (pure Ndjson)
        "Export tasks in NDJSON format")

    <> command "sql" (toParserInfo (pure Sql)
        "Show SQL commands to create and populate database")

    <> command "backup" (toParserInfo (pure Backup)
        "Create a local backup of tasks database")
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
    ListHead -> headTasks
    ListNew -> newTasks
    QueryTasks query -> queryTasks query
    RunSql query -> runSql query
    Import -> importTask
    Csv -> dumpCsv
    Ndjson -> dumpNdjson
    Sql -> dumpSql
    Backup -> backupDatabase
    AddTask body -> addTask body
    DoTask idSubstr -> doTask idSubstr
    EndTask idSubstr -> endTask idSubstr
    DeleteTask idSubstr -> deleteTask idSubstr
    BoostTasks ids -> adjustPriority 1 ids
    HushTasks ids -> adjustPriority (-1) ids
    Prioritize val ids -> adjustPriority val ids
    InfoTask idSubstr -> infoTask idSubstr
    NextTask -> nextTask
    FindTask pattern -> findTask pattern
    AddTag idSubstr tagText  -> addTag idSubstr tagText
    Count taskFilter -> countTasks taskFilter
    Help ->
      case (parserFailure defaultPrefs commandParserInfo ShowHelpText mempty) of
        ParserFailure a -> case a "tasklite" of
          (theHelp, _, _) -> theHelp
            & show
            & T.replace "\n  add     " "\n  add BODY"
            & putStrLn

