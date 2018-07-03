module Main where

import Protolude

import Lib
import Data.Hourglass
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Version (showVersion)
import Options.Applicative
import Utils
import ImportExport
import Task (TaskState(..))
import Paths_tasklite (version)

data Command
  {- Add -}
  = AddTask   [Text]
  | AddWrite  [Text]
  | AddRead   [Text]
  | AddWatch  [Text]
  | AddListen [Text]
  | AddBuy    [Text]
  | AddSell   [Text]
  | AddShip   [Text]
  | LogTask   [Text]

  {- Modify -}
  | DoTask IdText
  | EndTask IdText
  | DeleteTask IdText
  | BoostTasks [IdText]
  | HushTasks [IdText]
  | Prioritize Float [IdText]
  | AddTag TagText [IdText]
  | SetDueUtc DateTime [IdText]
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
  | ListAll
  | ListHead
  | ListNew
  | ListOpen
  | ListDone
  | ListObsolete
  | ListWaiting
  | ListNoTag
  | Count (Filter TaskState)
  | QueryTasks Text
  | RunSql Text
  -- | Views -- List all available views
  | Tags -- List all used tags
  -- | Active -- Started tasks
  -- | Blocked -- Tasks that are blocked by other tasks (newest first)
  -- | Blockers -- Tasks that block other tasks (newest first)
  -- | Unblocked -- Tasks that are not blocked


  {- Misc -}
  -- | Demo -- Switch to demo mode
  | Version -- Show version
  -- | License -- Show license
  | Alias Text
  | Help

  deriving (Show, Eq)


nameToAliasList :: [(Text, Text)]
nameToAliasList = (
  ("rm", "delete") :
  ("remove", "delete") :
  ("inbox", "notag") :
  -- ("duplicate", "clone") :
  -- ("blocking", "blockers") :
  -- ("annotate", "note") :
  -- ("denotate", "denote") :
  [])


aliasWarning :: Text -> Doc AnsiStyle
aliasWarning alias =
  "Invalid command."
    <+> "Use" <+> (dquotes $ pretty alias) <+> "instead."
    <> hardline


getCommand :: (Text, Text) -> Mod CommandFields Command
getCommand (alias, commandName) =
  command (T.unpack alias) $ info
    (pure $ Alias commandName)
    (progDesc $ T.unpack $ "-> " <> commandName)


toParserInfo :: Parser a -> Text -> ParserInfo a
toParserInfo parser description =
  info (helper <*> parser) (fullDesc <> progDesc (T.unpack description))


idVar :: Mod ArgumentFields a
idVar =
  metavar "TASK_ID" <> help "Id of the task (Ulid)"


commandParser :: Parser Command
commandParser =
  (pure $ ListHead)
  <|>
  ( subparser ( commandGroup "Basic Commands:"

    <> command "add" (toParserInfo (AddTask <$> some (strArgument
        (metavar "BODY" <> help "Body of the task")))
        "Add a new task")

    <> command "log" (toParserInfo (LogTask <$> some (strArgument
        (metavar "BODY" <> help "Body of the task")))
        "Log an already completed task")

    <> command "do" (toParserInfo (DoTask <$> strArgument idVar)
        "Mark a task as done")

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
      <$> strArgument (metavar "TAG" <> help "The tag")
      <*> some (strArgument idVar))
      "Add a tag to specified tasks")

    <> command "due" (toParserInfo (SetDueUtc
      <$> argument (maybeReader (parseUtc . T.pack))
            (metavar "DUE_UTC" <> help "Due timestamp in UTC")
      <*> some (strArgument idVar))
      "Set due UTC of specified tasks")
    )

  <|> subparser ( commandGroup "Shortcuts to Add a Task:"

    <> command "write" (toParserInfo (AddWrite <$> some (strArgument
        (metavar "BODY" <> help "Body of the task")))
        "Write a message or a post")

    <> command "read" (toParserInfo (AddRead <$> some (strArgument
        (metavar "BODY" <> help "Url or title oo a website or blog post")))
        "Read the specified URL")

    <> command "watch" (toParserInfo (AddWatch <$> some (strArgument
        (metavar "BODY" <> help "Url or title of a video or movie to watch")))
        "Watch a movie or a video")

    <> command "listen" (toParserInfo (AddListen <$> some (strArgument
        (metavar "BODY"
          <> help "Url or title of a song or podcast to listen to")))
        "Listen to a song or podcast")

    <> command "buy" (toParserInfo (AddBuy <$> some (strArgument
        (metavar "BODY" <> help "Body of the task")))
        "Buy something")

    <> command "sell" (toParserInfo (AddSell <$> some (strArgument
        (metavar "BODY" <> help "Body of the task")))
        "Sell something")

    <> command "ship" (toParserInfo (AddShip <$> some (strArgument
        (metavar "BODY" <> help "Body of the task")))
        "Ship an item to someone")
    )

  <|> subparser ( commandGroup "List Commands:"

    <> command "head" (toParserInfo (pure $ ListHead)
        ("List " <> show (headCount conf)
          <> " most important open tasks by priority"))

    <> command "all" (toParserInfo (pure $ ListAll)
        "List all tasks by priority")

    <> command "open" (toParserInfo (pure $ ListOpen)
        "List all open tasks by creation UTC desc")

    <> command "new" (toParserInfo (pure $ ListNew)
        ("List " <> show (headCount conf)
          <> " newest open tasks by creation UTC desc"))

    <> command "waiting" (toParserInfo (pure $ ListWaiting)
        "List all waiting tasks by priority")

    <> command "done" (toParserInfo (pure $ ListDone)
        ("List " <> show (headCount conf)
          <> "  done tasks by closing UTC desc"))

    <> command "obsolete" (toParserInfo (pure $ ListObsolete)
        "List all obsolete tasks by closing UTC")

    <> command "notag" (toParserInfo (pure $ ListNoTag)
        "List all tasks without a tag")

    -- TODO: Replace with tasks and tags commands
    <> command "query" (toParserInfo (QueryTasks <$> strArgument
        (metavar "QUERY" <> help "The SQL query after the \"where\" clause"))
        "Run \"select * from tasks where QUERY\" on the database")

    -- <> command "tasks" (toParserInfo (QueryTasks <$> strArgument
    --     (metavar "QUERY" <> help "The SQL query after the \"where\" clause"))
    --     "Run \"select * from tasks where QUERY\" on the database")

    -- <> command "tags" (toParserInfo (QueryTasks <$> strArgument
    --     (metavar "QUERY" <> help "The SQL query after the \"where\" clause"))
    --     "Run \"select * from tasks where QUERY\" on the database")

    <> command "tags" (toParserInfo (pure $ Tags)
        "List all used tags and their progress summary")

    <> command "runsql" (toParserInfo (RunSql <$> strArgument
        (metavar "QUERY" <> help "The SQL query"))
        "Run any SQL query and output result as CSV")

    -- <> command "newest" "Show all tasks (newest first)"
    -- <> command "oldest" "Show all tasks (oldest first)"
    -- <> command "overdue" -- Overdue tasks
    -- <> command "repeating" -- Open repeating tasks (soonest first)
    -- <> command "unblocked" -- Tasks that are not blocked (by priority)

  -- <|> subparser ( commandGroup "Visualizations:"
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

  <|> subparser ( commandGroup "I/O Commands:"

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

  <|> subparser ( commandGroup "Advanced Commands:"

    <> command "count" (toParserInfo (pure $ Count NoFilter)
        "Output total number of tasks")

    <> command "version" (toParserInfo (pure Version) "Display version")

    <> command "help" (toParserInfo (pure Help) "Display current help page")
    )

  <|> subparser ( commandGroup "Aliases:"

    <> fold (fmap getCommand nameToAliasList)
    )
  )


commandParserInfo :: ParserInfo Command
commandParserInfo =
  info
    (helper <*> commandParser)
    (noIntersperse
      <> briefDesc
      <> (headerDoc $ Just
            "TaskLite - Task-list manager powered by Haskell and SQLite")
      <> (progDescDoc $ Just
            "The default command `tasklite` is the same as `tasklite head`.")
      <> (footer $
            "Version " <> (showVersion version)
            <> ", developed by <adriansieber.com> at <feram.io>")
    )


helpText :: Doc AnsiStyle
helpText =
  case (parserFailure defaultPrefs commandParserInfo ShowHelpText mempty) of
    ParserFailure a -> case a "tasklite" of
      (theHelp, _, _) -> theHelp
        & show
        & T.replace "\n  add     " "\n  add BODY"
        & pretty
        & (<> hardline)


main :: IO ()
main = do
  cliCommand <- execParser commandParserInfo

  connection <- setupConnection

  let addTaskC = addTask connection

  -- runMigrations connection

  doc <- case cliCommand of
    ListAll -> listAll connection
    ListHead -> headTasks connection
    ListNew -> newTasks connection
    ListOpen -> openTasks connection
    ListWaiting -> listWaiting connection
    ListDone -> doneTasks connection
    ListObsolete -> obsoleteTasks connection
    ListNoTag -> listNoTag connection
    QueryTasks query -> queryTasks query
    RunSql query -> runSql query
    Tags -> listTags connection
    Import -> importTask
    Csv -> dumpCsv
    Ndjson -> dumpNdjson
    Sql -> dumpSql
    Backup -> backupDatabase
    AddTask bodyWords -> addTaskC bodyWords
    AddWrite bodyWords -> addTaskC $ ["Write"] <> bodyWords <> ["+write"]
    AddRead bodyWords -> addTaskC $ ["Read"] <> bodyWords <> ["+read"]
    AddWatch bodyWords -> addTaskC $ ["Watch"] <> bodyWords <> ["+watch"]
    AddListen bodyWords -> addTaskC $ ["Listen"] <> bodyWords <> ["+listen"]
    AddBuy bodyWords -> addTaskC $ ["Buy"] <> bodyWords <> ["+buy"]
    AddSell bodyWords -> addTaskC $ ["Sell"] <> bodyWords <> ["+sell"]
    AddShip bodyWords -> addTaskC $ ["Ship"] <> bodyWords <> ["+ship"]
    LogTask bodyWords -> logTask connection bodyWords
    DoTask idSubstr -> doTask connection idSubstr
    EndTask idSubstr -> endTask idSubstr
    DeleteTask idSubstr -> deleteTask connection idSubstr
    BoostTasks ids -> adjustPriority 1 ids
    HushTasks ids -> adjustPriority (-1) ids
    Prioritize val ids -> adjustPriority val ids
    InfoTask idSubstr -> infoTask idSubstr
    NextTask -> nextTask connection
    FindTask pattern -> findTask pattern
    AddTag tagText ids -> addTag connection tagText ids
    SetDueUtc datetime ids -> setDueUtc connection datetime ids
    Count taskFilter -> countTasks taskFilter
    Version -> pure $ (pretty $ showVersion version) <> hardline
    Help -> pure helpText
    Alias alias -> pure $ aliasWarning alias

  -- TODO: Remove color when piping into other command
  putDoc doc

