{-|
This is the main module which provides the CLI
-}

module Main where

import Protolude

import Lib
import Data.Char (isSpace)
import Data.Hourglass
import Data.String (fromString)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Version (showVersion)
import Options.Applicative
import Time.System
import Database.SQLite.Simple (close)

import Config
import DbSetup
import ImportExport
import Migrations
import Paths_tasklite (version)
import Task (TaskState(..))
import Utils


data Command
  {- Add -}
  = AddTask   [IdText]
  | AddWrite  [IdText]
  | AddRead   [IdText]
  | AddWatch  [IdText]
  | AddListen [IdText]
  | AddBuy    [IdText]
  | AddSell   [IdText]
  | AddPay    [IdText]
  | AddShip   [IdText]
  | LogTask   [IdText]

  {- Modify -}
  | DoTasks [IdText]
  | EndTasks [IdText]
  | TrashTasks [IdText]
  | DeleteTasks [IdText]
  | BoostTasks [IdText]
  | HushTasks [IdText]

  {- Modify With Parameter -}
  | Prioritize Float [IdText]
  | AddTag TagText [IdText]
  | AddNote Text [IdText]
  | SetDueUtc DateTime [IdText]
  | Start [IdText]
  | Stop [IdText]
  | Duplicate [IdText]
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
  | ListDeletable
  | ListWaiting
  | ListOverdue
  | ListNoTag
  | ListWithTag [Text]
  | Count (Filter TaskState)
  | QueryTasks Text
  | RunSql Text
  | RunFilter [Text]
  -- | Views -- List all available views
  | Tags -- List all used tags
  -- | Active -- Started tasks
  -- | Blocked -- Tasks that are blocked by other tasks (newest first)
  -- | Blockers -- Tasks that block other tasks (newest first)
  -- | Unblocked -- Tasks that are not blocked

  {- Unset -}
  | UnDueTasks [IdText]

  {- Misc -}
  -- | Demo -- Switch to demo mode
  | Version -- Show version
  -- | License -- Show license
  | Alias Text
  | Help
  | UlidToUtc Text

  deriving (Show, Eq)


nameToAliasList :: [(Text, Text)]
nameToAliasList = (
  ("annotate", "note") :
  ("clone", "duplicate") :
  ("close", "end") :
  ("decrease", "hush") :
  ("finish", "do") :
  ("fix", "do") :
  ("implement", "do") :
  ("inbox", "notag") :
  ("increase", "boost") :
  ("remove", "delete") :
  -- ("reopen", "unclose") :
  ("rm", "delete") :
  ("stop", "end") :
  ("search", "find") :
  -- ("week", "sunday") :
  -- ("latest", "newest") :
  -- ("snooze", "wait") :
  -- ("sleep", "wait") :
  -- ("schedule", "wait") :
  -- ("blocking", "blockers") :
  -- ("denotate", "denote") :
  [])


aliasWarning :: Text -> Doc AnsiStyle
aliasWarning alias =
  "Invalid command."
    <+> "Use" <+> dquotes (pretty alias) <+> "instead."
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


-- | Help Sections
basic_sec, shortcut_sec, list_sec,
  vis_sec, i_o_sec, advanced_sec,
  alias_sec, unset_sec, utils_sec
  :: (Text, Text)

basic_sec    = ("{{basic_sec}}", "Basic Commands:")
shortcut_sec = ("{{shortcut_sec}}", "Shortcuts to Add a Task:")
list_sec     = ("{{list_sec}}", "List Commands:")
vis_sec      = ("{{vis_sec}}", "Visualizations:")
i_o_sec      = ("{{i_o_sec}}", "I/O Commands:")
advanced_sec = ("{{advanced_sec}}", "Advanced Commands:")
alias_sec    = ("{{alias_sec}}", "Aliases:")
unset_sec    = ("{{unset_sec}}", "Unset:")
utils_sec    = ("{{utils_sec}}", "Utils:")



commandParser :: Parser Command
commandParser =
  (pure ListHead)
  <|>
  (   subparser ( commandGroup (T.unpack $ fst basic_sec)

    <> command "add" (toParserInfo (AddTask <$> some (strArgument
        (metavar "BODY" <> help "Body of the task")))
        "Add a new task")

    -- <> command "prompt" (toParserInfo (pure AddInteractive)
    --     "Add a new task via an interactive prompt")

    <> command "log" (toParserInfo (LogTask <$> some (strArgument
        (metavar "BODY" <> help "Body of the task")))
        "Log an already completed task")

    <> command "do" (toParserInfo (DoTasks <$> some (strArgument idVar))
        "Mark a task as done")

    <> command "end" (toParserInfo (EndTasks <$> some (strArgument idVar))
        "Mark a task as obsolete")

    <> command "trash" (toParserInfo (TrashTasks <$> some (strArgument idVar))
        "Mark a task as deletable")

    <> command "delete" (toParserInfo (DeleteTasks <$> some (strArgument idVar))
        "Delete a task from the database (Attention: Irreversible)")

    <> command "duplicate" (toParserInfo
        (Duplicate <$> some (strArgument idVar))
        "Duplicates a task (and resets the closed and due UTC fields)")

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

    <> command "note" (toParserInfo (AddNote
      <$> strArgument (metavar "NOTE" <> help "The note")
      <*> some (strArgument idVar))
      "Add a note to specified tasks")

    <> command "due" (toParserInfo (SetDueUtc
      <$> argument (maybeReader (parseUtc . T.pack))
            (metavar "DUE_UTC" <> help "Due timestamp in UTC")
      <*> some (strArgument idVar))
      "Set due UTC of specified tasks")

    <> command "start" (toParserInfo
        (Start <$> some (strArgument idVar))
        "Add a note that work on task was started")

    <> command "stop" (toParserInfo
        (Stop <$> some (strArgument idVar))
        "Add a note that work on task was stopped")

    -- <> command "active" (toParserInfo
    --     (Active <$> some (strArgument idVar))
    --     "List all currently worked on tasks")

    -- <> command "touch" (toParserInfo (TouchTask <$> strArgument idVar)
    --     "Update modified UTC")

    -- <> command "timer" (toParserInfo (TouchTask <$> strArgument idVar)
    --     "Show an overview of the currently active task \
    --     \with a timer of the past time since you started the task")
    )

  <|> subparser ( commandGroup (T.unpack $ fst shortcut_sec)

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

    <> command "pay" (toParserInfo (AddPay <$> some (strArgument
        (metavar "BODY" <> help "Body of the task")))
        "Pay for something")

    <> command "ship" (toParserInfo (AddShip <$> some (strArgument
        (metavar "BODY" <> help "Body of the task")))
        "Ship an item to someone")
    )

  <|> subparser ( commandGroup (T.unpack $ fst list_sec)

    <> command "head" (toParserInfo (pure ListHead)
        ("List " <> show (headCount conf)
          <> " most important open tasks by priority desc"))

    <> command "all" (toParserInfo (pure ListAll)
        "List all tasks by priority")

    <> command "open" (toParserInfo (pure ListOpen)
        "List all open tasks by creation UTC desc")

    -- All tasks due to no later than
    -- <> command "yesterday"
    -- <> command "today"
    -- <> command "tomorrow"

    -- <> command "monday"
    -- <> command "tuesday"
    -- <> command "wednesday"
    -- <> command "thursday"
    -- <> command "friday"
    -- <> command "saturday"
    -- <> command "sunday"

    -- <> command "month"  -- … last day of the month
    -- <> command "quarter"  -- … last day of the quarter
    -- <> command "year"  -- … last day of the year

    <> command "overdue" (toParserInfo (pure ListOverdue)
        "List all overdue tasks by priority desc")

    <> command "new" (toParserInfo (pure ListNew)
        ("List " <> show (headCount conf)
          <> " newest open tasks by creation UTC desc"))

    <> command "waiting" (toParserInfo (pure ListWaiting)
        "List all waiting tasks by priority")

    -- <> command "scheduled"
    --     "List tasks which have an earliest day to work on"


    <> command "done" (toParserInfo (pure ListDone)
        ("List " <> show (headCount conf)
          <> "  done tasks by closing UTC desc"))

    <> command "obsolete" (toParserInfo (pure ListObsolete)
        "List all obsolete tasks by closing UTC")

    <> command "deletable" (toParserInfo (pure ListDeletable)
        "List all deletable tasks by closing UTC")

    -- <> command "expired"
    -- "List tasks which are obsolete, \
    -- \because they crossed their expiration date"

    -- <> command "tagged" (toParserInfo (pure $ ListTagged)
    --     "List all tasks with a tag")

    <> command "notag" (toParserInfo (pure ListNoTag)
        "List tasks without any tags")

    <> command "withtag"
        (toParserInfo
          (ListWithTag <$> some
            (strArgument $ metavar "TAGS" <> help "The tags"))
          "List tasks which have all of the specified tags")

    -- TODO: Replace with tasks and tags commands
    <> command "query" (toParserInfo (QueryTasks <$> strArgument
        (metavar "QUERY" <> help "The SQL query after the \"where\" clause"))
        "Run \"select * from tasks where QUERY\" on the database")

    -- <> command "metadata" (toParserInfo (pure $ ListNoTag)
    --     "List all tasks with metadata")

    -- <> command "prioritized" (toParserInfo (pure $ ListNoTag)
    --     "List all tasks with an adjusted priority")

    -- <> command "tasks" (toParserInfo (QueryTasks <$> strArgument
    --     (metavar "QUERY" <> help "The SQL query after the \"where\" clause"))
    --     "Run \"select * from tasks where QUERY\" on the database")

    -- <> command "tags" (toParserInfo (QueryTasks <$> strArgument
    --     (metavar "QUERY" <> help "The SQL query after the \"where\" clause"))
    --     "Run \"select * from tasks where QUERY\" on the database")

    -- <> command "newest" "Show the newest task"
    -- <> command "oldest" "Show the oldest task"
    -- <> command "repeating" -- Open repeating tasks (soonest first)
    -- <> command "unblocked" -- Tasks that are not blocked (by priority)
    )

  <|> subparser ( commandGroup (T.unpack $ fst vis_sec)
    -- <> command "burndown" -- "Burndown chart by week"
    -- <> command "calendar" -- "Calendar view of all open tasks"
    -- <> command "history" -- "History of tasks"
    -- <> command "stats" -- "Statistics of all tasks"
    -- <> command "ulids" -- "List all ULIDs"

    <> command "tags" (toParserInfo (pure Tags)
        "List all used tags and their progress")

    -- <> command "active-tags" (toParserInfo (pure $ Tags)
    --     "List all active tags (a.k.a projects) and their progress")

    <> command "get"
        (toParserInfo
          (RunFilter <$> some
            (strArgument $ metavar "FILTER_EXP" <> help "Filter expressions"))
          "Get all tasks filtered by the specified expressions")
  )
  <|> subparser ( commandGroup (T.unpack $ fst i_o_sec)

    <> command "import" (toParserInfo (pure Import)
        "Import one JSON task from stdin")

    <> command "csv" (toParserInfo (pure Csv)
        "Show tasks in CSV format")

    <> command "runsql" (toParserInfo (RunSql <$> strArgument
        (metavar "QUERY" <> help "The SQL query"))
        "Run any SQL query and show result as CSV")

    <> command "ndjson" (toParserInfo (pure Ndjson)
        "Show tasks in NDJSON format")

    <> command "sql" (toParserInfo (pure Sql)
        "Show SQL commands to create and populate database")

    <> command "backup" (toParserInfo (pure Backup)
        "Create a backup of the tasks database at ~/tasklite/backups")
    )

  <|> subparser ( commandGroup (T.unpack $ fst advanced_sec)

    <> command "count" (toParserInfo (pure $ Count NoFilter)
        "Output total number of tasks")

    -- <> command "verify" (toParserInfo (pure Verify)
    --     "Verify the integrity of the database")

    <> command "version" (toParserInfo (pure Version) "Display version")

    <> command "help" (toParserInfo (pure Help) "Display current help page")
    )

  <|> subparser ( commandGroup (T.unpack $ fst unset_sec)

  --   <> command "unclose"
  --       "Delete closed UTC and delete Obsolete / Done state"

  --   <> command "untag"
  --       "Delete all tags"

  --   <> command "unnote"
  --       "Delete all notes"

    <> command "undue" (toParserInfo (UnDueTasks <$> some (strArgument idVar))
        "Delete due UTC")

  --   <> command "unwait"
  --       "Delete wait UTC"

  --   <> command "unprioritize"
  --       "Delete priority adjustment"

  --   <> command "unmeta"
  --       "Delete metadata"
    )

  <|> subparser ( commandGroup (T.unpack $ fst alias_sec)

    <> fold (fmap getCommand nameToAliasList)
    )

  <|> subparser ( commandGroup (T.unpack $ fst utils_sec)

    <> command "ulid2utc" (toParserInfo
        (UlidToUtc <$> strArgument (metavar "ULID" <> help "The ULID"))
        "Extract UTC timestamp from ULID")

    -- <> command "utc-yesterday"
    -- <> command "utc-today"
    -- <> command "utc-tomorrow"

    -- <> command "utc-monday"
    -- <> command "utc-tuesday"
    -- <> command "utc-wednesday"
    -- <> command "utc-thursday"
    -- <> command "utc-friday"
    -- <> command "utc-saturday"
    -- <> command "utc-sunday"

    -- <> command "utc-month"  -- … last day of the month
    -- <> command "utc-quarter"  -- … last day of the quarter
    -- <> command "utc-year"  -- … last day of the year
    )
  )


commandParserInfo :: ParserInfo Command
commandParserInfo =
  info
    (helper <*> commandParser)
    (noIntersperse
      <> briefDesc
      <> headerDoc (Just "{{header}}")
      <> progDescDoc (Just "{{examples}}")
      <> footerDoc (Just $ fromString $
            "Version " <> (showVersion version)
            <> ", developed by <adriansieber.com> at <feram.io>\n")
    )


groupBySpace :: Text -> [Doc ann]
groupBySpace =
  fmap pretty . T.groupBy (\a b ->
    isSpace a && isSpace b || not (isSpace a) && not (isSpace b))


replaceDoc :: Doc ann -> Doc ann -> [Doc ann] -> [Doc ann]
replaceDoc oldDoc newDoc =
  fmap $ \doc ->
    if (show oldDoc :: Text) == (show doc :: Text)
    then newDoc
    else doc


-- | Because optparse-applicative uses an old pretty printer,
-- | this function is necessary to splice new Docs into the old Docs
spliceDocsIntoText :: [(Text, Doc AnsiStyle)] -> Text -> [Doc AnsiStyle]
spliceDocsIntoText replacements renderedDoc =
  let
    docElems = groupBySpace renderedDoc
    replaceDocs (txt, doc) = replaceDoc (pretty txt) doc
  in
    foldr replaceDocs docElems replacements


examples :: Doc AnsiStyle
examples =
  let
    mkBold = annotate bold . pretty . T.justifyRight 26 ' '
    hiLite = (enclose "`" "`") . annotate (color Cyan)
  in ""
    <> hardline
    <> indent 2
      (  mkBold "Add an alias:" <+> hiLite "alias tl tasklite"
      <> hardline

      <> mkBold "Add a task with a tag:" <+> hiLite "tl add Buy milk +groceries"
      <> hardline

      <> mkBold "… or with the shortcut:" <+> hiLite "tl buy milk +groceries"
      <> hardline

      <> mkBold "List most important tasks:"
        <+> hiLite "tl"
        <+> parens ("same as" <+> hiLite "tl head")
      <> hardline

      <> mkBold "Complete it:" <+> hiLite "tl do <id>"
      )


helpReplacements :: [(Text, Doc AnsiStyle)]
helpReplacements =
  let
    prettyVersion = annotate (color Black) (pretty $ showVersion version)
  in (
    -- ("add", annotate (colorDull Green) "add BODY") :
    ("{{header}}",
        (annotate (bold <> color Blue) "TaskLite") <+> prettyVersion
        <> hardline <> hardline
        <> annotate (color Blue)
            "Task-list manager powered by Haskell and SQLite") :
    ("{{examples}}", examples) :
    fmap
      (\(a, b) -> (a, annotate (colorDull Yellow) (pretty b)))
      (
        basic_sec :
        shortcut_sec :
        list_sec :
        vis_sec :
        i_o_sec :
        advanced_sec :
        alias_sec :
        utils_sec :
        unset_sec :
      []))


helpText :: Doc AnsiStyle
helpText =
  let
    extendHelp theHelp = theHelp
      & show
      & spliceDocsIntoText helpReplacements
      & hcat
  in
    case (parserFailure defaultPrefs commandParserInfo ShowHelpText mempty) of
      ParserFailure a -> case a "tasklite" of
        (theHelp, _, _) -> extendHelp theHelp


main :: IO ()
main = do
  cliCommand <- execParser commandParserInfo

  connection <- setupConnection
  tableStatus <- createTables connection  -- TODO: Integrate into migrations
  migrationsStatus <- runMigrations connection
  nowElapsed <- timeCurrentP

  let
    now = timeFromElapsedP nowElapsed :: DateTime
    addTaskC = addTask connection
    prettyUlid ulid = pretty $ fmap
      (T.pack . timePrint (toFormat ("YYYY-MM-DD H:MI:S.ms" :: [Char])))
      (ulidTextToDateTime ulid)


  doc <- case cliCommand of
    ListAll -> listAll now connection
    ListHead -> headTasks now connection
    ListNew -> newTasks now connection
    ListOpen -> openTasks now connection
    ListOverdue -> overdueTasks now connection
    ListWaiting -> listWaiting now connection
    ListDone -> doneTasks now connection
    ListObsolete -> obsoleteTasks now connection
    ListDeletable -> deletableTasks now connection
    ListNoTag -> listNoTag now connection
    ListWithTag tags -> listWithTag now connection tags
    QueryTasks query -> queryTasks now connection query
    RunSql query -> runSql query
    RunFilter expressions -> runFilter now connection expressions
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
    AddPay bodyWords -> addTaskC $ ["Pay"] <> bodyWords <> ["+pay"]
    AddShip bodyWords -> addTaskC $ ["Ship"] <> bodyWords <> ["+ship"]
    LogTask bodyWords -> logTask connection bodyWords
    DoTasks ids -> doTasks connection ids
    EndTasks ids -> endTasks connection ids
    TrashTasks ids -> trashTasks connection ids
    DeleteTasks ids -> deleteTasks connection ids
    BoostTasks ids -> adjustPriority 1 ids
    HushTasks ids -> adjustPriority (-1) ids
    Start ids -> startTasks connection ids
    Stop ids -> stopTasks connection ids
    Prioritize val ids -> adjustPriority val ids
    InfoTask idSubstr -> infoTask connection idSubstr
    NextTask -> nextTask connection
    FindTask aPattern -> findTask connection aPattern
    AddTag tagText ids -> addTag connection tagText ids
    AddNote noteText ids -> addNote connection noteText ids
    SetDueUtc datetime ids -> setDueUtc connection datetime ids
    Duplicate ids -> duplicateTasks connection ids
    Count taskFilter -> countTasks taskFilter

    {- Unset -}
    UnDueTasks ids -> undueTasks connection ids

    Version -> pure $ pretty (showVersion version) <> hardline
    Help -> pure helpText
    Alias alias -> pure $ aliasWarning alias
    UlidToUtc ulid -> pure $ prettyUlid ulid

  -- TODO: Use withConnection instead
  close connection

  -- TODO: Remove color when piping into other command
  putDoc $ tableStatus <> migrationsStatus <> doc <> hardline

