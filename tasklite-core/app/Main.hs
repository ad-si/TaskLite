{-|
This is the main module which provides the CLI
-}

-- Necessary to print git hash in help output
-- and to embed example config file
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Protolude

import Lib
import Data.Char (isSpace)
import Data.FileEmbed (embedStringFile)
import Data.Hourglass
import Data.String (fromString)
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Text.Prettyprint.Doc.Render.Terminal
import qualified Data.Time.ISO8601.Duration as Iso
import Data.Version (showVersion)
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import GitHash
import Options.Applicative
import Paths_tasklite_core ()
import System.Directory
  ( createDirectoryIfMissing
  , getHomeDirectory
  , getXdgDirectory
  , XdgDirectory(..)
  )
import System.FilePath ((</>))
import Time.System
import Database.SQLite.Simple (close, Connection(..))

import Config (Config(..))
import DbSetup
import ImportExport
import Migrations
import Paths_tasklite_core (version)  -- Special module provided by Cabal
import Utils


data Command
  {- Add -}
  = AddTask   [Text]
  | AddWrite  [Text]
  | AddRead   [Text]
  | AddIdea   [Text]
  | AddWatch  [Text]
  | AddListen [Text]
  | AddBuy    [Text]
  | AddSell   [Text]
  | AddPay    [Text]
  | AddShip   [Text]
  | LogTask   [Text]

  {- Modify -}
  | ReadyOn DateTime [IdText]
  | WaitTasks [IdText]
  | WaitFor Iso.Duration [IdText]
  | ReviewTasks [IdText]
  | ReviewTasksIn Iso.Duration [IdText]
  | DoTasks [IdText]
  | DoOneTask IdText (Maybe [Text])
  | EndTasks [IdText]
  | TrashTasks [IdText]
  | DeleteTasks [IdText]
  | RepeatTasks Iso.Duration [IdText]
  | RecurTasks Iso.Duration [IdText]
  | BoostTasks [IdText]
  | HushTasks [IdText]
  -- | Modify [IdText] Text -- DSL for modifying a task

  {- Modify With Parameter -}
  | Prioritize Float [IdText]
  | AddTag TagText [IdText]
  | AddNote Text [IdText]
  | SetDueUtc DateTime [IdText]
  | Start [IdText]
  | Stop [IdText]
  | Duplicate [IdText]
  | EditTask IdText -- Launch editor with YAML version of task
  -- | Append -- Append words to a task description
  -- | Prepend -- Prepend words to a task description
  -- | Undo -- Revert last change

  {- Show -}
  | InfoTask IdText
  | NextTask
  | FindTask Text

  {- I/O -}
  | ImportFile FilePath
  | ImportJson
  | ImportEml
  | Csv
  | Ndjson
  | Sql
  | Backup
  -- | Fork -- Create new SQLite database with the tasks of the specified query

  {- List -}
  | ListAll
  | ListHead
  | ListNew
  | ListOld
  | ListOpen
  | ListDone
  | ListObsolete
  | ListDeletable
  | ListReady
  | ListWaiting
  | ListOverdue
  | ListRepeating
  | ListNoTag
  | ListWithTag [Text]
  | CountFiltered (Maybe [Text])
  | QueryTasks Text
  | RunSql Text
  | RunFilter [Text]
  -- | Views -- List all available views
  | Tags -- List all used tags
  | Projects -- List all active tags
  | Stats -- List statistics about tasks
  -- | Active -- Started tasks
  -- | Blocked -- Tasks that are blocked by other tasks (newest first)
  -- | Blockers -- Tasks that block other tasks (newest first)
  -- | Unblocked -- Tasks that are not blocked

  {- Unset -}
  | UnCloseTasks [IdText]
  | UnDueTasks [IdText]
  | UnWaitTasks [IdText]
  | UnWakeTasks [IdText]
  | UnReadyTasks [IdText]
  | UnRepeatTasks [IdText]
  | UnRecurTasks [IdText]
  | UnTagTasks [IdText]
  | UnNoteTasks [IdText]
  | UnPrioTasks [IdText]
  | UnMetaTasks [IdText]

  {- Misc -}
  -- | Demo -- Switch to demo mode
  | Version -- Show version
  -- | License -- Show license
  | Alias Text (Maybe [Text])
  | Help
  | PrintConfig
  | UlidToUtc Text

  deriving (Show, Eq)


nameToAliasList :: [(Text, Text)]
nameToAliasList = (
  ("annotate", "note") :
  ("clone", "duplicate") :
  ("close", "end") :
  ("decrease", "hush") :
  ("erase", "delete") :
  ("finish", "do") :
  ("fix", "do") :
  ("implement", "do") :
  ("inbox", "notag") :
  ("increase", "boost") :
  ("remove", "delete") :
  ("reopen", "unclose") :
  ("rm", "delete") :
  ("search", "find") :
  ("stop", "end") :
  -- ("week", "sunday") :
  -- ("latest", "newest") :
  -- ("schedule", "activate") :
  -- ("blocking", "blockers") :
  -- ("denotate", "denote") :
  [])


{- Imitates output from `git describe` -}
versionSlug :: Text
versionSlug =
  let
    gitInfo = $$tGitInfoCwd
  in
    fromString $
      (showVersion version)
      <> "+" <> take 8 (giHash gitInfo)
      <> (if giDirty gitInfo then "-dirty" else "")


aliasWarning :: Text -> Doc AnsiStyle
aliasWarning alias =
  "Invalid command."
    <+> "Use" <+> dquotes (pretty alias) <+> "instead."
    <> hardline


getCommand :: (Text, Text) -> Mod CommandFields Command
getCommand (alias, commandName) =
  command (T.unpack alias) $ info
    ((pure $ Alias commandName) <*> (optional $ some $ strArgument idm))
    (progDesc $ T.unpack $ alias <> "-> " <> commandName)


toParserInfo :: Parser a -> Text -> ParserInfo a
toParserInfo parser description =
  info (helper <*> parser) (fullDesc <> progDesc (T.unpack description))


idVar :: Mod ArgumentFields a
idVar =
  metavar "TASK_ID" <> help "Id of the task (Ulid)"

idsVar :: Mod ArgumentFields a
idsVar =
  metavar "TASK_ID ..." <> help "Ids of the tasks (Ulid)"


-- | Help Sections
basic_sec, shortcut_sec, list_sec,
  vis_sec, i_o_sec, advanced_sec,
  alias_sec, unset_sec, utils_sec
  :: (Text, Text)

basic_sec    = ("{{basic_sec}}", "Basic Commands")
shortcut_sec = ("{{shortcut_sec}}", "Shortcuts to Add a Task")
list_sec     = ("{{list_sec}}", "List Commands")
vis_sec      = ("{{vis_sec}}", "Visualizations")
i_o_sec      = ("{{i_o_sec}}", "I/O Commands")
advanced_sec = ("{{advanced_sec}}", "Advanced Commands")
alias_sec    = ("{{alias_sec}}", "Aliases")
unset_sec    = ("{{unset_sec}}", "Unset Commands")
utils_sec    = ("{{utils_sec}}", "Utils")


parseDurationString :: [Char] -> Either [Char] Iso.Duration
parseDurationString text = text
  & fromString
  & Iso.parseDuration


commandParser :: Config -> Parser Command
commandParser conf =
  let
    numTasks = show (headCount conf)
  in
  (pure ListReady)
  <|>
  (   subparser
    (  metavar (T.unpack $ snd basic_sec)
    <> commandGroup (T.unpack $ fst basic_sec)

    <> command "add" (toParserInfo (AddTask <$> some (strArgument
        (metavar "BODY" <> help "Body of the task")))
        "Add a new task")

    -- <> command "prompt" (toParserInfo (pure AddInteractive)
    --     "Add a new task via an interactive prompt")

    <> command "log" (toParserInfo (LogTask <$> some (strArgument
        (metavar "BODY" <> help "Body of the task")))
        "Log an already completed task")

    <> command "readyon" (toParserInfo (ReadyOn
      <$> argument (maybeReader (parseUtc . T.pack))
            (metavar "READY_UTC" <> help "Timestamp when task is ready")
      <*> some (strArgument idsVar))
        "Set ready UTC of tasks")

    <> command "wait" (toParserInfo (WaitTasks <$> some (strArgument idsVar))
        "Mark a task as waiting (e.g. for feedback) and review it in 3 days")

    <> command "waitfor" (toParserInfo (WaitFor
      <$> argument (eitherReader parseDurationString)
            (metavar "DURATION"
            <> help "ISO8601 duration (e.g. P1DT5H for 1 day and 5 hours)")
      <*> some (strArgument idsVar))
        "Wait DURATION until it's ready for review")

    <> command "review" (toParserInfo (ReviewTasks
        <$> some (strArgument idsVar))
        "Finish review and set new review date in 3 days")

    <> command "reviewin" (toParserInfo (ReviewTasksIn
      <$> argument (eitherReader parseDurationString)
            (metavar "DURATION"
            <> help "ISO8601 duration (e.g. P1DT5H for 1 day and 5 hours)")
      <*> some (strArgument idsVar))
        "Finish review and set new review date in DURATION")

    <> command "do" (toParserInfo (DoOneTask
        <$> strArgument idsVar
        <*> (optional $ some (strArgument (metavar "CLOSING_NOTE"
              <> help "Final note to explain why and how it was done"))))
        "Mark a task as done and add optional closing note")

    <> command "doonly" (toParserInfo
        (DoOneTask <$> strArgument idsVar <*> pure Nothing)
        "Mark only one task as done")

    <> command "doall" (toParserInfo (DoTasks <$> some (strArgument idsVar))
        "Mark one or more tasks as done")

    <> command "end" (toParserInfo (EndTasks <$> some (strArgument idsVar))
        "Mark a task as obsolete")

    <> command "edit" (toParserInfo (EditTask <$> strArgument idVar)
        "Edit YAML version of task in your $EDITOR")

    <> command "trash" (toParserInfo (TrashTasks <$> some (strArgument idsVar))
        "Mark a task as deletable")

    <> command "delete" (toParserInfo (DeleteTasks
        <$> some (strArgument idsVar))
        "Delete a task from the database (Attention: Irreversible)")

    <> command "repeat" (toParserInfo (RepeatTasks
      <$> argument (eitherReader parseDurationString)
            (metavar "DURATION"
            <> help "ISO8601 duration (e.g. P1DT5H for 1 day and 5 hours)")
      <*> some (strArgument idsVar))
        "Repeat a task x days after its due UTC or after it gets closed \
        \(whichever occurs later)")

    <> command "recur" (toParserInfo (RecurTasks
      <$> argument (eitherReader parseDurationString)
            (metavar "DURATION" <> help "ISO8601 duration \
              \(e.g. P1DT5H for 1 day and 5 hours)")
      <*> some (strArgument idsVar))
        "Recur a task DURATION after its due UTC")

    <> command "duplicate" (toParserInfo
        (Duplicate <$> some (strArgument idsVar))
        "Duplicates a task (and resets the closed and due UTC fields)")

    <> command "boost" (toParserInfo (BoostTasks <$> some (strArgument idsVar))
          "Increase priority of specified tasks by 1")

    <> command "hush" (toParserInfo (HushTasks <$> some (strArgument idsVar))
          "Decrease priority of specified tasks by 1")

    <> command "prioritize" (toParserInfo (Prioritize
          <$> argument auto (metavar "VALUE"
            <> help "Value to adjust priority by")
          <*> some (strArgument idsVar))
          "Adjust priority of specified tasks")

    -- <> command "snooze" (toParserInfo
    --     (SnoozeTasks <$> some (strArgument idsVar))
    --     "Add 1 day to awakening datetime")

    <> command "info" (toParserInfo (InfoTask <$> strArgument idVar)
        "Show detailed information and metadata of task")

    <> command "next" (toParserInfo (pure NextTask)
        "Show the task with the highest priority")

    <> command "find" (toParserInfo (FindTask <$> strArgument
        (metavar "PATTERN" <> help "Search pattern"))
        "Fuzzy search a task")

    <> command "tag" (toParserInfo (AddTag
      <$> strArgument (metavar "TAG" <> help "The tag")
      <*> some (strArgument idsVar))
      "Add a tag to specified tasks")

    <> command "note" (toParserInfo (AddNote
      <$> strArgument (metavar "NOTE" <> help "The note")
      <*> some (strArgument idsVar))
      "Add a note to specified tasks")

    <> command "due" (toParserInfo (SetDueUtc
      <$> argument (maybeReader (parseUtc . T.pack))
            (metavar "DUE_UTC" <> help "Due timestamp in UTC")
      <*> some (strArgument idsVar))
      "Set due UTC of specified tasks")

    <> command "start" (toParserInfo
        (Start <$> some (strArgument idsVar))
        "Add a note that work on task was started")

    <> command "stop" (toParserInfo
        (Stop <$> some (strArgument idsVar))
        "Add a note that work on task was stopped")

    -- <> command "active" (toParserInfo
    --     (Active <$> some (strArgument idsVar))
    --     "List all currently worked on tasks")

    -- <> command "touch" (toParserInfo (TouchTask <$> strArgument idVar)
    --     "Update modified UTC")

    -- <> command "timer" (toParserInfo (TouchTask <$> strArgument idVar)
    --     "Show an overview of the currently active task \
    --     \with a timer of the past time since you started the task")
    )

  <|> subparser
    (  metavar (T.unpack $ snd shortcut_sec)
    <> commandGroup (T.unpack $ fst shortcut_sec)

    <> command "write" (toParserInfo (AddWrite <$> some (strArgument
        (metavar "BODY" <> help "Body of the task")))
        "Write a message or a post")

    <> command "read" (toParserInfo (AddRead <$> some (strArgument
        (metavar "BODY" <> help "Url or title to a website or blog post")))
        "Read the specified URL")

    <> command "idea" (toParserInfo (AddIdea <$> some (strArgument
        (metavar "BODY" <> help "Description of your idea")))
        "Quickly capture an idea")

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

  <|> subparser
    (  metavar (T.unpack $ snd list_sec)
    <> commandGroup (T.unpack $ fst list_sec)

    <> command "head" (toParserInfo (pure ListHead)
        ("List " <> numTasks <> " most important open tasks by priority desc"))

    <> command "all" (toParserInfo (pure ListAll)
        "List all tasks by creation UTC asc")

    <> command "open" (toParserInfo (pure ListOpen)
        "List all open tasks by priority desc")

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

    <> command "repeating" (toParserInfo (pure ListRepeating)
        "List all repeating tasks by priority desc")

    <> command "new" (toParserInfo (pure ListNew)
        ("List " <> numTasks
          <> " newest open tasks by creation UTC desc"))

    <> command "old" (toParserInfo (pure ListOld)
        ("List " <> numTasks
          <> " oldest open tasks by creation UTC asc"))

    -- <> command "asleep" (toParserInfo (pure ListAwake)
    --     "List all sleeping tasks by priority")

    -- <> command "awake" (toParserInfo (pure ListAwake)
    --     "List all awake tasks by priority")

    <> command "ready" (toParserInfo (pure ListReady)
       ( "List " <> numTasks <> " most important ready tasks by priority desc"))

    <> command "waiting" (toParserInfo (pure ListWaiting)
        "List all waiting tasks by priority")

    -- <> command "scheduled"
    --     "List tasks which have an earliest day to work on"


    <> command "done" (toParserInfo (pure ListDone)
        ("List " <> numTasks
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

    <> command "get"
        (toParserInfo
          (RunFilter <$> some
            (strArgument $ metavar "FILTER_EXP" <> help "Filter expressions"))
          "Get all tasks filtered by the specified expressions \
          \by priority")

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

  <|> subparser
    (  metavar (T.unpack $ snd vis_sec)
    <> commandGroup (T.unpack $ fst vis_sec)
    -- <> command "kanban" -- "List tasks columnized by state"
    -- <> command "burndown" -- "Burndown chart by week"
    -- <> command "calendar" -- "Calendar view of all open tasks"
    -- <> command "history" -- "History of tasks"
    -- <> command "stats" -- "Statistics of all tasks"
    -- <> command "ulids" -- "List all ULIDs"

    <> command "tags" (toParserInfo (pure Tags)
        "List all used tags and their progress")

    <> command "projects" (toParserInfo (pure Projects)
        "List all active tags (a.k.a projects) and their progress")

    <> command "stats" (toParserInfo (pure Stats)
        "Show statistics about tasks")

    )

  <|> subparser
    (  metavar (T.unpack $ snd i_o_sec)
    <> commandGroup (T.unpack $ fst i_o_sec)

    <> command "import" (toParserInfo (ImportFile <$> strArgument
        (metavar "FILEPATH" <> help "Path to import file"))
        "Import a .json or .eml file containing one task")

    <> command "importjson" (toParserInfo (pure ImportJson)
        "Import one JSON object from stdin")

    <> command "importeml" (toParserInfo (pure ImportEml)
        "Import one email from stdin")

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

  <|> subparser
    (  metavar (T.unpack $ snd advanced_sec)
    <> commandGroup (T.unpack $ fst advanced_sec)

    <> command "count"
        (toParserInfo
          (CountFiltered <$> (optional $ some
            (strArgument $ metavar "FILTER_EXP" <> help "Filter expressions")))
        "Output total number of tasks filtered by the specified expressions")

    <> command "config" (toParserInfo (pure PrintConfig)
        "Print current configuration of TaskLite")

    -- <> command "verify" (toParserInfo (pure Verify)
    --     "Verify the integrity of the database")

    <> command "version" (toParserInfo (pure Version) "Display version")

    <> command "help" (toParserInfo (pure Help) "Display current help page")
    )

  <|> subparser
    (  metavar (T.unpack $ snd unset_sec)
    <> commandGroup (T.unpack $ fst unset_sec)

    <> command "unclose" (toParserInfo (UnCloseTasks
        <$> some (strArgument idsVar))
        "Erase closed timestamp and erase Done / Obsolete / Deletable state")

    <> command "undue" (toParserInfo (UnDueTasks
        <$> some (strArgument idsVar))
        "Erase due timestamp for specified tasks")

    <> command "unwait" (toParserInfo (UnWaitTasks
        <$> some (strArgument idsVar))
        "Erase wait timestamp for specified tasks")

    <> command "unwake" (toParserInfo (UnWakeTasks
        <$> some (strArgument idsVar))
        "Erase awake timestamp for specified tasks")

    <> command "unready" (toParserInfo (UnReadyTasks
        <$> some (strArgument idsVar))
        "Erase ready timestamp for specified tasks")

    <> command "unrepeat" (toParserInfo (UnRepeatTasks
        <$> some (strArgument idsVar))
        "Erase repetition duration for specified tasks")

    <> command "unrecur" (toParserInfo (UnRecurTasks
        <$> some (strArgument idsVar))
        "Erase recurrence duration for specified tasks")

    <> command "untag" (toParserInfo (UnTagTasks
        <$> some (strArgument idsVar))
        "Erase all tags")

    <> command "unnote" (toParserInfo (UnNoteTasks
        <$> some (strArgument idsVar))
        "Erase all notes")

    <> command "unprio" (toParserInfo (UnPrioTasks
        <$> some (strArgument idsVar))
        "Erase manual priority adjustment")

    <> command "unmeta" (toParserInfo (UnMetaTasks
        <$> some (strArgument idsVar))
        "Erase metadata")
    )

  <|> subparser (internal <> fold (fmap getCommand nameToAliasList))

  <|> subparser
    ( metavar (T.unpack $ snd utils_sec)
    <> commandGroup (T.unpack $ fst utils_sec)

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


commandParserInfo :: Config -> ParserInfo Command
commandParserInfo conf =
  let
    versionDesc =
      "Version "
      <> versionSlug
      <> ", developed by <adriansieber.com>"
  in
    info
      (helper <*> commandParser conf)
      (noIntersperse
        <> briefDesc
        <> headerDoc (Just "{{header}}")
        <> progDescDoc (Just "{{examples}}")
        <> footerDoc (Just $ fromString $ T.unpack versionDesc)
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
        <+> parens ("same as" <+> hiLite "tl ready")
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
      (\(a, b) -> (a, annotate (colorDull Yellow) (pretty b <> ":")))
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


helpText :: Config -> Doc AnsiStyle
helpText conf =
  let
    extendHelp theHelp = theHelp
      & show
      & spliceDocsIntoText helpReplacements
      & hcat
  in
    case
      (parserFailure defaultPrefs (commandParserInfo conf) ShowHelpText mempty)
    of
      ParserFailure a -> case a "tasklite" of
        (theHelp, _, _) -> extendHelp theHelp


executeCLiCommand ::
  Config -> DateTime -> Connection -> Command -> IO (Doc AnsiStyle)
executeCLiCommand conf now connection cmd =
  let
    addTaskC = addTask conf connection
    prettyUlid ulid = pretty $ fmap
      (T.pack . timePrint (toFormat ("YYYY-MM-DD H:MI:S.ms" :: [Char])))
      (ulidTextToDateTime ulid)
    days3 = Iso.DurationDate (Iso.DurDateDay (Iso.DurDay 3) Nothing)

  in case cmd of
    ListAll -> listAll conf now connection
    ListHead -> headTasks conf now connection
    ListNew -> newTasks conf now connection
    ListOld -> listOldTasks conf now connection
    ListOpen -> openTasks conf now connection
    ListOverdue -> overdueTasks conf now connection
    ListRepeating -> listRepeating conf now connection
    ListReady -> listReady conf now connection
    ListWaiting -> listWaiting conf now connection
    ListDone -> doneTasks conf now connection
    ListObsolete -> obsoleteTasks conf now connection
    ListDeletable -> deletableTasks conf now connection
    ListNoTag -> listNoTag conf now connection
    ListWithTag tags -> listWithTag conf now connection tags
    QueryTasks query -> queryTasks conf now connection query
    RunSql query -> runSql conf query
    RunFilter expressions -> runFilter conf now connection expressions
    Tags -> listTags conf connection
    Projects -> listProjects conf connection
    Stats -> getStats conf connection
    ImportFile filePath -> importFile conf connection filePath
    ImportJson -> importJson conf connection
    ImportEml -> importEml conf connection
    Csv -> dumpCsv conf
    Ndjson -> dumpNdjson conf
    Sql -> dumpSql conf
    Backup -> backupDatabase conf
    AddTask bodyWords -> addTaskC bodyWords
    AddWrite bodyWords -> addTaskC $ ["Write"] <> bodyWords <> ["+write"]
    AddRead bodyWords -> addTaskC $ ["Read"] <> bodyWords <> ["+read"]
    AddIdea bodyWords -> addTaskC $ bodyWords <> ["+idea"]
    AddWatch bodyWords -> addTaskC $ ["Watch"] <> bodyWords <> ["+watch"]
    AddListen bodyWords -> addTaskC $ ["Listen"] <> bodyWords <> ["+listen"]
    AddBuy bodyWords -> addTaskC $ ["Buy"] <> bodyWords <> ["+buy"]
    AddSell bodyWords -> addTaskC $ ["Sell"] <> bodyWords <> ["+sell"]
    AddPay bodyWords -> addTaskC $ ["Pay"] <> bodyWords <> ["+pay"]
    AddShip bodyWords -> addTaskC $ ["Ship"] <> bodyWords <> ["+ship"]
    LogTask bodyWords -> logTask conf connection bodyWords
    ReadyOn datetime ids -> setReadyUtc conf connection datetime ids
    WaitTasks ids -> waitTasks conf connection ids
    WaitFor duration ids -> waitFor conf connection duration ids
    ReviewTasks ids -> reviewTasksIn conf connection days3 ids
    ReviewTasksIn days ids -> reviewTasksIn conf connection days ids
    DoTasks ids -> doTasks conf connection Nothing ids
    DoOneTask id noteWords -> doTasks conf connection noteWords [id]
    EndTasks ids -> endTasks conf connection ids
    EditTask id -> editTask conf connection id
    TrashTasks ids -> trashTasks conf connection ids
    DeleteTasks ids -> deleteTasks conf connection ids
    RepeatTasks duration ids -> repeatTasks conf connection duration ids
    RecurTasks duration ids -> recurTasks conf connection duration ids
    BoostTasks ids -> adjustPriority conf 1 ids
    HushTasks ids -> adjustPriority conf (-1) ids
    Start ids -> startTasks conf connection ids
    Stop ids -> stopTasks conf connection ids
    Prioritize val ids -> adjustPriority conf val ids
    InfoTask idSubstr -> infoTask conf connection idSubstr
    NextTask -> nextTask conf connection
    FindTask aPattern -> findTask connection aPattern
    AddTag tagText ids -> addTag conf connection tagText ids
    AddNote noteText ids -> addNote conf connection noteText ids
    SetDueUtc datetime ids -> setDueUtc conf connection datetime ids
    Duplicate ids -> duplicateTasks conf connection ids
    CountFiltered taskFilter -> countTasks conf connection taskFilter

    {- Unset -}
    UnCloseTasks ids -> uncloseTasks conf connection ids
    UnDueTasks ids -> undueTasks conf connection ids
    UnWaitTasks ids -> unwaitTasks conf connection ids
    UnWakeTasks ids -> unwakeTasks conf connection ids
    UnReadyTasks ids -> unreadyTasks conf connection ids
    UnRepeatTasks ids -> unrepeatTasks conf connection ids
    UnRecurTasks ids -> unrecurTasks conf connection ids
    UnTagTasks ids -> untagTasks conf connection ids
    UnNoteTasks ids -> unnoteTasks conf connection ids
    UnPrioTasks ids -> unprioTasks conf connection ids
    UnMetaTasks ids -> unmetaTasks conf connection ids

    Version -> pure $ pretty versionSlug <> hardline
    Help -> pure $ helpText conf
    PrintConfig -> pure $ pretty conf
    Alias alias _ -> pure $ aliasWarning alias
    UlidToUtc ulid -> pure $ prettyUlid ulid


printOutput :: [Char] -> Config -> IO ()
printOutput appName configUser = do
  configUserNorm <-
    if (dataDir configUser /= "")
    then pure $ configUser
    else do
     xdgDataDir <- getXdgDirectory XdgData appName
     pure $ configUser {dataDir = xdgDataDir}

  config <- case (T.stripPrefix "~/" $ T.pack $ dataDir configUserNorm) of
              Nothing ->
                pure $ configUser {dataDir = dataDir configUserNorm}
              Just rest -> do
                homeDir <- getHomeDirectory
                pure $ configUser { dataDir = homeDir </> T.unpack rest }

  cliCommand <- execParser $ commandParserInfo config

  connection <- setupConnection config
  -- TODO: Integrate into migrations
  tableStatus <- createTables config connection
  migrationsStatus <- runMigrations config connection
  nowElapsed <- timeCurrentP

  let
    now = timeFromElapsedP nowElapsed :: DateTime

  doc <- executeCLiCommand config now connection cliCommand

  -- TODO: Use withConnection instead
  close connection

  -- TODO: Remove color when piping into other command
  putDoc $ tableStatus <> migrationsStatus <> doc <> hardline


exampleConfig :: Text
exampleConfig = $(
    makeRelativeToProject "example-config.yaml"
    >>= embedStringFile
  )


main :: IO ()
main = do
  -- Necessary for Docker image
  setLocaleEncoding utf8

  let appName = "tasklite"

  configDirectory <- getXdgDirectory XdgConfig appName
  createDirectoryIfMissing True configDirectory

  let configPath = configDirectory </> "config.yaml"

  configResult <- decodeFileEither configPath

  case configResult of
    Left error -> do
      if "not found" `T.isInfixOf` (T.pack $ prettyPrintParseException error)
      then do
        writeFile configPath exampleConfig
        configResult2 <- decodeFileEither configPath

        case configResult2 of
          Left error2 -> die $ T.pack $ prettyPrintParseException error2
          Right configUser -> printOutput appName configUser
      else
        die $ T.pack $ prettyPrintParseException error

    Right configUser ->
      printOutput appName configUser
