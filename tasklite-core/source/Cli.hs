-- Necessary to print git hash in help output
-- and to embed example config file
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Replace case with maybe" #-}

module Cli where

import Protolude (
  Alternative (some, (<|>)),
  Applicative (pure, (<*>)),
  Bool (True),
  Char,
  Either (..),
  Eq ((==)),
  FilePath,
  Float,
  Foldable (foldr, null),
  Functor (fmap),
  IO,
  Maybe (..),
  Monad ((>>=)),
  Semigroup ((<>)),
  Show,
  Text,
  filter,
  foldMap,
  fst,
  getArgs,
  isPrefixOf,
  isSpace,
  not,
  optional,
  readFile,
  show,
  snd,
  ($),
  (&),
  (&&),
  (*),
  (-),
  (.),
  (<$>),
  (<&>),
  (||),
 )
import Protolude qualified as P

import AirGQL.Config qualified as AirGQL
import Control.Monad.Catch (catchAll)
import Data.Aeson as Aeson (KeyValue ((.=)), encode, object)
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.Hourglass (DateTime, Time (timeFromElapsedP))
import Data.String (fromString)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Time.ISO8601.Duration qualified as Iso
import Data.Version (showVersion)
import Database.SQLite.Simple (Connection (..))
import Database.SQLite.Simple qualified as SQLite
import GitHash (giDirty, giTag, tGitInfoCwd)
import Options.Applicative (
  ArgumentFields,
  CommandFields,
  Mod,
  ParseError (ShowHelpText),
  Parser,
  ParserHelp,
  ParserInfo,
  ParserResult (CompletionInvoked, Failure, Success),
  argument,
  auto,
  briefDesc,
  command,
  commandGroup,
  defaultPrefs,
  eitherReader,
  execParserPure,
  footerDoc,
  fullDesc,
  headerDoc,
  help,
  helpHeader,
  helper,
  hsubparser,
  idm,
  info,
  internal,
  long,
  maybeReader,
  metavar,
  noIntersperse,
  parserFailure,
  progDesc,
  progDescDoc,
  renderFailure,
  strArgument,
  switch,
 )
import Options.Applicative.Help.Chunk (Chunk (Chunk), (<<+>>))
import Options.Applicative.Help.Core (parserHelp)
import Paths_tasklite_core (version)
import Prettyprinter (
  Doc,
  LayoutOptions (layoutPageWidth),
  PageWidth (AvailablePerLine),
  Pretty (pretty),
  annotate,
  defaultLayoutOptions,
  dquotes,
  enclose,
  hardline,
  hcat,
  indent,
  layoutPretty,
  parens,
  (<+>),
 )
import Prettyprinter.Render.Terminal (
  AnsiStyle,
  Color (Black, Blue, Cyan, Red, Yellow),
  bold,
  color,
  hPutDoc,
  renderIO,
 )
import System.Console.Terminal.Size (Window (Window, height, width), size)
import System.Directory (
  Permissions,
  XdgDirectory (..),
  createDirectoryIfMissing,
  executable,
  getHomeDirectory,
  getPermissions,
  getXdgDirectory,
  listDirectory,
 )
import System.FilePath (hasExtension, (</>))
import System.IO (stdout)
import System.Process (readProcess)
import Time.System (timeCurrentP)

import Config (
  Config (..),
  HookSet (..),
  HooksConfig (..),
  addHookFilesToConfig,
  defaultConfig,
 )
import Control.Arrow ((>>>))
import Hooks (executeHooks, formatHookResult)
import ImportExport (
  backupDatabase,
  dumpCsv,
  dumpJson,
  dumpNdjson,
  dumpSql,
  editTask,
  enterTask,
  importDir,
  importEml,
  importFile,
  importJson,
  importMarkdown,
  importYaml,
  ingestDir,
  ingestFile,
 )
import Lib (
  addNote,
  addTag,
  addTask,
  adjustPriority,
  countTasks,
  deletableTasks,
  deleteNote,
  deleteTag,
  deleteTasks,
  doTasks,
  doneTasks,
  duplicateTasks,
  endTasks,
  findTask,
  getStats,
  headTasks,
  infoTask,
  listAll,
  listNoTag,
  listNotes,
  listOldTasks,
  listProjects,
  listReady,
  listRecurring,
  listRepeating,
  listTags,
  listWaiting,
  listWithTag,
  logTask,
  modifiedTasks,
  newTasks,
  nextTask,
  obsoleteTasks,
  openTasks,
  overdueTasks,
  queryTasks,
  randomTask,
  recurTasks,
  repeatTasks,
  reviewTasksIn,
  runFilter,
  runSql,
  setDueUtc,
  setReadyUtc,
  setupConnection,
  startTasks,
  stopTasks,
  trashTasks,
  uncloseTasks,
  undueTasks,
  unmetaTasks,
  unnoteTasks,
  unprioTasks,
  unreadyTasks,
  unrecurTasks,
  unrepeatTasks,
  unreviewTasks,
  untagTasks,
  unwaitTasks,
  unwakeTasks,
  waitFor,
  waitTasks,
 )
import Migrations (runMigrations)
import Server (startServer)
import System.Environment (getProgName, lookupEnv)
import Utils (
  IdText,
  ListModifiedFlag (AllItems, ModifiedItemsOnly),
  TagText,
  colr,
  colrDull,
  parseUtc,
  removeColorsIfNecessary,
  ulidText2utc,
  (<!!>),
 )


type String = [Char]


data Command
  = {- Add -}
    AddTask [Text]
  | AddWrite [Text]
  | AddRead [Text]
  | AddIdea [Text]
  | AddWatch [Text]
  | AddListen [Text]
  | AddBuy [Text]
  | AddSell [Text]
  | AddPay [Text]
  | AddShip [Text]
  | LogTask [Text]
  | EnterTask --
  {- Modify -}
  | ReadyOn DateTime [IdText]
  | WaitTasks [IdText]
  | WaitFor Iso.Duration [IdText]
  | ReviewTasks [IdText]
  | ReviewTasksIn Iso.Duration [IdText]
  | DoTasks [IdText]
  | DoOneTask IdText (Maybe [Text])
  | EndTasks [IdText]
  | EndOneTask IdText (Maybe [Text])
  | TrashTasks [IdText]
  | DeleteTasks [IdText]
  | RepeatTasks Iso.Duration [IdText]
  | RecurTasks Iso.Duration [IdText]
  | BoostTasks [IdText]
  | HushTasks [IdText] --
  {- Modify With Parameter -}
  | -- | Modify [IdText] Text -- DSL for modifying a task
    Prioritize Float [IdText]
  | AddTag TagText [IdText]
  | DeleteTag TagText [IdText]
  | AddNote Text [IdText]
  | DeleteNote IdText
  | SetDueUtc DateTime [IdText]
  | Start [IdText]
  | Stop [IdText]
  | Duplicate [IdText]
  | EditTask IdText -- Launch editor with YAML version of task
  {- Show -}
  | -- \| Append -- Append words to a task description
    -- \| Prepend -- Prepend words to a task description
    -- \| Undo -- Revert last change
    InfoTask IdText
  | NextTask
  | RandomTask (Maybe [Text])
  | FindTask Text --
  {- I/O -}
  | ImportFile FilePath
  | ImportDir FilePath
  | ImportJson
  | ImportYaml
  | ImportMarkdown
  | ImportEml
  | IngestFile FilePath
  | IngestDir FilePath
  | Csv
  | Json
  | Ndjson
  | Sql
  | Backup --
  {- List -}
  -- \| Fork -- Create new SQLite db with the tasks of the specified query
  | ListAll
  | ListHead
  | ListNewFiltered (Maybe [Text])
  | ListOld
  | ListOpen (Maybe [Text])
  | ListModified
  | ListModifiedOnly
  | ListDone
  | ListObsolete
  | ListDeletable
  | ListReady
  | ListWaiting
  | ListOverdue
  | ListRepeating
  | ListRecurring
  | ListNoTag
  | ListWithTag [Text]
  | CountFiltered (Maybe [Text])
  | QueryTasks Text
  | RunSql Text
  | RunFilter [Text] --
  -- \| Views -- List all available views
  | Tags -- List all used tags
  | Projects -- List all active tags
  | Notes -- List all notes
  | Stats -- List statistics about tasks
  {- Unset -}
  | -- \| Active -- Started tasks
    -- \| Blocked -- Tasks that are blocked by other tasks (newest first)
    -- \| Blockers -- Tasks that block other tasks (newest first)
    -- \| Unblocked -- Tasks that are not blocked
    UnCloseTasks [IdText]
  | UnDueTasks [IdText]
  | UnWaitTasks [IdText]
  | UnWakeTasks [IdText]
  | UnReadyTasks [IdText]
  | UnReviewTasks [IdText]
  | UnRepeatTasks [IdText]
  | UnRecurTasks [IdText]
  | UnTagTasks [IdText]
  | UnNoteTasks [IdText]
  | UnPrioTasks [IdText]
  | UnMetaTasks [IdText] --
  {- Misc -}
  | -- \| Demo -- Switch to demo mode
    Version -- Show version
  | -- \| License -- Show license
    Alias Text (Maybe [Text])
  | Help
  | PrintConfig
  | StartServer
  | UlidToUtc Text
  | ExternalCommand Text (Maybe [Text])
  deriving (Show, Eq)


data CliOptions = CliOptions
  { noColorFlag :: Bool
  , cliCommand :: Command
  }
  deriving (Show, Eq)


nameToAliasList :: [(Text, Text)]
nameToAliasList =
  [ ("annotate", "note")
  , ("clone", "duplicate")
  , ("close", "end")
  , ("decrease", "hush")
  , ("erase", "delete")
  , ("finish", "do")
  , ("fix", "do")
  , ("implement", "do")
  , ("inbox", "notag")
  , ("increase", "boost")
  , ("remove", "delete")
  , ("reopen", "unclose")
  , ("rm", "delete")
  , ("search", "find")
  , ("stop", "end")
  -- , ("week", "sunday")
  -- , ("latest", "newest")
  -- , ("schedule", "activate")
  -- , ("blocking", "blockers")
  -- , ("denotate", "denote")
  ]


{- Imitates output from `git describe` -}
versionSlug :: Text
versionSlug = do
  let
    gitInfo = $$tGitInfoCwd

  fromString $
    giTag gitInfo
      <> if giDirty gitInfo then "-dirty" else ""


aliasWarning :: Text -> Doc AnsiStyle
aliasWarning alias =
  "Invalid command."
    <+> "Use"
    <+> dquotes (pretty alias)
    <+> "instead."
      <> hardline


getCommand :: (Text, Text) -> Mod CommandFields Command
getCommand (alias, commandName) =
  command (T.unpack alias) $
    info
      (Alias commandName <$> optional (some $ strArgument idm))
      (progDesc $ T.unpack $ alias <> "-> " <> commandName)


toParserInfo :: Parser a -> Text -> ParserInfo a
toParserInfo parser description =
  info parser (fullDesc <> progDesc (T.unpack description))


idVar :: Mod ArgumentFields a
idVar =
  metavar "TASK_ID" <> help "Id of the task (Ulid)"


idsVar :: Mod ArgumentFields a
idsVar =
  metavar "TASK_ID ..." <> help "Ids of the tasks (Ulid)"


-- | Help Sections
basic_sec
  , shortcut_sec
  , list_sec
  , vis_sec
  , i_o_sec
  , advanced_sec
  , alias_sec
  , unset_sec
  , utils_sec ::
    (Text, Text)
basic_sec = ("{{basic_sec}}", "Basic Commands")
shortcut_sec = ("{{shortcut_sec}}", "Shortcuts to Add a Task")
list_sec = ("{{list_sec}}", "List Commands")
vis_sec = ("{{vis_sec}}", "Visualizations")
i_o_sec = ("{{i_o_sec}}", "I/O Commands")
advanced_sec = ("{{advanced_sec}}", "Advanced Commands")
alias_sec = ("{{alias_sec}}", "Aliases")
unset_sec = ("{{unset_sec}}", "Unset Commands")
utils_sec = ("{{utils_sec}}", "Utils")


parseDurationString :: String -> Either String Iso.Duration
parseDurationString text =
  text
    & fromString
    & Iso.parseDuration


cliOptionsParser :: Config -> Parser CliOptions
cliOptionsParser conf =
  CliOptions
    <$> switch
      ( long "no-color"
          <> help "Disable color output. Can be set via NO_COLOR env var."
      )
    <*> commandParser conf

{- FOURMOLU_DISABLE -}
commandParser :: Config -> Parser Command
commandParser conf =
  let
    numTasks = show conf.headCount
  in
  pure ListReady
  <|>
  (   hsubparser
    (  metavar (T.unpack $ snd basic_sec)
    <> commandGroup (T.unpack $ fst basic_sec)

    <> command "add" (toParserInfo (AddTask <$> some (strArgument
        (metavar "BODY" <> help "Body of the task" )))
        "Add a new task")

    -- <> command "prompt" (toParserInfo (pure AddInteractive)
    --     "Add a new task via an interactive prompt")

    <> command "log" (toParserInfo (LogTask <$> some (strArgument
        (metavar "BODY" <> help "Body of the task")))
        "Log an already completed task")

    <> command "enter" (toParserInfo (pure EnterTask)
        "Open your default editor with an empty task template")

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
        <*> optional (some (strArgument (metavar "CLOSING_NOTE"
              <> help "Final note to explain why and how it was done"))))
        "Mark a task as done and add optional closing note")

    <> command "doonly" (toParserInfo
        (DoOneTask <$> strArgument idsVar <*> pure Nothing)
        "Mark only one task as done")

    <> command "doall" (toParserInfo (DoTasks <$> some (strArgument idsVar))
        "Mark one or more tasks as done")

    <> command "end" (toParserInfo (EndOneTask
        <$> strArgument idsVar
        <*> optional (some (strArgument (metavar "CLOSING_NOTE"
              <> help "Final note to explain why and how it was closed"))))
        "Mark a task as obsolete and add optional closing note")

    <> command "endall" (toParserInfo (EndTasks <$> some (strArgument idsVar))
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
        "Repeat a task DURATION after it gets closed")

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

    <> command "random"
        (toParserInfo
          (RandomTask <$> optional ( some
            (strArgument $ metavar "FILTER_EXP" <> help "Filter expressions")))
              "Show a random open task \
              \from the tasks filtered by the specified expressions")

    <> command "find" (toParserInfo (FindTask <$> strArgument
        (metavar "PATTERN" <> help "Search pattern"))
        "Fuzzy search a task")

    <> command "tag" (toParserInfo (AddTag
      <$> strArgument (metavar "TAG" <> help "The tag")
      <*> some (strArgument idsVar))
      "Add a tag to specified tasks")

    <> command "deletetag" (toParserInfo (DeleteTag
      <$> strArgument (metavar "TAG" <> help "The tag")
      <*> some (strArgument idsVar))
      "Delete a tag from specified tasks")

    <> command "note" (toParserInfo (AddNote
      <$> strArgument (metavar "NOTE" <> help "The note")
      <*> some (strArgument idsVar))
      "Add a note to specified tasks")

    <> command "deletenote" (toParserInfo (DeleteNote
      <$> strArgument (metavar "ULID" <> help "The ULID of the note"))
      "Delete the specified note")

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

  <|> hsubparser
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

  <|> hsubparser
    (  metavar (T.unpack $ snd list_sec)
    <> commandGroup (T.unpack $ fst list_sec)

    <> command "head" (toParserInfo (pure ListHead)
        ("List " <> numTasks <> " most important open tasks by priority desc"))

    <> command "all" (toParserInfo (pure ListAll)
        "List all tasks by creation UTC asc")

    <> command "open"
        (toParserInfo
          (ListOpen <$> optional ( some
            (strArgument $ metavar "FILTER_EXP" <> help "Filter expressions")))
              "List all open tasks by priority desc \
              \filtered by the specified expression")

    <> command "modified" (toParserInfo (pure ListModified)
        "List all tasks by modified UTC desc")

    <> command "modifiedonly" (toParserInfo (pure ListModifiedOnly)
        "List tasks where modified UTC != creation UTC by modified UTC desc")

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

    <> command "recurring" (toParserInfo (pure ListRecurring)
        "List all recurring tasks by priority desc")

    <> command "new"
        (toParserInfo
          (ListNewFiltered <$> optional (some
            (strArgument $ metavar "FILTER_EXP" <> help "Filter expressions")))
          "List newest tasks by creation UTC desc (Open and Closed)")

    <> command "old" (toParserInfo (pure ListOld)
        ("List " <> numTasks <> " oldest open tasks by creation UTC asc"))

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
        ("List " <> numTasks <> " done tasks by closing UTC desc"))

    <> command "obsolete" (toParserInfo (pure ListObsolete)
        ("List " <> numTasks <> " obsolete tasks by closing UTC"))

    <> command "deletable" (toParserInfo (pure ListDeletable)
        ("List " <> numTasks <> " deletable tasks by closing UTC"))

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
          "Get all tasks filtered by the specified expression \
          \by priority")

    -- TODO: Replace with tasks and tags commands
    <> command "query" (toParserInfo (QueryTasks <$> strArgument
        (metavar "QUERY" <> help "The SQL query after the \"WHERE\" clause"))
        "Run \"SELECT * FROM tasks WHERE QUERY\" on the database")

    -- <> command "metadata" (toParserInfo (pure $ ListNoTag)
    --     "List all tasks with metadata")

    -- <> command "prioritized" (toParserInfo (pure $ ListNoTag)
    --     "List all tasks with an adjusted priority")

    -- <> command "tasks" (toParserInfo (QueryTasks <$> strArgument
    --     (metavar "QUERY" <> help "The SQL query after the \"where\" clause"))
    --     "Run \"SELECT * FROM tasks WHERE QUERY\" on the database")

    -- <> command "tags" (toParserInfo (QueryTasks <$> strArgument
    --     (metavar "QUERY" <> help "The SQL query after the \"where\" clause"))
    --     "Run \"SELECT * FROM tasks WHERE QUERY\" on the database")

    -- <> command "newest" "Show the newest task"
    -- <> command "oldest" "Show the oldest task"
    -- <> command "repeating" -- Open repeating tasks (soonest first)
    -- <> command "unblocked" -- Tasks that are not blocked (by priority)
    )

  <|> hsubparser
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

    <> command "notes" (toParserInfo (pure Notes)
        "List all notes descending by creation UTC")

    <> command "stats" (toParserInfo (pure Stats)
        "Show statistics about tasks")

    )

  <|> hsubparser
    (  metavar (T.unpack $ snd i_o_sec)
    <> commandGroup (T.unpack $ fst i_o_sec)

    <> command "import" (toParserInfo (ImportFile <$> strArgument
        (metavar "FILEPATH" <> help "Path to import file"))
        "Import a .json, .yaml, .md, or .eml file containing one task")

    <> command "importdir" (toParserInfo (ImportDir <$> strArgument
        (metavar "DIRECTORY_PATH" <> help "Path to directory"))
        "Import all .json, .yaml, .md, and .eml files in a directory")

    <> command "importjson" (toParserInfo (pure ImportJson)
        "Import one JSON object from stdin")

    <> command "importyaml" (toParserInfo (pure ImportYaml)
        "Import one YAML object from stdin")

    <> command "importmd" (toParserInfo (pure ImportMarkdown)
        "Import one Markdown file (with optional YAML front-matter) from stdin")

    <> command "importeml" (toParserInfo (pure ImportEml)
        "Import one email from stdin")

    <> command "ingest" (toParserInfo (IngestFile <$> strArgument
        (metavar "FILEPATH" <> help "Path to file"))
        ("Ingest a .json, .yaml, .md, or .eml file containing one task "
          <> "(import, open in editor, delete the original file)"))

    <> command "ingestdir" (toParserInfo (IngestDir <$> strArgument
        (metavar "DIRECTORY_PATH" <> help "Path to directory"))
        "Ingest all .json, .yaml, .md, and .eml files in a directory")

    <> command "csv" (toParserInfo (pure Csv)
        "Show tasks in CSV format")

    <> command "runsql" (toParserInfo (RunSql <$> strArgument
        (metavar "QUERY" <> help "The SQL query"))
        "Run any SQL query and show result as CSV")

    <> command "json" (toParserInfo (pure Json)
        "Show tasks in JSON format")

    <> command "ndjson" (toParserInfo (pure Ndjson)
        "Show tasks in NDJSON format")

    <> command "sql" (toParserInfo (pure Sql)
        "Show SQL commands to create and populate database")

    <> command "backup" (toParserInfo (pure Backup)
        ("Create a backup of the tasks database at "
        <> T.pack conf.dataDir <> "/backups"))

    )

  <|> hsubparser
    (  metavar (T.unpack $ snd advanced_sec)
    <> commandGroup (T.unpack $ fst advanced_sec)

    <> command "count"
        (toParserInfo
          (CountFiltered <$> optional ( some
            (strArgument $ metavar "FILTER_EXP" <> help "Filter expressions")))
        "Output total number of tasks filtered by the specified expressions")

    <> command "config" (toParserInfo (pure PrintConfig)
        "Print current configuration of TaskLite")

    <> command "server" (toParserInfo (pure StartServer)
        "Start an API server with several endpoints \
        \for data access and management \
        \(including a GraphQL endpoint powered by AirGQL)")

    -- <> command "verify" (toParserInfo (pure Verify)
    --     "Verify the integrity of the database")

    <> command "version" (toParserInfo (pure Version) "Display version")

    <> command "help" (toParserInfo (pure Help) "Display current help page")
    )

  <|> hsubparser
    (  metavar (T.unpack $ snd unset_sec)
    <> commandGroup (T.unpack $ fst unset_sec)

    <> command "unclose" (toParserInfo (UnCloseTasks
        <$> some (strArgument idsVar))
        "Erase closed timestamp and erase Done / Obsolete / Deletable state")

    <> command "undue" (toParserInfo (UnDueTasks
        <$> some (strArgument idsVar))
        "Erase due timestamp of specified tasks")

    <> command "unwait" (toParserInfo (UnWaitTasks
        <$> some (strArgument idsVar))
        "Erase wait timestamp of specified tasks")

    <> command "unwake" (toParserInfo (UnWakeTasks
        <$> some (strArgument idsVar))
        "Erase awake timestamp of specified tasks")

    <> command "unready" (toParserInfo (UnReadyTasks
        <$> some (strArgument idsVar))
        "Erase ready timestamp of specified tasks")

    <> command "unreview" (toParserInfo (UnReviewTasks
        <$> some (strArgument idsVar))
        "Erase review timestamp of specified tasks")

    <> command "unrepeat" (toParserInfo (UnRepeatTasks
        <$> some (strArgument idsVar))
        "Erase repetition duration of specified tasks")

    <> command "unrecur" (toParserInfo (UnRecurTasks
        <$> some (strArgument idsVar))
        "Erase recurrence duration of specified tasks")

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

  <|> hsubparser (internal <> foldMap getCommand nameToAliasList)

  <|> hsubparser
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

  -- Catch-all parser for any external "tasklite-???" command
  -- Do not show in help
  <|> ExternalCommand
        <$> strArgument P.mempty
        <*> optional (some (strArgument P.mempty))
  )

{- FOURMOLU_ENABLE -}


commandParserInfo :: Config -> ParserInfo CliOptions
commandParserInfo conf =
  let
    versionDesc =
      "Version "
        <> versionSlug
        <> ", developed by <adriansieber.com>"
        <> "\n"

    prettyVersion =
      annotate (colr conf Black) (pretty $ showVersion version)

    header =
      annotate (bold <> color Blue) "TaskLite"
        <+> prettyVersion
          <> hardline
          <> hardline
          <> annotate
            (colr conf Blue)
            "Task-list manager powered by Haskell and SQLite"

    examples = do
      let
        mkBold = annotate bold . pretty . T.justifyRight 26 ' '
        hiLite = enclose "`" "`" . annotate (colr conf Cyan)

      ""
        <> hardline
        <> indent
          2
          ( (mkBold "Add an alias:" <+> hiLite "alias tl tasklite")
              <> hardline
              <> ( mkBold "Add a task with a tag:"
                    <+> hiLite "tl add Buy milk +groceries"
                 )
              <> hardline
              <> ( mkBold "… or with the shortcut:"
                    <+> hiLite "tl buy milk +groceries"
                 )
              <> hardline
              <> ( mkBold "List most important tasks:"
                    <+> hiLite "tl"
                    <+> parens ("same as" <+> hiLite "tl ready")
                 )
              <> hardline
              <> (mkBold "Complete it:" <+> hiLite "tl do <id>")
          )
  in
    info
      (helper <*> cliOptionsParser conf)
      ( noIntersperse
          <> briefDesc
          <> headerDoc (Just header)
          <> progDescDoc (Just examples)
          <> footerDoc (Just $ fromString $ T.unpack versionDesc)
      )


groupBySpace :: Text -> [Doc ann]
groupBySpace =
  fmap pretty
    . T.groupBy
      ( \a b ->
          isSpace a && isSpace b || not (isSpace a) && not (isSpace b)
      )


replaceDoc :: Doc ann -> Doc ann -> [Doc ann] -> [Doc ann]
replaceDoc oldDoc newDoc =
  fmap $ \doc ->
    if (show oldDoc :: Text) == (show doc :: Text)
      then newDoc
      else doc


{-|
Because optparse-applicative does not support styling group headers,
this function is necessary to splice new Docs into the old Docs
TODO: Remove after https://github.com/pcapriotti/optparse-applicative/issues/485
-}
spliceDocsIntoText :: [(Text, Doc AnsiStyle)] -> Text -> [Doc AnsiStyle]
spliceDocsIntoText replacements renderedDoc = do
  let
    docElems = groupBySpace renderedDoc
    replaceDocs (txt, doc) = replaceDoc (pretty txt) doc

  foldr replaceDocs docElems replacements


helpReplacements :: Config -> [(Text, Doc AnsiStyle)]
helpReplacements conf =
  [ basic_sec
  , shortcut_sec
  , list_sec
  , vis_sec
  , i_o_sec
  , advanced_sec
  , alias_sec
  , utils_sec
  , unset_sec
  ]
    <&> (\(a, b) -> (a, annotate (colrDull conf Yellow) (pretty b <> ":")))


getHelpText :: String -> Config -> Doc AnsiStyle
getHelpText progName conf =
  parserFailure
    defaultPrefs
    (commandParserInfo conf)
    (ShowHelpText P.Nothing)
    []
    & P.flip renderFailure progName
    & P.fst
    & T.pack
    & spliceDocsIntoText (helpReplacements conf)
    & hcat


handleExternalCommand :: Config -> Text -> Maybe [Text] -> IO (Doc AnsiStyle)
handleExternalCommand conf cmd argsMb = do
  let
    args =
      argsMb & P.fromMaybe []

    runCmd = do
      output <-
        readProcess
          ("tasklite-" <> T.unpack cmd)
          (args <&> T.unpack)
          ""
      pure $ pretty output

    extendHelp :: ParserHelp -> Doc AnsiStyle
    extendHelp theHelp =
      theHelp
        & show
        & spliceDocsIntoText (helpReplacements conf)
        & hcat

    handleException exception = do
      hPutDoc P.stderr $
        if not $ exception & show & T.isInfixOf "does not exist"
          then pretty (show exception :: Text)
          else do
            let
              theHelp = parserHelp defaultPrefs (helper <*> cliOptionsParser conf)
              newHeader =
                Chunk
                  ( Just $
                      annotate (colr conf Red) $
                        "ERROR: Command \""
                          <> pretty cmd
                          <> "\" does not exist"
                  )
                  <<+>> helpHeader theHelp

            extendHelp theHelp{helpHeader = newHeader}
              <+> hardline
              <+> hardline

      P.exitFailure

  catchAll runCmd handleException


executeCLiCommand ::
  Config ->
  DateTime ->
  Connection ->
  String ->
  [String] ->
  Maybe P.Int ->
  IO (Doc AnsiStyle)
executeCLiCommand config now connection progName args availableLinesMb = do
  let cliCommandRes =
        execParserPure
          defaultPrefs
          (commandParserInfo config)
          args

  case cliCommandRes of
    CompletionInvoked _ ->
      P.die "Completion not implemented yet"
    --
    Failure failure -> do
      failure
        & P.flip renderFailure progName
        & P.fst
        & T.pack
        & spliceDocsIntoText (helpReplacements defaultConfig)
        & hcat
        & hPutDoc P.stderr
      P.exitFailure
    --
    Success cliOptions -> do
      conf <-
        removeColorsIfNecessary
          config{noColor = config.noColor || cliOptions.noColorFlag}
      let addTaskC = addTask conf connection

      case cliOptions.cliCommand of
        ListAll -> listAll conf now connection availableLinesMb
        ListHead -> headTasks conf now connection availableLinesMb
        ListNewFiltered taskFilter -> newTasks conf now connection taskFilter availableLinesMb
        ListOld -> listOldTasks conf now connection availableLinesMb
        ListOpen taskFilter -> openTasks conf now connection taskFilter availableLinesMb
        ListModified -> modifiedTasks conf now connection AllItems availableLinesMb
        ListModifiedOnly -> modifiedTasks conf now connection ModifiedItemsOnly availableLinesMb
        ListOverdue -> overdueTasks conf now connection availableLinesMb
        ListRepeating -> listRepeating conf now connection availableLinesMb
        ListRecurring -> listRecurring conf now connection availableLinesMb
        ListReady -> listReady conf now connection availableLinesMb
        ListWaiting -> listWaiting conf now connection availableLinesMb
        ListDone -> doneTasks conf now connection availableLinesMb
        ListObsolete -> obsoleteTasks conf now connection availableLinesMb
        ListDeletable -> deletableTasks conf now connection availableLinesMb
        ListNoTag -> listNoTag conf now connection availableLinesMb
        ListWithTag tags -> listWithTag conf now connection tags availableLinesMb
        QueryTasks query -> queryTasks conf now connection query
        RunSql query -> runSql conf query
        RunFilter expressions -> runFilter conf now connection expressions availableLinesMb
        Tags -> listTags conf connection
        Projects -> listProjects conf connection
        Notes -> listNotes conf connection
        Stats -> getStats conf connection
        ImportFile filePath -> importFile conf connection filePath
        ImportDir filePath -> importDir conf connection filePath
        ImportJson -> importJson conf connection
        ImportYaml -> importYaml conf connection
        ImportMarkdown -> importMarkdown conf connection
        ImportEml -> importEml conf connection
        IngestFile filePath -> ingestFile conf connection filePath
        IngestDir filePath -> ingestDir conf connection filePath
        Csv -> dumpCsv conf
        Json -> dumpJson conf
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
        EnterTask -> enterTask conf connection
        ReadyOn datetime ids -> setReadyUtc conf connection datetime ids
        WaitTasks ids -> waitTasks conf connection ids
        WaitFor duration ids -> waitFor conf connection duration ids
        ReviewTasks ids ->
          let days3 = Iso.DurationDate (Iso.DurDateDay (Iso.DurDay 3) Nothing)
          in  reviewTasksIn conf connection days3 ids
        ReviewTasksIn days ids -> reviewTasksIn conf connection days ids
        DoTasks ids -> doTasks conf connection Nothing ids
        DoOneTask id noteWords -> doTasks conf connection noteWords [id]
        EndTasks ids -> endTasks conf connection Nothing ids
        EndOneTask id noteWords -> endTasks conf connection noteWords [id]
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
        RandomTask taskFilter -> randomTask conf connection taskFilter
        FindTask aPattern -> findTask conf connection aPattern
        AddTag tagText ids -> addTag conf connection tagText ids
        DeleteTag tagText ids -> deleteTag conf connection tagText ids
        AddNote noteText ids -> addNote conf connection noteText ids
        DeleteNote id -> deleteNote conf connection id
        SetDueUtc datetime ids -> setDueUtc conf connection datetime ids
        Duplicate ids -> duplicateTasks conf connection ids
        CountFiltered taskFilter -> countTasks conf connection taskFilter
        {- Unset -}
        UnCloseTasks ids -> uncloseTasks conf connection ids
        UnDueTasks ids -> undueTasks conf connection ids
        UnWaitTasks ids -> unwaitTasks conf connection ids
        UnWakeTasks ids -> unwakeTasks conf connection ids
        UnReadyTasks ids -> unreadyTasks conf connection ids
        UnReviewTasks ids -> unreviewTasks conf connection ids
        UnRepeatTasks ids -> unrepeatTasks conf connection ids
        UnRecurTasks ids -> unrecurTasks conf connection ids
        UnTagTasks ids -> untagTasks conf connection ids
        UnNoteTasks ids -> unnoteTasks conf connection ids
        UnPrioTasks ids -> unprioTasks conf connection ids
        UnMetaTasks ids -> unmetaTasks conf connection ids
        Version -> pure $ pretty versionSlug <> hardline
        Help -> pure $ getHelpText progName conf
        PrintConfig -> pure $ pretty conf
        StartServer -> startServer AirGQL.defaultConfig conf
        Alias alias _ -> pure $ aliasWarning alias
        UlidToUtc ulid -> pure $ pretty $ ulidText2utc ulid
        ExternalCommand cmd argsMb -> handleExternalCommand conf cmd argsMb


printOutput :: String -> Maybe [String] -> Config -> IO ()
printOutput appName argsMb config = do
  noColorEnv <- lookupEnv "NO_COLOR"
  let conf = config{noColor = config.noColor || P.isJust noColorEnv}

  configNormDataDir <-
    if null conf.dataDir
      then do
        xdgDataDir <- getXdgDirectory XdgData appName
        pure $ conf{dataDir = xdgDataDir}
      else case T.stripPrefix "~/" $ T.pack conf.dataDir of
        Nothing -> pure conf
        Just rest -> do
          homeDir <- getHomeDirectory
          pure $ conf{dataDir = homeDir </> T.unpack rest}

  let hooksPath = configNormDataDir.hooks.directory

  configNormHookDir <-
    if null hooksPath
      then
        pure $
          configNormDataDir
            { hooks =
                configNormDataDir.hooks
                  { directory = configNormDataDir.dataDir </> "hooks"
                  }
            }
      else case T.stripPrefix "~/" $ T.pack hooksPath of
        Nothing -> pure configNormDataDir
        Just rest -> do
          homeDir <- getHomeDirectory
          pure $
            configNormDataDir
              { hooks =
                  configNormDataDir.hooks
                    { directory = homeDir </> T.unpack rest
                    }
              }

  let hooksPathNorm = configNormHookDir.hooks.directory

  createDirectoryIfMissing True hooksPathNorm

  hookFiles <- listDirectory hooksPathNorm

  hookFilesPerm :: [(FilePath, Permissions)] <-
    hookFiles
      & filter
        ( \name ->
            ("pre-" `isPrefixOf` name) || ("post-" `isPrefixOf` name)
        )
      <&> (hooksPathNorm </>)
      & P.mapM
        ( \path -> do
            perm <- getPermissions path
            pure (path, perm)
        )

  hookFilesPermContent <-
    hookFilesPerm
      & filter (\(filePath, perm) -> hasExtension filePath || executable perm)
      & P.mapM
        ( \(filePath, perm) -> do
            fileContent <- readFile filePath
            pure (filePath, perm, fileContent)
        )
  let (configNorm, errors) =
        addHookFilesToConfig configNormHookDir hookFilesPermContent
  P.when (not $ null errors) $
    ["WARNING:\n"]
      <> errors
      & P.traverse_
        ( pretty
            >>> annotate (colr conf Yellow)
            >>> hPutDoc P.stderr
        )

  -- Run pre-launch hooks
  preLaunchResults <- executeHooks "" configNorm.hooks.launch.pre
  let preLaunchHookMsg =
        preLaunchResults
          <&> \case
            Left error -> pretty error
            Right hookResult -> formatHookResult conf hookResult
          & P.fold

  connection <- setupConnection configNorm

  -- For debugging SQLite interactions
  -- SQLite.setTrace connection $ Just P.putStrLn

  migrationsStatus <- runMigrations configNorm connection

  -- Run post-launch hooks
  progName <- getProgName
  args <- case argsMb of
    Just args -> pure args
    Nothing -> getArgs
  postLaunchResults <-
    executeHooks
      ( TL.toStrict $
          TL.decodeUtf8 $
            Aeson.encode $
              object ["arguments" .= args]
      )
      configNorm.hooks.launch.post

  let postLaunchHookMsg =
        postLaunchResults
          <&> \case
            Left error -> pretty error
            Right hookResult -> formatHookResult conf hookResult
          & P.fold

  -- isTTY <- queryTerminal stdOutput
  -- if isTTY then size else 80
  termSizeMb <- size
  let
    linesNumMb =
      -- Ignore available terminal lines if output isn't printed to a terminal
      termSizeMb <&> \(Window{height}) ->
        -- TODO: Use the correct number of terminal prompt lines
        --       and overflowing lines here.
        --       We're currently simply assuming
        --       that 20% of the lines will overflow.
        P.max 1 $ P.round $ (P.fromIntegral height - 5) * (0.8 :: P.Double)

    termWidthMb =
      termSizeMb <&> \(Window{width}) -> width

    outputWidth =
      case (termWidthMb, configNorm.maxWidth) of
        (Just termWidth, Just maxWidth) -> P.min termWidth maxWidth
        (Just termWidth, Nothing) -> termWidth
        (Nothing, Just maxWidth) -> maxWidth
        (Nothing, Nothing) -> 80

  nowElapsed <- timeCurrentP
  let now = timeFromElapsedP nowElapsed :: DateTime

  doc <- executeCLiCommand configNorm now connection progName args linesNumMb

  -- TODO: Use withConnection instead
  SQLite.close connection

  -- Run pre-exit hooks
  preExitResults <- executeHooks "" configNorm.hooks.exit.pre
  let
    preExitHookMsg =
      preExitResults
        <&> \case
          Left error -> pretty error
          Right hookResult -> formatHookResult conf hookResult
        & P.fold
    putDocCustom document =
      renderIO
        stdout
        $ layoutPretty
          ( defaultLayoutOptions
              { layoutPageWidth = AvailablePerLine outputWidth 1.0
              }
          )
          document

  -- TODO: Remove color when piping into other command
  putDocCustom $
    preLaunchHookMsg
      <!!> migrationsStatus
      <!!> postLaunchHookMsg
      <!!> doc
      <!!> preExitHookMsg


exampleConfig :: Text
exampleConfig =
  $( makeRelativeToProject "example-config.yaml"
      >>= embedStringFile
   )
