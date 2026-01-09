{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use maybe" #-}

{-|
Functions to create, update, and delete tasks / tags / notes
-}
module Lib where

import Protolude (
  Applicative (liftA2, pure),
  Bool (..),
  Char,
  Double,
  Down (Down),
  Either (Left, Right),
  Eq (..),
  FilePath,
  Float,
  Floating (logBase),
  Fractional ((/)),
  Functor (fmap),
  IO,
  Int,
  Int64,
  Integer,
  Integral (toInteger),
  Maybe (..),
  Monad (return, (>>=)),
  MonadIO (liftIO),
  Monoid (mempty),
  Num (abs, fromInteger, (*), (+), (-)),
  Ord (compare, max, (<), (>)),
  Read,
  RealFrac (ceiling, floor),
  Semigroup ((<>)),
  Show,
  Text,
  catMaybes,
  const,
  decodeUtf8,
  either,
  encodeUtf8,
  exitFailure,
  forM,
  forM_,
  fromIntegral,
  fromMaybe,
  fst,
  getArgs,
  identity,
  isJust,
  isNothing,
  isSpace,
  listToMaybe,
  maybe,
  maybeToEither,
  not,
  on,
  otherwise,
  realToFrac,
  show,
  snd,
  sortBy,
  sortOn,
  stderr,
  unlines,
  unwords,
  ($),
  ($>),
  (&),
  (&&),
  (.),
  (<$>),
  (<&>),
  (<=),
  (||),
 )
import Protolude qualified as P

import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Data.Aeson as Aeson (KeyValue ((.=)), encode, object)
import Data.Char (isDigit)
import Data.Coerce (coerce)
import Data.Generics (Data, constrFields, toConstr)
import Data.Hourglass (
  DateTime (dtTime),
  Duration (durationHours, durationMinutes),
  ISO8601_Date (ISO8601_Date),
  Minutes (Minutes),
  Seconds (Seconds),
  Time (timeFromElapsedP),
  TimeOfDay (todNSec),
  timeAdd,
  timeDiff,
  timePrint,
 )
import Data.List (nub)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Time.Clock (UTCTime)
import Data.Time.ISO8601.Duration qualified as Iso
import Data.ULID (ULID, getULID)
import Data.Yaml qualified as Yaml
import Database.SQLite.Simple (
  Connection,
  Error (ErrorConstraint),
  FromRow (..),
  NamedParam ((:=)),
  Only (Only),
  Query (Query),
  SQLData (SQLNull, SQLText),
  SQLError (sqlError),
  ToRow,
  changes,
  execute,
  executeNamed,
  execute_,
  field,
  open,
  query,
  queryNamed,
  query_,
  toRow,
  withConnection,
 )
import Database.SQLite.Simple.QQ (sql)
import Numeric (showFFloat)
import Prettyprinter as Pp (
  Doc,
  Pretty (pretty),
  align,
  annotate,
  concatWith,
  dquotes,
  fill,
  hang,
  hardline,
  hsep,
  indent,
  line,
  punctuate,
  surround,
  vcat,
  vsep,
  (<+>),
 )
import Prettyprinter.Render.Terminal (
  AnsiStyle,
  Color (Black, Green, Red, Yellow),
  hPutDoc,
 )
import Prettyprinter.Util (reflow)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Posix.User (getEffectiveUserName)
import System.Process (readProcess)
import Text.Fuzzily qualified as Fuzzily
import Text.ParserCombinators.ReadP (
  ReadP,
  char,
  eof,
  munch,
  munch1,
  readP_to_S,
  sepBy1,
  skipSpaces,
  string,
  (<++),
 )
import Text.Printf (printf)
import Time.System (dateCurrent, timeCurrentP)

import Config (
  Column (..),
  Config (
    bodyClosedStyle,
    bodyStyle,
    bodyWidth,
    closedStyle,
    columns,
    dataDir,
    dateStyle,
    dateWidth,
    dbName,
    dueStyle,
    headCount,
    hooks,
    idStyle,
    prioWidth,
    priorityStyle,
    progressBarWidth,
    tableName,
    tagStyle,
    tagsWidth,
    utcFormat,
    utcFormatShort
  ),
  HookSet (post, pre),
  HooksConfig (add),
  defaultConfig,
 )
import Control.Monad.Catch (catchAll, catchIf)
import FullTask (
  FullTask (
    awake_utc,
    body,
    closed_utc,
    due_utc,
    group_ulid,
    metadata,
    modified_utc,
    notes,
    priority,
    ready_utc,
    recurrence_duration,
    repetition_duration,
    review_utc,
    tags,
    ulid,
    user,
    waiting_utc
  ),
  cpTimesAndState,
  selectQuery,
 )
import Hooks (HookResult (message, task), executeHooks, formatHookResult)
import ImportTask (
  ImportTask (ImportTask, closedUtcWasExplicit, notes, tags, task),
  importTaskToFullTask,
  setMissingFields,
 )
import Note (Note (body, ulid))
import Prettyprinter.Internal.Type (Doc (Empty))
import SqlUtils (quoteKeyword, quoteText)
import Task (
  DerivedState (IsOpen),
  Task,
  TaskState (..),
  derivedStateToQuery,
  emptyTask,
  getStateHierarchy,
  textToDerivedState,
 )
import Task qualified
import TaskToNote (TaskToNote (..))
import TaskToTag (TaskToTag (..))
import Utils (
  IdText,
  ListModifiedFlag (..),
  applyColorMode,
  bgColrDull,
  colr,
  colrDull,
  countChar,
  dateTimeToUtcTime,
  formatElapsedP,
  maybeBold,
  maybeUnderlined,
  numDigits,
  parseUlidText,
  parseUtc,
  setDateTime,
  ulidTextToDateTime,
  utcFormatReadable,
  utcTimeToDateTime,
  vsepCollapse,
  (<$$>),
  (<++>),
 )


noTasksWarning :: Text
noTasksWarning =
  "No tasks available. "
    <> "Run `tasklite help` to learn how to create tasks."


newtype NumRows = NumRows Integer
  deriving (Eq, Ord, Read, Show)


instance FromRow NumRows where
  fromRow = NumRows <$> field


newtype SqliteStat = SqliteStat Text
  deriving (Eq, Show)


instance FromRow SqliteStat where
  fromRow = SqliteStat <$> field


{-| Get the total number of tasks from sqlite_stat1 statistics table
with a fallback value if the table doesn't exist or can't be parsed.
-}
getTotalTaskCount :: Connection -> IO Integer
getTotalTaskCount connection = do
  let
    fallbackNumTasks = 500
    queryStats =
      query_
        connection
        "SELECT stat FROM sqlite_stat1 WHERE tbl = 'tasks' LIMIT 1"

  result <- catchAll queryStats (\_ -> pure [])
  pure $ fromMaybe fallbackNumTasks $ do
    SqliteStat stat <- listToMaybe result
    countText <- listToMaybe $ T.words stat
    P.readMaybe $ T.unpack countText


getDbPath :: Config -> IO FilePath
getDbPath conf = do
  pure $ conf.dataDir </> conf.dbName


setupConnection :: Config -> IO Connection
setupConnection conf = do
  createDirectoryIfMissing True $ dataDir conf
  open $ conf.dataDir </> conf.dbName


execWithConn :: Config -> (Connection -> IO a) -> IO a
execWithConn conf func = do
  createDirectoryIfMissing True $ dataDir conf
  withConnection
    (conf.dataDir </> conf.dbName)
    func


-- | Get fields names of record (empty list if not record constructor)
getRecordFields :: (Data object) => object -> [Text]
getRecordFields =
  toConstr >>> constrFields >>> fmap T.pack


insertRecord :: (ToRow r, Data r) => Text -> Connection -> r -> IO ()
insertRecord tableName connection record = do
  let recordFields = record & getRecordFields
  execute
    connection
    ( Query $
        "INSERT INTO "
          <> tableName
          <> "("
          <> (recordFields & T.intercalate ",\n")
          <> ") VALUES ("
          <> ((recordFields $> "?") & T.intercalate ",\n")
          <> ")"
    )
    (toRow record)


getUpdateAssignments :: Task -> Text
getUpdateAssignments task =
  task
    & getRecordFields
    <&> (<> " = ?")
    & T.intercalate ", "


updateTask :: Connection -> Task -> IO ()
updateTask connection task = do
  execute
    connection
    ( Query $
        " UPDATE tasks SET "
          <> getUpdateAssignments task
          <> " WHERE ulid == ?"
    )
    (toRow task <> [SQLText task.ulid])


handleTagDupError :: Config -> Text -> (Applicative f) => e -> f (Doc AnsiStyle)
handleTagDupError conf tag _exception =
  pure $
    annotate (colr conf Yellow) $
      "⚠️ Tag " <> dquotes (pretty tag) <> " is already assigned"


insertTags ::
  Config -> Connection -> Maybe DateTime -> Task -> [Text] -> IO (Doc AnsiStyle)
insertTags conf connection mbCreatedUtc task tags = do
  let uniqueTags = nub tags
  taskToTags <- forM uniqueTags $ \tag -> do
    newUlid <- getULID
    pure $
      TaskToTag
        { ulid =
            mbCreatedUtc
              <&> setDateTime newUlid
              & fromMaybe newUlid
              & show
              & T.toLower
        , task_ulid = task.ulid
        , tag = tag
        }

  -- TODO: Insert all tags at once
  insertWarnings <- P.forM taskToTags $ \taskToTag ->
    catchIf
      -- TODO: Find out why it's not `ErrorConstraintUnique`
      (\(err :: SQLError) -> err.sqlError == ErrorConstraint)
      (insertRecord "task_to_tag" connection taskToTag P.>> pure "")
      (handleTagDupError conf taskToTag.tag)

  pure $ vsepCollapse insertWarnings


insertNotes ::
  Config ->
  Connection ->
  Maybe DateTime ->
  Task ->
  [Note] ->
  IO (Doc AnsiStyle)
insertNotes conf connection mbCreatedUtc task notes = do
  let uniqueNotes = nub notes
  taskToNotes <- forM uniqueNotes $ \theNote -> do
    newUlid <- getULID
    pure $
      TaskToNote
        { ulid =
            theNote.ulid
              & parseUlidText
              & fromMaybe newUlid
              & case mbCreatedUtc of
                Nothing -> P.identity
                Just createdUtc -> P.flip setDateTime createdUtc
              & show @ULID
              & T.toLower
        , task_ulid = task.ulid
        , note = theNote.body
        }

  insertWarnings <- P.forM taskToNotes $ \taskToNote ->
    catchAll
      (insertRecord "task_to_note" connection taskToNote P.>> pure "")
      ( \exception ->
          pure $
            annotate (colr conf Yellow) $
              "⚠️ Note "
                <> dquotes (pretty taskToNote.note)
                <> " could not be inserted"
                <+> "ERROR:"
                <+> pretty (show exception :: Text)
      )

  pure $ vsepCollapse insertWarnings


-- | Tuple is (Maybe createdUtc, noteBody)
insertNoteTuples :: Connection -> Task -> [(Maybe DateTime, Text)] -> IO ()
insertNoteTuples connection task notes = do
  taskToNotes <- forM notes $ \(mbCreatedUtc, noteBody) -> do
    noteUlid <- getULID
    pure $
      TaskToNote
        { ulid =
            mbCreatedUtc
              <&> setDateTime noteUlid
              & fromMaybe noteUlid
              & show
              & T.toLower
        , task_ulid = task.ulid
        , note = noteBody
        }

  forM_ taskToNotes $ \taskToNote ->
    insertRecord "task_to_note" connection taskToNote


formatUlid :: IO ULID -> IO Text
formatUlid =
  fmap (T.toLower . show)


{-| Parses the body of the tasks and extracts all meta data
 | Returns a tuple (body, tags, dueUtcMb, createdUtcMb)
 TODO: Replace with parsec implementation
-}
parseTaskBody :: [Text] -> (Text, [Text], Maybe Text, Maybe DateTime)
parseTaskBody bodyWords =
  let
    isTag = ("+" `T.isPrefixOf`)
    isDueUtc = ("due:" `T.isPrefixOf`)
    isCreatedUtc = ("created:" `T.isPrefixOf`)
    isMeta word = isTag word || isDueUtc word || isCreatedUtc word
    -- Handle case when word is actually a text
    bodyWordsReversed = bodyWords & T.unwords & T.words & P.reverse
    body =
      bodyWordsReversed
        & P.dropWhile isMeta
        & P.reverse
        & unwords
    metadata =
      bodyWordsReversed
        & P.takeWhile isMeta
        & P.reverse
    tags = fmap (T.replace "+" "") (P.filter isTag metadata)
    dueUtcMb =
      metadata
        & P.filter isDueUtc
        <&> T.replace "due:" ""
        & P.lastMay
          >>= parseUtc
        <&> (timePrint utcFormatReadable >>> T.pack)
    createdUtcMb =
      metadata
        & P.filter isCreatedUtc
        <&> T.replace "created:" ""
        & P.lastMay
          >>= parseUtc
  in
    (body, tags, dueUtcMb, createdUtcMb)


-- | Get (ulid, modified_utc, effectiveUserName) from the environment
getTriple :: Config -> IO (ULID, Text, [Char])
getTriple conf = do
  ulid <- getULID
  -- TODO: Set via a SQL trigger
  modified_utc <- formatElapsedP conf timeCurrentP
  effectiveUserName <- getEffectiveUserName

  pure (ulid, modified_utc, effectiveUserName)


addEmptyTask :: Config -> Connection -> IO Task
addEmptyTask conf conn = do
  (ulid, modified_utc, effectiveUserName) <- getTriple conf
  let task =
        emptyTask
          { Task.ulid = T.toLower $ show ulid
          , Task.body = ""
          , Task.state = Just Done
          , Task.due_utc = Nothing
          , Task.closed_utc = Just modified_utc
          , Task.user = T.pack effectiveUserName
          , Task.modified_utc = modified_utc
          }

  insertRecord "tasks" conn task
  pure task


-- TODO: Eliminate code duplications with `editTask`
addTask :: Config -> Connection -> [Text] -> IO (Doc AnsiStyle)
addTask conf connection bodyWords = do
  (ulid, modified_utc, effectiveUserName) <- getTriple conf
  let
    (body, tags, dueUtcMb, createdUtcMb) = parseTaskBody bodyWords
    importTaskDraft =
      ImportTask
        { task =
            emptyTask
              { Task.ulid = T.toLower $ show $ case createdUtcMb of
                  Nothing -> ulid
                  Just createdUtc -> setDateTime ulid createdUtc
              , Task.body = body
              , Task.due_utc = dueUtcMb
              , Task.modified_utc = modified_utc
              , Task.user = T.pack effectiveUserName
              }
        , tags = tags
        , notes = [] -- TODO: Add notes to task
        , closedUtcWasExplicit = False
        }

  args <- getArgs
  preAddResults <-
    executeHooks
      ( TL.toStrict $
          TL.decodeUtf8 $
            Aeson.encode $
              object
                [ "arguments" .= args
                , "taskToAdd" .= importTaskToFullTask importTaskDraft
                ]
      )
      conf.hooks.add.pre

  -- Maybe the task was changed by the hook
  (importTask, preAddHookMsg) <- case preAddResults of
    [] -> pure (importTaskDraft, Empty)
    [Left error] -> do
      _ <- exitFailure
      pure (importTaskDraft, pretty error)
    [Right hookResult] -> do
      case hookResult.task of
        Nothing -> pure (importTaskDraft, Empty)
        Just task -> do
          fullImportTask <- setMissingFields task
          pure (fullImportTask, formatHookResult conf hookResult)
    _ -> do
      pure
        ( importTaskDraft
        , annotate (colr conf Red) $
            "ERROR: Multiple pre-add hooks are not supported yet. "
              <> "None of the hooks were executed."
        )

  insertRecord "tasks" connection importTask.task
  warnings <- insertTags conf connection Nothing importTask.task importTask.tags

  -- TODO: Use RETURNING clause in `insertRecord` instead
  (insertedTasks :: [FullTask]) <-
    queryNamed
      connection
      "SELECT * FROM tasks_view WHERE ulid == :ulid"
      [":ulid" := importTask.task.ulid]

  case insertedTasks of
    [insertedTask] -> do
      postAddResults <-
        executeHooks
          ( TL.toStrict $
              TL.decodeUtf8 $
                Aeson.encode $
                  object
                    [ "arguments" .= args
                    , "taskAdded" .= insertedTask
                    -- TODO: Add tags and notes to task
                    ]
          )
          conf.hooks.add.post

      let
        postAddHookMsg :: Doc AnsiStyle
        postAddHookMsg =
          ( postAddResults
              <&> \case
                Left error -> "ERROR:" <+> pretty error
                Right hookResult -> pretty hookResult.message
              & P.fold
          )
            <> hardline

      pure $
        [ preAddHookMsg
        , warnings
        , "🆕 Added task"
            <+> dquotes (pretty importTask.task.body)
            <+> "with id"
            <+> dquotes (pretty importTask.task.ulid)
        , postAddHookMsg
        ]
          & P.filter (\d -> show d /= T.empty)
          & vsep
    ---
    _ -> pure "Task could not be added"


logTask :: Config -> Connection -> [Text] -> IO (Doc AnsiStyle)
logTask conf connection bodyWords = do
  (ulid, modified_utc, effectiveUserName) <- getTriple conf
  let
    (body, extractedTags, dueUtcMb, createdUtcMb) = parseTaskBody bodyWords
    tags = extractedTags <> ["log"]
    task =
      emptyTask
        { Task.ulid = T.toLower $ show $ case createdUtcMb of
            Nothing -> ulid
            Just createdUtc -> setDateTime ulid createdUtc
        , Task.body = body
        , Task.state = Just Done
        , Task.due_utc = dueUtcMb
        , Task.closed_utc = Just modified_utc
        , Task.user = T.pack effectiveUserName
        , Task.modified_utc = modified_utc
        }

  insertRecord "tasks" connection task
  warnings <- insertTags conf connection Nothing task tags
  pure $
    warnings
      <$$> "📝 Logged task"
      <+> dquotes (pretty task.body)
      <+> "with id"
      <+> dquotes (pretty task.ulid)


execWithTask ::
  Config ->
  Connection ->
  Text ->
  (Task -> IO (Doc AnsiStyle)) ->
  IO (Doc AnsiStyle)
execWithTask conf connection idSubstr callback = do
  if T.null idSubstr
    then pure "⚠️  An ID must be specified"
    else do
      tasks <-
        query
          connection
          ( Query $
              "SELECT *\n"
                <> ("FROM \"" <> conf.tableName <> "\"\n")
                <> "WHERE ulid LIKE ?\n"
          )
          ["%" <> idSubstr :: Text] ::
          IO [Task]

      let
        numOfTasks = P.length tasks
        ulidLength = 26
        prefix = if T.length idSubstr == ulidLength then "" else "…"
        quote = dquotes . pretty

      if
        | numOfTasks == 0 ->
            pure $
              "⚠️  Task" <+> quote (prefix <> idSubstr) <+> "does not exist"
        | numOfTasks == 1 ->
            callback $ fromMaybe emptyTask $ P.head tasks
        | numOfTasks > 1 ->
            pure $
              "⚠️  Id slice"
                <+> quote idSubstr
                <+> "is not unique."
                <+> "It could refer to one of the following tasks:"
                  <++> P.foldMap
                    ( \task ->
                        annotate conf.idStyle (pretty task.ulid)
                          <++> pretty task.body
                          <> hardline
                    )
                    tasks
        | otherwise -> pure "This case should not be possible"


-- | Set state and automatically sets `closed_utc` via an SQL trigger
setClosedWithState :: Connection -> Task -> Maybe TaskState -> IO ()
setClosedWithState connection task theTaskState = do
  executeNamed
    connection
    [sql|
      UPDATE tasks
      SET
        state = :state,
        review_utc = NULL
      WHERE
        ulid == :ulid AND
        (state IS NULL OR state != :state)
    |]
    [ ":ulid" := task.ulid
    , ":state" := theTaskState
    ]


setReadyUtc ::
  Config -> Connection -> DateTime -> [IdText] -> IO (Doc AnsiStyle)
setReadyUtc conf connection datetime ids = do
  let utcText = T.pack $ timePrint conf.utcFormat datetime

  docs <- forM ids $ \idSubstr ->
    execWithTask conf connection idSubstr $ \task -> do
      updateTask connection $ task{Task.ready_utc = Just utcText}

      let
        prettyBody = dquotes $ pretty task.body
        prettyId = dquotes $ pretty task.ulid

      pure $
        "📅 Set ready UTC of task"
          <+> prettyBody
          <+> "with id"
          <+> prettyId
          <+> "to"
          <+> dquotes (pretty utcText)

  pure $ vsep docs


waitFor ::
  Config -> Connection -> Iso.Duration -> [Text] -> IO (Doc AnsiStyle)
waitFor conf connection duration ids = do
  docs <- forM ids $ \idSubstr ->
    execWithTask conf connection idSubstr $ \task -> do
      now <- timeCurrentP
      let
        nowAsText = (T.pack . timePrint conf.utcFormat) now
        threeDays =
          (T.pack . timePrint conf.utcFormat)
            ( utcTimeToDateTime $
                Iso.addDuration duration $
                  dateTimeToUtcTime $
                    timeFromElapsedP now
            )

      executeNamed
        connection
        [sql|
          UPDATE tasks
          SET
            waiting_utc = :waiting_utc,
            review_utc = :review_utc
          WHERE
            ulid == :ulid
        |]
        [ ":ulid" := task.ulid
        , ":waiting_utc" := nowAsText
        , ":review_utc" := threeDays
        ]

      let
        prettyBody = dquotes $ pretty task.body
        prettyId = dquotes $ pretty task.ulid

      numOfChanges <- changes connection

      pure $
        if numOfChanges == 0
          then
            "⚠️  An error occurred while moving task"
              <+> prettyBody
                <> "with id"
              <+> prettyId
              <+> "into waiting mode"
          else
            "⏳  Set waiting UTC and review UTC for task"
              <+> prettyBody
              <+> "with id"
              <+> prettyId

  pure $ vsep docs


waitTasks :: Config -> Connection -> [Text] -> IO (Doc AnsiStyle)
waitTasks conf connection =
  waitFor conf connection $
    Iso.DurationDate (Iso.DurDateDay (Iso.DurDay 3) Nothing)


reviewTasksIn ::
  Config ->
  Connection ->
  Iso.Duration ->
  [Text] ->
  IO (Doc AnsiStyle)
reviewTasksIn conf connection duration ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      now <- timeCurrentP
      let
        xDays =
          (T.pack . timePrint conf.utcFormat)
            ( utcTimeToDateTime $
                Iso.addDuration duration $
                  dateTimeToUtcTime $
                    timeFromElapsedP now
            )
        prettyBody = dquotes $ pretty task.body
        prettyId = dquotes $ pretty task.ulid
        warningStart = "⚠️  Task" <+> prettyBody <+> "with id" <+> prettyId

      if isJust task.closed_utc
        then pure $ warningStart <+> "is already closed"
        else do
          executeNamed
            connection
            [sql|
              UPDATE tasks
              SET review_utc = :review_utc
              WHERE ulid == :ulid
            |]
            [ ":ulid" := task.ulid
            , ":review_utc" := xDays
            ]

          numOfChanges <- changes connection

          pure $
            if numOfChanges == 0
              then warningStart <+> "could not be reviewed"
              else getResultMsg task "🔎 Finished review"

  pure $ vsep docs


showDateTime :: Config -> DateTime -> Text
showDateTime conf =
  T.pack . timePrint conf.utcFormat


showEither :: Config -> Either a UTCTime -> Maybe Text
showEither conf theEither =
  theEither
    & either (const Nothing) Just
    <&> (utcTimeToDateTime >>> showDateTime conf)


-- TODO: Eliminate code duplication with createNextRecurrence
createNextRepetition ::
  Config -> Connection -> Task -> IO (Maybe (Doc AnsiStyle))
createNextRepetition conf connection task = do
  newUlidText <- formatUlid getULID
  let
    nowMb =
      ulidTextToDateTime newUlidText

    durTextEither =
      maybeToEither
        "Task has no repetition duration"
        task.repetition_duration

    isoDurEither =
      durTextEither
        <&> encodeUtf8
          >>= Iso.parseDuration

    nextDueMb =
      liftA2
        Iso.addDuration
        isoDurEither
        ( maybeToEither
            "ULID can't be converted to UTC time"
            (nowMb <&> dateTimeToUtcTime)
        )

    newTask =
      task
        { Task.ulid = newUlidText
        , Task.due_utc = nextDueMb & showEither conf
        , Task.closed_utc = Nothing
        , Task.state = Nothing
        , Task.awake_utc =
            liftA2
              Iso.addDuration
              isoDurEither
              ( maybeToEither
                  "Task has no awake UTC"
                  (task.awake_utc >>= parseUtc <&> dateTimeToUtcTime)
              )
              & showEither conf
        , Task.ready_utc =
            liftA2
              Iso.addDuration
              isoDurEither
              ( maybeToEither
                  "Task has no ready UTC"
                  (task.ready_utc >>= parseUtc <&> dateTimeToUtcTime)
              )
              & showEither conf
        , Task.modified_utc =
            nowMb
              <&> (timePrint conf.utcFormat >>> T.pack)
              & fromMaybe ""
        , Task.review_utc = Nothing
        }

  insertRecord "tasks" connection newTask

  tags <-
    query
      connection
      [sql|
        SELECT tag
        FROM task_to_tag
        WHERE task_ulid == ?
      |]
      (Only task.ulid)

  warnings <-
    liftIO $ insertTags conf connection Nothing newTask (tags & P.concat)

  liftIO $
    pure $
      Just $
        warnings
          <$$> "➡️ Created next task"
          <+> dquotes (pretty newTask.body)
          <+> "in repetition series"
          <+> dquotes (pretty newTask.group_ulid)
          <+> "with id"
          <+> dquotes (pretty newUlidText)


-- TODO: Eliminate code duplication with createNextRepetition
createNextRecurrence ::
  Config -> Connection -> Task -> IO (Maybe (Doc AnsiStyle))
createNextRecurrence conf connection task = do
  newUlidText <- formatUlid getULID
  let
    dueUtcMb =
      task.due_utc >>= parseUtc

    durTextEither =
      maybeToEither
        "Task has no recurrence duration"
        task.recurrence_duration

    isoDurEither =
      durTextEither
        <&> encodeUtf8
          >>= Iso.parseDuration

    -- If task has no due UTC, use current UTC as the base for recurrence
    nowMb = ulidTextToDateTime newUlidText
    baseDateMb = dueUtcMb <|> nowMb

    nextDueMb =
      liftA2
        Iso.addDuration
        isoDurEither
        ( maybeToEither
            "Task has no due UTC current time couldn't be determined"
            (baseDateMb <&> dateTimeToUtcTime)
        )

    newTask =
      task
        { Task.ulid = newUlidText
        , Task.due_utc = nextDueMb & showEither conf
        , Task.closed_utc = Nothing
        , Task.state = Nothing
        , Task.awake_utc =
            liftA2
              Iso.addDuration
              isoDurEither
              ( maybeToEither
                  "Task has no awake UTC"
                  (task.awake_utc >>= parseUtc <&> dateTimeToUtcTime)
              )
              & showEither conf
        , Task.ready_utc =
            liftA2
              Iso.addDuration
              isoDurEither
              ( maybeToEither
                  "Task has no ready UTC"
                  (task.ready_utc >>= parseUtc <&> dateTimeToUtcTime)
              )
              & showEither conf
        , Task.modified_utc =
            newUlidText
              & ulidTextToDateTime
              <&> (timePrint conf.utcFormat >>> T.pack)
              & fromMaybe ""
        , Task.review_utc = Nothing
        }

  insertRecord "tasks" connection newTask

  tags <-
    query
      connection
      [sql|
        SELECT tag
        FROM task_to_tag
        WHERE task_ulid == ?
      |]
      (Only task.ulid)

  warnings <-
    liftIO $ insertTags conf connection Nothing newTask (tags & P.concat)

  liftIO $
    pure $
      Just $
        warnings
          <$$> "➡️ Created next task"
          <+> dquotes (pretty task.body)
          <+> "in recurrence series"
          <+> dquotes (pretty task.group_ulid)
          <+> "with id"
          <+> dquotes (pretty newUlidText)


doTasks :: Config -> Connection -> Maybe [Text] -> [Text] -> IO (Doc AnsiStyle)
doTasks conf connection noteWordsMaybe ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      let
        prettyBody = dquotes $ pretty task.body
        prettyId = dquotes $ pretty task.ulid

      if task.state == Just Done
        then
          pure $
            "⚠️  Task"
              <+> prettyBody
              <+> "with id"
              <+> prettyId
              <+> "is already done"
        else do
          logMessageMaybe <-
            if isJust task.repetition_duration
              then createNextRepetition conf connection task
              else
                if isJust task.recurrence_duration
                  then createNextRecurrence conf connection task
                  else pure Nothing

          noteMessageMaybe <- case noteWordsMaybe of
            Nothing -> pure Nothing
            Just noteWords ->
              liftIO $
                addNote conf connection (unwords noteWords) ids <&> Just

          setClosedWithState connection task $ Just Done

          pure $
            fromMaybe "" (noteMessageMaybe <&> (<> hardline))
              <> ( "✅ Finished task"
                    <+> dquotes (pretty task.body)
                    <+> "with id"
                    <+> dquotes (pretty task.ulid)
                 )
              <> fromMaybe "" (logMessageMaybe <&> (hardline <>))

  pure $ vsep docs


endTasks :: Config -> Connection -> Maybe [Text] -> [Text] -> IO (Doc AnsiStyle)
endTasks conf connection noteWordsMaybe ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      let
        prettyBody = dquotes $ pretty task.body
        prettyId = dquotes $ pretty task.ulid

      if task.state == Just Obsolete
        then
          pure $
            "⚠️  Task"
              <+> prettyBody
              <+> "with id"
              <+> prettyId
              <+> "is already marked as obsolete"
        else do
          logMessageMaybe <-
            if isJust task.repetition_duration
              then createNextRepetition conf connection task
              else
                if isJust task.recurrence_duration
                  then createNextRecurrence conf connection task
                  else pure Nothing

          noteMessageMaybe <- case noteWordsMaybe of
            Nothing -> pure Nothing
            Just noteWords ->
              liftIO $
                addNote conf connection (unwords noteWords) ids <&> Just

          setClosedWithState connection task $ Just Obsolete

          pure $
            fromMaybe "" (noteMessageMaybe <&> (<> hardline))
              <> ( "⏹  Marked task"
                    <+> prettyBody
                    <+> "with id"
                    <+> prettyId
                    <+> "as obsolete"
                 )
              <> fromMaybe "" (logMessageMaybe <&> (hardline <>))

  pure $ vsep docs


trashTasks :: Config -> Connection -> [Text] -> IO (Doc AnsiStyle)
trashTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      setClosedWithState connection task $ Just Deletable
      numOfChanges <- changes connection

      let
        prettyBody = dquotes $ pretty task.body
        prettyId = dquotes $ pretty task.ulid

      pure $
        if numOfChanges == 0
          then
            "⚠️  Task"
              <+> prettyBody
              <+> "with id"
              <+> prettyId
              <+> "is already marked as deletable"
          else
            "🗑  Marked task"
              <+> prettyBody
              <+> "with id"
              <+> prettyId
              <+> "as deletable"

  pure $ vsep docs


deleteTasks :: Config -> Connection -> [Text] -> IO (Doc AnsiStyle)
deleteTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      execute
        connection
        [sql|
          DELETE FROM tasks
          WHERE ulid == ?
        |]
        (Only task.ulid)

      execute
        connection
        [sql|
          DELETE FROM task_to_tag
          WHERE task_ulid == ?
        |]
        (Only task.ulid)

      execute
        connection
        [sql|
          DELETE FROM task_to_note
          WHERE task_ulid == ?
        |]
        (Only task.ulid)

      let
        prettyBody = dquotes $ pretty task.body
        prettyId = dquotes $ pretty task.ulid

      pure $ "❌ Deleted task" <+> prettyBody <+> "with id" <+> prettyId

  pure $ vsep docs


durationToIso :: Duration -> Text
durationToIso dur =
  "PT" <> show (coerce (durationMinutes dur) :: Int64) <> "M"


repeatTasks ::
  Config -> Connection -> Iso.Duration -> [IdText] -> IO (Doc AnsiStyle)
repeatTasks conf connection duration ids = do
  let durationIsoText = decodeUtf8 $ Iso.formatDuration duration

  docs <- forM ids $ \idSubstr ->
    execWithTask conf connection idSubstr $ \task -> do
      groupUlid <- formatUlid getULID

      recDur :: [Only SQLData] <-
        queryNamed
          connection
          [sql|
            UPDATE tasks
            SET
              repetition_duration = :repetition_duration,
              group_ulid = :group_ulid
            WHERE
              ulid == :ulid AND
              recurrence_duration IS NULL
            RETURNING recurrence_duration
          |]
          [ ":repetition_duration" := durationIsoText
          , ":group_ulid" := groupUlid
          , ":ulid" := task.ulid
          ]

      if recDur /= [Only SQLNull]
        then
          pure $
            "⚠️ Task"
              <+> dquotes (pretty task.body)
              <+> "with id"
              <+> dquotes (pretty task.ulid)
              <+> "is already in a recurrence series"
        else do
          -- If repetition is set for already closed task,
          -- next task in series must be created immediately
          creationMb <-
            if isNothing task.closed_utc
              then pure $ Just mempty
              else
                liftIO $
                  createNextRepetition conf connection $
                    task
                      { Task.repetition_duration = Just durationIsoText
                      , Task.group_ulid = Just groupUlid
                      }

          pure $
            "📅 Set repeat duration of task"
              <+> dquotes (pretty task.body)
              <+> "with id"
              <+> dquotes (pretty task.ulid)
              <+> "to"
              <+> dquotes (pretty durationIsoText)
                <++> ( creationMb
                        & fromMaybe
                          "⚠️ Next task in repetition series could not be created!"
                     )

  pure $ vsep docs


recurTasks ::
  Config -> Connection -> Iso.Duration -> [IdText] -> IO (Doc AnsiStyle)
recurTasks conf connection duration ids = do
  let durationIsoText = decodeUtf8 $ Iso.formatDuration duration

  docs <- forM ids $ \idSubstr ->
    execWithTask conf connection idSubstr $ \task -> do
      groupUlid <- formatUlid getULID

      repDur :: [Only SQLData] <-
        queryNamed
          connection
          [sql|
            UPDATE tasks
            SET
              recurrence_duration = :recurrence_duration,
              group_ulid = :group_ulid
            WHERE
              ulid == :ulid AND
              repetition_duration IS NULL
            RETURNING repetition_duration
          |]
          [ ":recurrence_duration" := durationIsoText
          , ":group_ulid" := groupUlid
          , ":ulid" := task.ulid
          ]

      if repDur /= [Only SQLNull]
        then
          pure $
            "⚠️ Task"
              <+> dquotes (pretty task.body)
              <+> "with id"
              <+> dquotes (pretty task.ulid)
              <+> "is already in a repetition series"
        else do
          -- If recurrence is set for already closed task,
          -- next task in series must be created immediately
          creationMb <-
            if isNothing task.closed_utc
              then pure $ Just mempty
              else
                liftIO $
                  createNextRecurrence conf connection $
                    task
                      { Task.recurrence_duration = Just durationIsoText
                      , Task.group_ulid = Just groupUlid
                      }

          pure $
            "📅 Set recurrence duration of task"
              <+> dquotes (pretty task.body)
              <+> "with id"
              <+> dquotes (pretty task.ulid)
              <+> "to"
              <+> dquotes (pretty durationIsoText)
                <++> ( creationMb
                        & fromMaybe
                          "⚠️ Next task in recurrence series could not be created!"
                     )

  pure $ vsep docs


adjustPriority :: Config -> Float -> [IdText] -> IO (Doc AnsiStyle)
adjustPriority conf adjustment ids = do
  dbPath <- getDbPath conf
  withConnection dbPath $ \connection -> do
    docs <- forM ids $ \idSubstr -> do
      execWithTask conf connection idSubstr $ \task -> do
        executeNamed
          connection
          [sql|
            UPDATE tasks
            SET priority_adjustment =
              ifnull(priority_adjustment, 0) + :adjustment
            WHERE ulid == :ulid
          |]
          [ ":adjustment" := adjustment
          , ":ulid" := task.ulid
          ]

        numOfChanges <- changes connection

        let prettyBody = dquotes $ pretty task.body

        pure $
          if numOfChanges == 0
            then
              "⚠️ An error occurred while adjusting the priority of task"
                <+> prettyBody
            else
              (if adjustment > 0 then "⬆️  Increased" else "⬇️  Decreased")
                <+> "priority of task"
                <+> prettyBody
                <+> "with id"
                <+> dquotes (pretty task.ulid)
                <+> "by"
                <+> pretty (abs adjustment)

    pure $ vsep docs


startTasks :: Config -> Connection -> [Text] -> IO (Doc AnsiStyle)
startTasks conf connection ids = do
  logMessage <- addNote conf connection "start" ids

  pure $
    pretty $
      T.replace
        "📝  Added a note to"
        "⏳ Started"
        (show logMessage)


stopTasks :: Config -> Connection -> [Text] -> IO (Doc AnsiStyle)
stopTasks conf connection ids = do
  logMessages <- addNote conf connection "stop" ids

  pure $
    pretty $
      T.replace
        "📝  Added a note to"
        "⌛️ Stopped"
        (show logMessages)


formatTaskForInfo ::
  Config ->
  DateTime ->
  (FullTask, [TaskToTag], [TaskToNote]) ->
  Doc AnsiStyle
formatTaskForInfo conf now (taskV, tags, notes) =
  let
    mkGreen = annotate (colr conf Green)
    grayOut = annotate (colrDull conf Black)
    stateHierarchy = getStateHierarchy now $ cpTimesAndState taskV
    mbCreatedUtc =
      fmap
        (T.pack . timePrint (utcFormat defaultConfig))
        (ulidTextToDateTime taskV.ulid)
    tagsPretty =
      tags
        <&> ( \t ->
                annotate conf.tagStyle (pretty t.tag)
                  <++> maybe
                    mempty
                    (grayOut . pretty . T.pack . timePrint conf.utcFormat)
                    (ulidTextToDateTime t.ulid)
                  <++> grayOut (pretty t.ulid)
            )
    notesPretty =
      notes
        <&> ( \n ->
                maybe
                  mempty
                  (grayOut . pretty . T.pack . timePrint conf.utcFormat)
                  (ulidTextToDateTime n.ulid)
                  <++> grayOut (pretty n.ulid)
                  <> hardline
                  <> indent 2 (pretty n.note)
                  <> hardline
            )

    mbAwakeUtc = FullTask.awake_utc taskV
    mbReadyUtc = FullTask.ready_utc taskV
    mbWaitingUtc = FullTask.waiting_utc taskV
    mbReviewUtc = FullTask.review_utc taskV
    mbDueUtc = FullTask.due_utc taskV
    mbClosedUtc = FullTask.closed_utc taskV
    mbModifiedUtc = Just $ FullTask.modified_utc taskV

    printIf :: Doc AnsiStyle -> Maybe Text -> Maybe (Doc AnsiStyle)
    printIf name =
      fmap
        ( \v ->
            name
              <+> annotate conf.dueStyle (pretty v)
                <> hardline
        )
  in
    hardline
      <> annotate (maybeBold conf) (pretty taskV.body)
      <> hardline
      <> hardline
      <> ( if P.null tags
            then mempty
            else
              (tags <&> (TaskToTag.tag >>> formatTag conf) & hsep)
                <> hardline
                <> hardline
         )
      <> ( if P.null notes
            then mempty
            else
              ( notes
                  <&> ( \n ->
                          maybe
                            mempty
                            ( grayOut
                                . pretty
                                . T.pack
                                . timePrint
                                  conf.utcFormatShort
                            )
                            (ulidTextToDateTime n.ulid)
                            <++> align (pretty n.note)
                      )
                  & vsep
              )
                <> hardline
                <> hardline
         )
      <> ( "   State:"
            <+> mkGreen (pretty stateHierarchy)
              <> hardline
         )
      <> ( "Priority:"
            <+> annotate
              conf.priorityStyle
              (pretty $ FullTask.priority taskV)
              <> hardline
         )
      <> ( "    ULID:"
            <+> grayOut (pretty $ FullTask.ulid taskV)
              <> hardline
         )
      <> hardline
      <> ( [ (printIf "🆕  Created  ", mbCreatedUtc)
           , (printIf "☀️   Awake   ", mbAwakeUtc)
           , (printIf "📅   Ready   ", mbReadyUtc)
           , (printIf "⏳  Waiting  ", mbWaitingUtc)
           , (printIf "🔎  Review   ", mbReviewUtc)
           , (printIf "📅    Due    ", mbDueUtc)
           , (printIf "✅   Done    ", mbClosedUtc)
           , (printIf "✏️   Modified ", mbModifiedUtc)
           ]
            & sortBy (compare `on` snd)
            & P.mapMaybe (\tup -> fst tup (snd tup))
            & punctuate (pretty ("       ⬇" :: Text))
            & vsep
         )
      <> hardline
      <> maybe
        mempty
        ( \value ->
            "Repetition Duration:"
              <+> mkGreen (pretty value)
                <> hardline
        )
        (FullTask.repetition_duration taskV)
      <> maybe
        mempty
        ( \value ->
            "Recurrence Duration:"
              <+> mkGreen (pretty value)
                <> hardline
        )
        (FullTask.recurrence_duration taskV)
      <> maybe
        mempty
        ( \value ->
            "Group Ulid:"
              <+> grayOut (pretty value)
                <> hardline
        )
        (FullTask.group_ulid taskV)
      <> ( "User:"
            <+> mkGreen (pretty $ FullTask.user taskV)
              <> hardline
         )
      <> hardline
      <> maybe
        mempty
        ( \value ->
            "Metadata:"
              <> hardline
              <> indent 2 (pretty $ decodeUtf8 $ Yaml.encode value)
              <> hardline
        )
        (FullTask.metadata taskV)
      <> ( if P.null tags
            then mempty
            else
              annotate (maybeUnderlined conf) "Tags Detailed:"
                <> hardline
                <> hardline
                <> vsep tagsPretty
                <> hardline
                <> hardline
         )
      <> ( if P.null notes
            then mempty
            else
              annotate (maybeUnderlined conf) "Notes Detailed:"
                <> hardline
                <> hardline
                <> vsep notesPretty
                <> hardline
         )


infoTask :: Config -> Connection -> Text -> IO (Doc AnsiStyle)
infoTask conf connection idSubstr = do
  execWithTask conf connection idSubstr $ \task -> do
    now <- dateCurrent

    fullTasks :: [FullTask] <-
      query
        connection
        [sql|
          SELECT *
          FROM tasks_view
          WHERE ulid == ?
        |]
        (Only task.ulid)

    tags <-
      query
        connection
        [sql|
          SELECT *
          FROM task_to_tag
          WHERE task_ulid == ?
        |]
        (Only task.ulid)

    notes <-
      query
        connection
        [sql|
          SELECT *
          FROM task_to_note
          WHERE task_ulid == ?
        |]
        (Only task.ulid)

    pure $ case fullTasks of
      [fullTask] -> formatTaskForInfo conf now (fullTask, tags, notes)
      _ -> pretty noTasksWarning


nextTask :: Config -> Connection -> IO (Doc AnsiStyle)
nextTask conf connection = do
  now <- dateCurrent

  tasks :: [FullTask] <-
    query_
      connection
      [sql|
        SELECT *
        FROM tasks_view
        WHERE closed_utc IS NULL
        ORDER BY priority DESC
        LIMIT 1
      |]

  case tasks of
    [fullTask] -> do
      tags <-
        query
          connection
          [sql|
            SELECT *
            FROM task_to_tag
            WHERE task_ulid == ?
          |]
          (Only fullTask.ulid)

      notes <-
        query
          connection
          [sql|
            SELECT *
            FROM task_to_note
            WHERE task_ulid == ?
          |]
          (Only fullTask.ulid)

      pure $ formatTaskForInfo conf now (fullTask, tags, notes)
    _ ->
      pure $ pretty noTasksWarning


randomTask ::
  Config -> Connection -> Maybe [Text] -> IO (Doc AnsiStyle)
randomTask conf connection filterExpression = do
  let
    parserResults =
      readP_to_S filterExpsParser $
        T.unpack (unwords $ fromMaybe [""] filterExpression)
    filterMay = listToMaybe parserResults

  case filterMay of
    Nothing -> do
      (tasks :: [FullTask]) <-
        query_
          connection
          [sql|
            SELECT *
            FROM tasks_view
            WHERE closed_utc IS NULL
            ORDER BY random()
            LIMIT 1
          |]

      case tasks of
        [fullTask] -> infoTask conf connection fullTask.ulid
        _ -> pure $ pretty noTasksWarning
    --
    Just (filterExps, _) -> do
      let
        ppInvalidFilter = \case
          (InvalidFilter error) ->
            dquotes (pretty error) <+> "is an invalid filter"
          (HasStatus Nothing) -> "Filter contains an invalid state value"
          _ -> "The functions should not be called with a valid function"
        errors = P.filter (not . isValidFilter) filterExps
        errorsDoc =
          if P.length errors > 0
            then
              Just $
                vsep (fmap (annotate (colr conf Red) . ppInvalidFilter) errors)
                  <> hardline
                  <> hardline
            else Nothing

      tasks <-
        query_ connection $
          getFilterQuery
            (HasStatus (Just IsOpen) : filterExps)
            (Just "random()")
            Nothing

      case P.headMay (tasks :: [FullTask]) of
        Nothing -> pure $ pretty noTasksWarning
        Just fullTask -> do
          taskFormatted <- infoTask conf connection fullTask.ulid
          pure $ errorsDoc & fromMaybe taskFormatted


-- | Fuzzy search for tasks matching a pattern in body, ulid, notes, or metadata
findTask :: Config -> Connection -> Text -> IO (Doc AnsiStyle)
findTask conf connection aPattern = do
  let sqlFuzzyPattern = aPattern & T.chunksOf 1 & T.intercalate (T.pack "%")
  tasksFuzzy :: [(Text, Text, Maybe Text, Maybe Text)] <-
    query
      connection
      [sql|
        SELECT ulid, body, notes, metadata
        FROM tasks_view
        WHERE body LIKE '%' || ? || '%'
           OR ulid LIKE '%' || ? || '%'
           OR notes LIKE '%' || ? || '%'
           OR metadata LIKE '%' || ? || '%'
      |]
      ( sqlFuzzyPattern
      , aPattern -- Only match on ULID substring
      , sqlFuzzyPattern
      , sqlFuzzyPattern
      )

  let
    ulidWidth = 5
    numOfResults = 8
    minimumScore = 5
    ulidColor = Green
    -- TODO: Escape sequences are counted as several chars and mess up wrapping.
    --       Implement first: https://github.com/ad-si/Fuzzily/issues/1
    preTag = "\x1b[4m\x1b[34m" -- Set color to blue and underline text
    postTag = "\x1b[0m" -- Reset styling
    metaNorm metadata =
      metadata
        & fromMaybe ""
        & T.replace ",\"" ", \""
        & T.replace "\":" "\": "
        & T.replace "\"" ""
    matchFunc =
      Fuzzily.match
        Fuzzily.IgnoreCase
        (preTag, postTag)
        identity
        aPattern

    -- Calculate fuzzy score for each part individually
    -- and pick the highest one
    scoreFunc (ulid, theBody, mbNotes, mbMetadata) =
      let
        scoreParts =
          [ ( matchFunc theBody
                -- Weight the body score higher
                <&> \case
                  Fuzzily.Fuzzy{score, ..} ->
                    Fuzzily.Fuzzy{score = score + 1, ..}
            )
              -- Always include the body
              <|> ( Just $
                      Fuzzily.Fuzzy
                        { original = theBody
                        , rendered = theBody
                        , score = 0
                        }
                  )
          , matchFunc (fromMaybe "" mbNotes)
          , -- TODO: Find good way to include tags
            -- , matchFunc (maybe "" unwords mbTags)
            matchFunc (metaNorm mbMetadata)
          , matchFunc ulid
          ]
        highestScore = P.maximum $ 0 : (catMaybes scoreParts <&> Fuzzily.score)
        combinedText =
          scoreParts
            & catMaybes
            <&> (Fuzzily.rendered >>> reflow)
            & P.intersperse mempty
            & vcat
      in
        (highestScore, ulid, combinedText)

    fstOf3 (x, _, _) = x
    tasksScored =
      tasksFuzzy
        <&> scoreFunc
        & P.filter ((> minimumScore) . fstOf3)
        & sortOn (Down . fstOf3)
    moreResults = P.length tasksScored - numOfResults
    header =
      annotate (maybeUnderlined conf <> colr conf ulidColor) (fill ulidWidth "ULID")
        <++> annotate (maybeUnderlined conf) (fill 20 "Task")
        <> hardline
    body =
      tasksScored
        & P.take numOfResults
        <&> ( \(_, ulid, combinedText) ->
                annotate
                  (colr conf ulidColor)
                  (fill ulidWidth $ pretty $ T.takeEnd ulidWidth ulid)
                  <> indent 2 combinedText
            )
        & P.intersperse mempty
        & vsep
    footer =
      if moreResults > 0
        then
          hardline
            <> hardline
            <> annotate
              (colr conf Red)
              ("There are " <> pretty moreResults <> " more results available")
            <> hardline
        else hardline

  pure $ header <> body <> footer


-- TODO: Use Continuation monad to avoid callback hell
-- withConnectCont :: Text -> ContT a IO Connection
-- withConnectCont dbPath =
--     ContT $ withConnection dbPath

addTag :: Config -> Connection -> Text -> [IdText] -> IO (Doc AnsiStyle)
addTag conf conn tag ids = do
  let tagNorm = T.dropWhile (== '+') tag
  docs <- forM ids $ \idSubstr ->
    execWithTask conf conn idSubstr $ \task -> do
      now <- fmap (T.pack . timePrint conf.utcFormat) timeCurrentP
      ulid <- formatUlid getULID

      catchIf
        -- TODO: Find out why it's not `ErrorConstraintUnique`
        (\(err :: SQLError) -> err.sqlError == ErrorConstraint)
        ( do
            insertRecord
              "task_to_tag"
              conn
              TaskToTag{ulid, task_ulid = task.ulid, tag = tagNorm}

            -- TODO: Check if modified_utc could be set via SQL trigger
            executeNamed
              conn
              [sql|
                UPDATE tasks
                SET modified_utc = :now
                WHERE ulid == :ulid
              |]
              [ ":now" := now
              , ":ulid" := task.ulid
              ]

            let
              prettyBody = dquotes $ pretty task.body
              prettyId = dquotes $ pretty task.ulid

            pure $
              "🏷  Added tag"
                <+> dquotes (pretty tagNorm)
                <+> "to task"
                <+> prettyBody
                <+> "with id"
                <+> prettyId
        )
        (handleTagDupError conf tagNorm)

  pure $ vsep docs


deleteTag :: Config -> Connection -> Text -> [IdText] -> IO (Doc AnsiStyle)
deleteTag conf connection tag ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      executeNamed
        connection
        [sql|
          DELETE FROM task_to_tag
          WHERE
            task_ulid == :task_ulid
            AND tag == :tag
        |]
        [ ":task_ulid" := task.ulid
        , ":tag" := tag
        ]

      numOfChanges <- changes connection

      pure $
        if numOfChanges == 0
          then
            annotate (colr conf Yellow) $
              "⚠️  Tag"
                <+> dquotes (pretty tag)
                <+> "is not set for task"
                <+> dquotes (pretty task.ulid)
          else getResultMsg task ("💥 Removed tag \"" <> pretty tag <> "\"")

  pure $ vsep docs


addNote :: Config -> Connection -> Text -> [IdText] -> IO (Doc AnsiStyle)
addNote conf connection noteBody ids = do
  docs <- forM ids $ \idSubstr ->
    execWithTask conf connection idSubstr $ \task -> do
      now <- timeCurrentP <&> (timePrint conf.utcFormat >>> T.pack)
      ulid <- formatUlid getULID

      insertRecord
        "task_to_note"
        connection
        TaskToNote
          { ulid
          , task_ulid = task.ulid
          , TaskToNote.note = noteBody
          }

      -- TODO: Check if modified_utc could be set via SQL trigger
      executeNamed
        connection
        [sql|
          UPDATE tasks
          SET modified_utc = :now
          WHERE ulid == :ulid
        |]
        [ ":now" := now
        , ":ulid" := task.ulid
        ]

      let
        prettyBody = dquotes $ pretty task.body
        prettyId = dquotes $ pretty task.ulid

      pure $
        "🗒  Added a note to task"
          <+> prettyBody
          <+> "with id"
          <+> prettyId

  pure $ vsep docs


deleteNote :: Config -> Connection -> IdText -> IO (Doc AnsiStyle)
deleteNote conf connection noteId = do
  taskIds :: [Only Text] <-
    queryNamed
      connection
      [sql|
        DELETE FROM task_to_note
        WHERE ulid == :noteId
        RETURNING task_ulid
      |]
      [":noteId" := noteId]

  case taskIds of
    [Only taskId] ->
      pure $
        "💥 Deleted note"
          <+> dquotes (pretty noteId)
          <+> "of task"
          <+> dquotes (pretty taskId)
    [] ->
      pure $
        annotate (colr conf Yellow) $
          "⚠️  Note" <+> dquotes (pretty noteId) <+> "does not exist"
    _ ->
      pure $
        annotate (colr conf Yellow) $
          ("⚠️  Note" <+> dquotes (pretty noteId) <+> "exists multiple times.")
            <++> "This indicates a serious database inconsistency \
                 \and you should clean up the database manually \
                 \before continuing."


setDueUtc :: Config -> Connection -> DateTime -> [IdText] -> IO (Doc AnsiStyle)
setDueUtc conf connection datetime ids = do
  let
    utcText :: Text
    utcText = T.pack $ timePrint conf.utcFormat datetime

  docs <- forM ids $ \idSubstr ->
    execWithTask conf connection idSubstr $ \task -> do
      updateTask connection task{Task.due_utc = Just utcText}

      let
        prettyBody = dquotes $ pretty task.body
        prettyId = dquotes $ pretty task.ulid

      pure $
        "📅 Set due UTC of task"
          <+> prettyBody
          <+> "with id"
          <+> prettyId
          <+> "to"
          <+> dquotes (pretty utcText)

  pure $ vsep docs


getResultMsg :: Task -> Doc AnsiStyle -> Doc AnsiStyle
getResultMsg task msg = do
  let
    prettyBody = dquotes $ pretty task.body
    prettyId = dquotes $ pretty task.ulid

  msg <+> "of task" <+> prettyBody <+> "with id" <+> prettyId


getWarnMsg :: Config -> Task -> Doc AnsiStyle -> Doc AnsiStyle
getWarnMsg conf task msg = do
  let
    prettyBody = dquotes $ pretty task.body
    prettyId = dquotes $ pretty task.ulid

  annotate (colr conf Yellow) $
    "⚠️ Task" <+> prettyBody <+> "with id" <+> prettyId <+> msg


uncloseTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
uncloseTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      executeNamed
        connection
        [sql|
          UPDATE tasks
          SET
            closed_utc = NULL,
            state = NULL
          WHERE
            ulid == :ulid AND
            closed_utc IS NOT NULL AND
            state IS NOT NULL
        |]
        [":ulid" := task.ulid]

      numOfChanges <- changes connection
      pure $
        if numOfChanges == 0
          then getWarnMsg conf task "is still open"
          else getResultMsg task "💥 Removed close timestamp and state field"

  pure $ vsep docs


undueTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
undueTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      executeNamed
        connection
        [sql|
          UPDATE tasks
          SET due_utc = NULL
          WHERE
            ulid == :ulid AND
            due_utc IS NOT NULL
        |]
        [":ulid" := task.ulid]

      numOfChanges <- changes connection
      pure $
        if numOfChanges == 0
          then getWarnMsg conf task "does not have a due timestamp"
          else getResultMsg task "💥 Removed due timestamp"

  pure $ vsep docs


unwaitTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
unwaitTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      executeNamed
        connection
        [sql|
          UPDATE tasks
          SET waiting_utc = NULL
          WHERE
            ulid == :ulid AND
            waiting_utc IS NOT NULL
        |]
        [":ulid" := task.ulid]

      numOfChanges <- changes connection
      pure $
        if numOfChanges == 0
          then getWarnMsg conf task "does not have a waiting timestamp"
          else getResultMsg task "💥 Removed waiting and review timestamps"

  pure $ vsep docs


unwakeTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
unwakeTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      executeNamed
        connection
        [sql|
          UPDATE tasks
          SET awake_utc = NULL
          WHERE
            ulid == :ulid AND
            awake_utc IS NOT NULL
        |]
        [":ulid" := task.ulid]

      numOfChanges <- changes connection
      pure $
        if numOfChanges == 0
          then getWarnMsg conf task "does not have an awake timestamp"
          else getResultMsg task "💥 Removed awake timestamp"

  pure $ vsep docs


unreadyTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
unreadyTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      executeNamed
        connection
        [sql|
          UPDATE tasks
          SET ready_utc = NULL
          WHERE
            ulid == :ulid AND
            ready_utc IS NOT NULL
        |]
        [":ulid" := task.ulid]

      numOfChanges <- changes connection
      pure $
        if numOfChanges == 0
          then getWarnMsg conf task "does not have a ready timestamp"
          else getResultMsg task "💥 Removed ready timestamp"

  pure $ vsep docs


unreviewTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
unreviewTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      executeNamed
        connection
        [sql|
          UPDATE tasks
          SET review_utc = NULL
          WHERE
            ulid == :ulid AND
            review_utc IS NOT NULL
        |]
        [":ulid" := task.ulid]

      numOfChanges <- changes connection
      pure $
        if numOfChanges == 0
          then getWarnMsg conf task "does not have a review timestamp"
          else getResultMsg task "💥 Removed review timestamp"

  pure $ vsep docs


unrepeatTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
unrepeatTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      executeNamed
        connection
        [sql|
          UPDATE tasks
          SET repetition_duration = NULL
          WHERE
            ulid == :ulid AND
            repetition_duration IS NOT NULL
        |]
        [":ulid" := task.ulid]

      numOfChanges <- changes connection
      pure $
        if numOfChanges == 0
          then getWarnMsg conf task "does not have a repetition duration"
          else getResultMsg task "💥 Removed repetition duration"

  pure $ vsep docs


unrecurTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
unrecurTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      executeNamed
        connection
        [sql|
          UPDATE tasks
          SET recurrence_duration = NULL
          WHERE
            ulid == :ulid AND
            recurrence_duration IS NOT NULL
        |]
        [":ulid" := task.ulid]

      numOfChanges <- changes connection
      pure $
        if numOfChanges == 0
          then getWarnMsg conf task "does not have a recurrence duration"
          else getResultMsg task "💥 Removed recurrence duration"

  pure $ vsep docs


untagTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
untagTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      executeNamed
        connection
        [sql|
          DELETE FROM task_to_tag
          WHERE task_ulid == :task_ulid
        |]
        [":task_ulid" := task.ulid]

      numOfChanges <- changes connection
      pure $
        if numOfChanges == 0
          then getWarnMsg conf task "does not have any tags"
          else getResultMsg task "💥 Removed all tags"

  pure $ vsep docs


unnoteTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
unnoteTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      executeNamed
        connection
        [sql|
          DELETE FROM task_to_note
          WHERE task_ulid == :task_ulid
        |]
        [":task_ulid" := task.ulid]

      numOfChanges <- changes connection
      pure $
        if numOfChanges == 0
          then getWarnMsg conf task "does not have any notes"
          else getResultMsg task "💥 Deleted all notes"

  pure $ vsep docs


unprioTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
unprioTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      executeNamed
        connection
        [sql|
          UPDATE tasks
          SET priority_adjustment = NULL
          WHERE
            ulid == :ulid AND
            priority_adjustment IS NOT NULL
        |]
        [":ulid" := task.ulid]

      numOfChanges <- changes connection
      pure $
        if numOfChanges == 0
          then getWarnMsg conf task "does not have a priority adjustment"
          else getResultMsg task "💥 Removed priority adjustment"

  pure $ vsep docs


unmetaTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
unmetaTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      executeNamed
        connection
        [sql|
          UPDATE tasks
          SET metadata = NULL
          WHERE
            ulid == :ulid AND
            metadata IS NOT NULL
        |]
        [":ulid" := task.ulid]

      numOfChanges <- changes connection
      pure $
        if numOfChanges == 0
          then getWarnMsg conf task "does not have any metadata"
          else getResultMsg task "💥 Removed metadata"

  pure $ vsep docs


duplicateTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
duplicateTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      dupeUlid <- formatUlid getULID
      -- TODO: Check if modified_utc can be set via an SQL trigger
      modified_utc <- formatElapsedP conf timeCurrentP
      user <- getEffectiveUserName

      let dupeTask =
            emptyTask
              { Task.ulid = dupeUlid
              , Task.body = task.body
              , Task.modified_utc = modified_utc
              , Task.priority_adjustment = task.priority_adjustment
              , Task.user = T.pack user
              , Task.metadata = task.metadata
              }

      insertRecord "tasks" connection dupeTask

      tags <-
        query
          connection
          [sql|
            SELECT tag
            FROM task_to_tag
            WHERE task_ulid == ?
          |]
          (Only task.ulid)

      warnings <-
        liftIO $ insertTags conf connection Nothing dupeTask (tags & P.concat)

      notes <-
        query
          connection
          [sql|
            SELECT *
            FROM task_to_note
            WHERE task_ulid == ?
          |]
          (Only task.ulid)

      let
        noteTuples =
          notes <&> \theNote ->
            ( ulidTextToDateTime (TaskToNote.ulid theNote)
            , TaskToNote.note theNote
            )

      liftIO $
        insertNoteTuples
          connection
          dupeTask
          noteTuples

      numOfChanges <- changes connection

      let
        prettyBody = dquotes $ pretty task.body
        prettyId = dquotes $ pretty task.ulid

      pure $
        warnings
          <$$> if numOfChanges == 0
            then
              "⚠️  Task"
                <+> prettyBody
                <+> "with id"
                <+> prettyId
                <+> "could not be duplicated"
            else
              "👯  Created a duplicate of task"
                <+> prettyBody
                <+> "(id:"
                <+> pretty task.ulid
                <+> ")"
                <+> "with id"
                <+> pretty dupeUlid

  pure $ vsep docs


showAtPrecision :: Int -> Double -> Text
showAtPrecision numOfDigits number =
  let
    tuple = T.breakOn "." (show number)
    clipDecimalPart =
      if snd tuple == ".0"
        then T.replace ".0" (T.replicate (1 + numOfDigits) " ")
        else T.take (1 + numOfDigits)
  in
    fst tuple
      <> if numOfDigits /= 0
        then (clipDecimalPart . snd) tuple
        else ""


formatTag :: (Pretty a) => Config -> a -> Doc AnsiStyle
formatTag conf =
  annotate conf.tagStyle
    . pretty


invalidUlidMsg :: FullTask -> Doc AnsiStyle
invalidUlidMsg task =
  "Id"
    <+> dquotes (pretty task.ulid)
    <+> "is an invalid ulid and could not be converted to a datetime"


-- | Convert seconds into a short, fractional notation like 1.5y, 2.3mo, …
formatDuration :: Seconds -> T.Text
formatDuration (Seconds seconds) = do
  let
    secInHour = 3600.0
    secInDay = 24 * secInHour
    secInMonth = 30 * secInDay
    secInYear = 12 * secInMonth

    years = fromIntegral seconds / secInYear
    months = fromIntegral seconds / secInMonth
    days = fromIntegral seconds / secInDay
    hours = fromIntegral seconds / secInHour

    format :: Double -> [Char] -> T.Text
    format val suffix =
      T.pack $ printf "%.1f%-2s" val suffix

  case () of
    _
      | abs years > 1 -> format years "y"
      | abs months > 1 -> format months "mo"
      | abs days > 1 -> format days "d"
      | otherwise -> format hours "h"


formatTaskPriority :: Config -> FullTask -> Doc AnsiStyle
formatTaskPriority conf task = do
  let
    prio = fromMaybe 0 task.priority
    txt = T.justifyRight 4 ' ' $ showAtPrecision 1 $ realToFrac prio
  annotate conf.priorityStyle (pretty txt)


formatTaskDue :: Config -> DateTime -> FullTask -> Doc AnsiStyle
formatTaskDue conf now task = do
  let
    dueUtcMaybe = task.due_utc >>= parseUtc
    formatTaskDuration due =
      timeDiff due now
        & formatDuration
        & T.center (colToWidth conf 0 AgeCol) ' '
        & T.replace ".0" "  "

  annotate
    (dateStyle conf)
    (pretty $ maybe (T.replicate 6 " ") formatTaskDuration dueUtcMaybe)


formatTaskClose :: Config -> FullTask -> Doc AnsiStyle
formatTaskClose conf task = do
  let closedUtcMaybe = task.closed_utc >>= parseUtc <&> timePrint conf.utcFormat
  annotate conf.closedStyle (pretty closedUtcMaybe)


-- | Format tags for display in a dedicated column (comma-separated)
formatTaskTagsComma :: Config -> FullTask -> Doc AnsiStyle
formatTaskTagsComma conf task =
  case task.tags of
    Nothing -> mempty
    Just tags -> concatWith (surround ",") (tags <&> formatTag conf)


-- | Format tags for display appended to the body (+tag style)
formatTaskTags :: Config -> FullTask -> Doc AnsiStyle
formatTaskTags conf task = do
  let tags = fromMaybe [] task.tags
  case tags of
    [] -> mempty
    _ -> hsep (tags <&> (\t -> annotate conf.tagStyle ("+" <> pretty t)))


formatTaskNotes :: FullTask -> Doc AnsiStyle
formatTaskNotes task =
  if not $ P.null task.notes then "📝" else ""


formatTaskId :: Config -> Int -> FullTask -> Doc AnsiStyle
formatTaskId conf taskWidth task = do
  let id = pretty $ T.takeEnd taskWidth task.ulid
  annotate conf.idStyle id


formatTaskBody :: Config -> DateTime -> FullTask -> Doc AnsiStyle
formatTaskBody conf now task = pretty reviewIcon <> dueSoon <> body
  where
    dueIn offset =
      let dateMaybe = task.due_utc >>= parseUtc
      in  isJust dateMaybe && dateMaybe < Just (now `timeAdd` offset)
    grayOutIfDone doc =
      if isOpen
        then annotate conf.bodyStyle doc
        else annotate conf.bodyClosedStyle doc

    isOpen = isNothing task.closed_utc
    reviewIcon = case task.review_utc >>= parseUtc of
      Nothing -> "" :: Text
      Just date_ -> if date_ < now then "🔎 " else ""
    dueSoon = if dueIn mempty{durationHours = 24} && isOpen then "⚠️️ " else ""
    taskBody =
      reflow $
        if countChar '\n' task.body > 0
          then (task.body & T.takeWhile (/= '\n')) <> " ▼"
          else task.body
    body =
      if dueIn mempty && isOpen
        then annotate (colr conf Red) taskBody
        else grayOutIfDone taskBody


formatTaskOpenedUTC :: Config -> DateTime -> FullTask -> Doc AnsiStyle
formatTaskOpenedUTC conf _now task = do
  let
    dateMaybe = ulidTextToDateTime task.ulid
    formatTaskDate = T.pack . timePrint ISO8601_Date

  annotate (dateStyle conf) (pretty $ maybe "bad Ulid" formatTaskDate dateMaybe)


formatTaskAge :: Config -> DateTime -> FullTask -> Doc AnsiStyle
formatTaskAge conf now task = do
  let
    dateMaybe = ulidTextToDateTime task.ulid
    formatTaskDuration =
      timeDiff now
        >>> formatDuration
        >>> T.center (colToWidth conf 0 AgeCol) ' '
        >>> T.replace ".0" "  "

  annotate
    (dateStyle conf)
    (pretty $ maybe "bad Ulid" formatTaskDuration dateMaybe)


colToWidth :: Config -> Int -> Column -> Int
colToWidth conf idColWidth = \case
  IdCol -> idColWidth
  PrioCol -> conf.prioWidth
  OpenedUTCCol -> conf.dateWidth
  AgeCol -> 6
  DueCol -> 6
  BodyCol -> conf.bodyWidth
  TagsCol -> conf.tagsWidth
  EmptyCol -> 0


formatTaskLine :: Config -> DateTime -> Int -> FullTask -> Doc AnsiStyle
formatTaskLine conf now idColWidth task = do
  let
    columns = conf.columns & P.filter (/= EmptyCol)
    hasTagsCol = TagsCol `P.elem` columns
    multilineIndent = 2
    hangWidth =
      ( (columns & P.filter (/= BodyCol) <&> colToWidth conf idColWidth)
          <> [multilineIndent]
      )
        & P.intersperse 2
        & P.sum
    hhsep = concatWith (<++>)
    isEmptyDoc doc = show doc /= ("" :: Text)
    fields =
      columns
        & P.concatMap
          ( \case
              IdCol -> [formatTaskId conf idColWidth task]
              PrioCol -> [formatTaskPriority conf task]
              OpenedUTCCol -> [formatTaskOpenedUTC conf now task]
              AgeCol -> [formatTaskAge conf now task]
              DueCol -> [formatTaskDue conf now task]
              TagsCol -> [fill (colToWidth conf idColWidth TagsCol) (formatTaskTagsComma conf task)]
              BodyCol ->
                [ formatTaskBody conf now task
                , formatTaskClose conf task
                ]
                  <> [formatTaskTags conf task | not hasTagsCol]
                  <> [formatTaskNotes task]
              EmptyCol -> []
          )
  hang hangWidth $ hhsep $ P.filter isEmptyDoc fields


getIdLength :: Float -> Int
getIdLength numOfItems =
  let
    targetCollisionChance = 0.01 -- Targeted likelihood of id collisions
    sizeOfAlphabet = 32 -- Crockford's base 32 alphabet
    -- Required ID space size using birthday problem approximation
    requiredSpace =
      (numOfItems P.** 2) / (2 * P.log (1 / (1 - targetCollisionChance)))
  in
    max 1 (ceiling (logBase sizeOfAlphabet requiredSpace))


countTasks :: Config -> Connection -> Maybe [Text] -> IO (Doc AnsiStyle)
countTasks conf connection filterExpression = do
  let
    parserResults =
      readP_to_S filterExpsParser $
        T.unpack (unwords $ fromMaybe [""] filterExpression)
    filterMay = listToMaybe parserResults

  case filterMay of
    Nothing -> do
      [NumRows taskCount] <-
        query_ connection $
          Query $
            "SELECT count(1) FROM " <> quoteKeyword conf.tableName

      pure $ pretty taskCount
    Just (filterExps, _) -> do
      let
        ppInvalidFilter = \case
          (InvalidFilter error) ->
            dquotes (pretty error) <+> "is an invalid filter"
          (HasStatus Nothing) -> "Filter contains an invalid state value"
          _ -> "The functions should not be called with a valid function"
        errors = P.filter (not . isValidFilter) filterExps
        errorsDoc =
          if P.length errors > 0
            then
              Just $
                vsep (fmap (annotate (colr conf Red) . ppInvalidFilter) errors)
                  <> hardline
                  <> hardline
            else Nothing

      tasks <- query_ connection (getFilterQuery filterExps Nothing Nothing)
      pure $ errorsDoc & fromMaybe (pretty $ P.length (tasks :: [FullTask]))


-- TODO: Print number of remaining tasks and how to display them at the bottom
headTasks :: Config -> DateTime -> Connection -> Maybe Int -> IO (Doc AnsiStyle)
headTasks conf now connection availableLinesMb = do
  let taskCount = do
        let availableLines = availableLinesMb & fromMaybe 0
        if P.isJust availableLinesMb && availableLines < conf.headCount
          then availableLines
          else conf.headCount

  tasks <-
    queryNamed
      connection
      -- TODO: Add `wait_utc` < datetime('now')"
      [sql|
        SELECT *
        FROM tasks_view
        WHERE closed_utc IS NULL
        ORDER BY
          priority DESC,
          due_utc ASC,
          ulid DESC
        LIMIT :taskCount
      |]
      [":taskCount" := taskCount]

  formatTasksColor
    conf
    connection
    now
    (wasListTruncated availableLinesMb tasks)
    tasks


newTasks ::
  Config ->
  DateTime ->
  Connection ->
  Maybe [Text] ->
  Maybe Int ->
  IO (Doc AnsiStyle)
newTasks conf now connection filterExp availableLinesMb = do
  let
    parserResults =
      readP_to_S filterExpsParser $
        T.unpack (unwords $ fromMaybe [""] filterExp)
    filterMay = listToMaybe parserResults

  case filterMay of
    Nothing -> do
      tasks <-
        queryNamed
          connection
          [sql|
            SELECT *
            FROM tasks_view
            ORDER BY ulid DESC
            LIMIT :taskCount
          |]
          [ ":taskCount" := case availableLinesMb of
              Nothing -> -1 -- No limit
              Just availableLines -> availableLines
          ]

      formatTasksColor
        conf
        connection
        now
        (wasListTruncated availableLinesMb tasks)
        tasks
    --
    Just (filterExps, _) -> do
      let
        ppInvalidFilter = \case
          (InvalidFilter error) ->
            dquotes (pretty error) <+> "is an invalid filter"
          (HasStatus Nothing) -> "Filter contains an invalid state value"
          _ -> "The functions should not be called with a valid function"
        errors = P.filter (not . isValidFilter) filterExps

      if P.length errors > 0
        then
          pure $
            vsep (fmap (annotate (colr conf Red) . ppInvalidFilter) errors)
              <> hardline
              <> hardline
        else do
          tasks <-
            query_
              connection
              (getFilterQuery filterExps (Just "ulid DESC") availableLinesMb)
          formatTasksColor
            conf
            connection
            now
            (wasListTruncated availableLinesMb tasks)
            tasks


listOldTasks ::
  Config -> DateTime -> Connection -> Maybe Int -> IO (Doc AnsiStyle)
listOldTasks conf now connection availableLinesMb = do
  tasks <-
    queryNamed
      connection
      [sql|
        SELECT *
        FROM tasks_view
        WHERE closed_utc IS NULL
        ORDER BY ulid ASC
        LIMIT :taskCount
      |]
      [ ":taskCount" := case availableLinesMb of
          Nothing -> -1 -- No limit
          Just availableLines -> availableLines
      ]

  formatTasksColor
    conf
    connection
    now
    (wasListTruncated availableLinesMb tasks)
    tasks


openTasks ::
  Config ->
  DateTime ->
  Connection ->
  Maybe [Text] ->
  Maybe Int ->
  IO (Doc AnsiStyle)
openTasks conf now connection filterExpression availableLinesMb = do
  let
    parserResults =
      readP_to_S filterExpsParser $
        T.unpack (unwords $ fromMaybe [""] filterExpression)
    filterMay = listToMaybe parserResults

  case filterMay of
    Nothing -> do
      (tasks :: [FullTask]) <-
        queryNamed
          connection
          [sql|
            SELECT *
            FROM tasks_view
            WHERE closed_utc IS NULL
            ORDER BY priority DESC, due_utc ASC, ulid DESC
            LIMIT :taskCount
          |]
          [ ":taskCount" := case availableLinesMb of
              Nothing -> -1 -- No limit
              Just availableLines -> availableLines
          ]

      formatTasksColor
        conf
        connection
        now
        (wasListTruncated availableLinesMb tasks)
        tasks
    --
    Just (filterExps, _) -> do
      let
        ppInvalidFilter = \case
          (InvalidFilter error) ->
            dquotes (pretty error) <+> "is an invalid filter"
          (HasStatus Nothing) -> "Filter contains an invalid state value"
          _ -> "The functions should not be called with a valid function"
        errors = P.filter (not . isValidFilter) filterExps

      if P.length errors > 0
        then
          pure $
            vsep (fmap (annotate (colr conf Red) . ppInvalidFilter) errors)
              <> hardline
              <> hardline
        else do
          let
            isStateExp = \case (HasStatus _) -> True; _ -> False
            filterExpWithOpen =
              if P.any isStateExp filterExps
                then filterExps
                else HasStatus (Just IsOpen) : filterExps
            sqlQuery =
              getFilterQuery
                filterExpWithOpen
                (Just "priority DESC, due_utc ASC, ulid DESC")
                availableLinesMb

          tasks <- query_ connection sqlQuery
          formatTasksColor
            conf
            connection
            now
            (wasListTruncated availableLinesMb tasks)
            tasks


modifiedTasks ::
  Config ->
  DateTime ->
  Connection ->
  ListModifiedFlag ->
  Maybe Int ->
  IO (Doc AnsiStyle)
modifiedTasks conf now connection listModifiedFlag availableLinesMb = do
  tasks <-
    queryNamed
      connection
      [sql|
        SELECT *
        FROM tasks_view
        ORDER BY modified_utc Desc
        LIMIT :taskCount
      |]
      [ ":taskCount" := case availableLinesMb of
          Nothing -> -1 -- No limit
          Just availableLines -> availableLines
      ]

  let
    filterModified =
      P.filter
        ( \task ->
            removeNSec (ulidTextToDateTime task.ulid)
              /= parseUtc task.modified_utc
        )

    removeNSec :: Maybe DateTime -> Maybe DateTime
    removeNSec mDateTime =
      case mDateTime of
        Just dateTime ->
          Just $
            dateTime{dtTime = (dtTime dateTime){todNSec = 0}}
        Nothing -> Nothing

    filteredTasks =
      case listModifiedFlag of
        AllItems -> tasks
        ModifiedItemsOnly -> filterModified tasks

  formatTasksColor
    conf
    connection
    now
    (wasListTruncated availableLinesMb filteredTasks)
    filteredTasks


overdueTasks ::
  Config -> DateTime -> Connection -> Maybe Int -> IO (Doc AnsiStyle)
overdueTasks conf now connection availableLinesMb = do
  tasks <-
    queryNamed
      connection
      [sql|
        SELECT *
        FROM tasks_view
        WHERE
          closed_utc IS NULL AND
          due_utc < datetime('now')
        ORDER BY
          priority DESC,
          due_utc ASC,
          ulid Desc
        LIMIT :taskCount
      |]
      [ ":taskCount" := case availableLinesMb of
          Nothing -> -1 -- No limit
          Just availableLines -> availableLines
      ]

  formatTasksColor
    conf
    connection
    now
    (wasListTruncated availableLinesMb tasks)
    tasks


doneTasks :: Config -> DateTime -> Connection -> Maybe Int -> IO (Doc AnsiStyle)
doneTasks conf now connection availableLinesMb = do
  tasks <-
    queryNamed
      connection
      [sql|
        SELECT *
        FROM tasks_view
        WHERE
          closed_utc IS NOT NULL AND
          state == 'Done'
        ORDER BY closed_utc DESC
        LIMIT :taskCount
      |]
      [ ":taskCount" := case availableLinesMb of
          Nothing -> -1 -- No limit
          Just availableLines -> availableLines
      ]

  formatTasksColor
    conf
    connection
    now
    (wasListTruncated availableLinesMb tasks)
    tasks


obsoleteTasks ::
  Config -> DateTime -> Connection -> Maybe Int -> IO (Doc AnsiStyle)
obsoleteTasks conf now connection availableLinesMb = do
  tasks <-
    queryNamed
      connection
      [sql|
        SELECT *
        FROM tasks_view
        WHERE
          closed_utc IS NOT NULL AND
          state == 'Obsolete'
        ORDER BY ulid DESC
        LIMIT :taskCount
      |]
      [ ":taskCount" := case availableLinesMb of
          Nothing -> -1 -- No limit
          Just availableLines -> availableLines
      ]

  formatTasksColor
    conf
    connection
    now
    (wasListTruncated availableLinesMb tasks)
    tasks


deletableTasks ::
  Config -> DateTime -> Connection -> Maybe Int -> IO (Doc AnsiStyle)
deletableTasks conf now connection availableLinesMb = do
  tasks <-
    queryNamed
      connection
      [sql|
        SELECT *
        FROM tasks_view
        WHERE
          closed_utc IS NOT NULL
          AND state == 'Deletable'
        ORDER BY ulid DESC
        LIMIT :taskCount
      |]
      [ ":taskCount" := case availableLinesMb of
          Nothing -> -1 -- No limit
          Just availableLines -> availableLines
      ]
  formatTasksColor
    conf
    connection
    now
    (wasListTruncated availableLinesMb tasks)
    tasks


listRepeating ::
  Config -> DateTime -> Connection -> Maybe Int -> IO (Doc AnsiStyle)
listRepeating conf now connection availableLinesMb = do
  tasks <-
    queryNamed
      connection
      [sql|
        SELECT *
        FROM tasks_view
        WHERE repetition_duration IS NOT NULL
        ORDER BY repetition_duration Desc
        LIMIT :taskCount
      |]
      [ ":taskCount" := case availableLinesMb of
          Nothing -> -1 -- No limit
          Just availableLines -> availableLines
      ]

  formatTasksColor
    conf
    connection
    now
    (wasListTruncated availableLinesMb tasks)
    tasks


listRecurring ::
  Config -> DateTime -> Connection -> Maybe Int -> IO (Doc AnsiStyle)
listRecurring conf now connection availableLinesMb = do
  tasks <-
    queryNamed
      connection
      [sql|
        SELECT *
        FROM tasks_view
        WHERE recurrence_duration IS NOT NULL
        ORDER BY recurrence_duration DESC
        LIMIT :taskCount
      |]
      [ ":taskCount" := case availableLinesMb of
          Nothing -> -1 -- No limit
          Just availableLines -> availableLines
      ]

  formatTasksColor
    conf
    connection
    now
    (wasListTruncated availableLinesMb tasks)
    tasks


listReady ::
  Config -> DateTime -> Connection -> Maybe P.Int -> IO (Doc AnsiStyle)
listReady conf now connection availableLinesMb = do
  tasks <-
    queryNamed
      connection
      [sql|
        SELECT *
        FROM tasks_view
        WHERE
          (ready_utc IS NULL OR
            (ready_utc IS NOT NULL AND ready_utc < datetime('now'))
          ) AND
          waiting_utc IS NULL AND
          ready_utc IS NULL AND
          closed_utc IS NULL
        ORDER BY
          priority DESC,
          due_utc ASC,
          ulid DESC
        LIMIT :taskCount
      |]
      [ ":taskCount" := case availableLinesMb of
          Nothing -> -1 -- No limit
          Just availableLines -> availableLines
      ]

  formatTasksColor
    conf
    connection
    now
    (wasListTruncated availableLinesMb tasks)
    tasks


listWaiting ::
  Config -> DateTime -> Connection -> Maybe Int -> IO (Doc AnsiStyle)
listWaiting conf now connection availableLinesMb = do
  tasks <-
    queryNamed
      connection
      [sql|
        SELECT *
        FROM tasks_view
        WHERE closed_utc IS NULL
          AND waiting_utc IS NOT NULL
          AND (review_utc > datetime('now') OR review_utc IS NULL)
        ORDER BY waiting_utc DESC
        LIMIT :taskCount
      |]
      [ ":taskCount" := case availableLinesMb of
          Nothing -> -1 -- No limit
          Just availableLines -> availableLines
      ]

  formatTasksColor
    conf
    connection
    now
    (wasListTruncated availableLinesMb tasks)
    tasks


listAll :: Config -> DateTime -> Connection -> Maybe Int -> IO (Doc AnsiStyle)
listAll conf now connection availableLinesMb = do
  tasks <-
    queryNamed
      connection
      [sql|
        SELECT *
        FROM tasks_view
        ORDER BY ulid ASC
        LIMIT :taskCount
      |]
      [ ":taskCount" := case availableLinesMb of
          Nothing -> -1 -- No limit
          Just availableLines -> availableLines
      ]

  formatTasksColor
    conf
    connection
    now
    (wasListTruncated availableLinesMb tasks)
    tasks


listNoTag :: Config -> DateTime -> Connection -> Maybe Int -> IO (Doc AnsiStyle)
listNoTag conf now connection availableLinesMb = do
  tasks <-
    queryNamed
      connection
      [sql|
        SELECT *
        FROM tasks_view
        WHERE
          closed_utc IS NULL AND
          tags IS NULL
        ORDER BY
          priority DESC,
          due_utc ASC,
          ulid DESC
        LIMIT :taskCount
      |]
      [ ":taskCount" := case availableLinesMb of
          Nothing -> -1 -- No limit
          Just availableLines -> availableLines
      ]

  formatTasksColor
    conf
    connection
    now
    (wasListTruncated availableLinesMb tasks)
    tasks


getWithTag ::
  Connection ->
  Maybe DerivedState ->
  Maybe Int ->
  [Text] ->
  IO [FullTask]
getWithTag connection stateMaybe availableLinesMb tags = do
  let
    tagQuery = case tags of
      [] -> ""
      _ ->
        tags
          <&> (\t -> "tag LIKE '" <> t <> "'")
          & T.intercalate " OR "
          & ("AND " <>)

    stateQuery = case stateMaybe of
      Nothing -> ""
      Just derivedState -> "AND " <> derivedStateToQuery derivedState

    -- `WHERE TRUE` simplifies adding additional filters with "AND"
    ulidsQuery =
      "\
      \SELECT tasks.ulid \n\
      \FROM tasks \n\
      \LEFT JOIN task_to_tag ON tasks.ulid IS task_to_tag.task_ulid \n\
      \WHERE TRUE \n\
      \"
        <> tagQuery
        <> " \
           \"
        <> stateQuery
        <> " \
           \GROUP BY tasks.ulid \
           \HAVING count(tag) = "
        <> show (P.length tags)

    mainQuery =
      FullTask.selectQuery
        <> "\
           \FROM ("
        <> Query ulidsQuery
        <> ") tasks1\n\
           \LEFT JOIN tasks_view ON tasks1.ulid IS tasks_view.ulid\n\
           \ORDER BY \n\
           \  priority DESC,\n\
           \  due_utc ASC,\n\
           \  ulid DESC\n\
           \LIMIT "
        <> Query
          ( show @Int $ case availableLinesMb of
              Nothing -> -1 -- No limit
              Just availableLines -> availableLines
          )

  query_ connection mainQuery


listWithTag ::
  Config -> DateTime -> Connection -> [Text] -> Maybe Int -> IO (Doc AnsiStyle)
listWithTag conf now connection tags availableLinesMb = do
  tasks <- getWithTag connection Nothing availableLinesMb tags
  formatTasksColor
    conf
    connection
    now
    (wasListTruncated availableLinesMb tasks)
    tasks


queryTasks ::
  Config -> DateTime -> Connection -> Text -> Maybe Int -> IO (Doc AnsiStyle)
queryTasks conf now connection sqlQuery availableLinesMb = do
  let
    -- Check if query ends with LIMIT clause by finding last "limit" and
    -- checking if only digits/spaces follow
    queryLower = T.toLower $ T.strip sqlQuery
    endsWithLimit = case T.breakOnEnd "limit" queryLower of
      ("", _) -> False -- No "limit" found
      (_, after) ->
        T.all (\c -> isSpace c || isDigit c) after
          && T.any isDigit after
    limitClause = case availableLinesMb of
      Nothing -> ""
      Just availableLines ->
        if endsWithLimit then "" else " LIMIT " <> show availableLines
  tasks <-
    query_ connection $
      Query $
        "SELECT * FROM tasks_view WHERE "
          <> sqlQuery
          <> T.pack limitClause
  formatTasksColor
    conf
    connection
    now
    (wasListTruncated availableLinesMb tasks && not endsWithLimit)
    tasks


runSql :: Config -> Text -> IO (Doc AnsiStyle)
runSql conf sqlQuery = do
  result <-
    readProcess
      "sqlite3"
      [ conf.dataDir </> conf.dbName
      , ".headers on"
      , ".mode csv"
      , ".separator , '\n'"
      , T.unpack sqlQuery
      ]
      []
  -- Remove trailing newline
  pure $ pretty (T.dropEnd 1 $ T.pack result)


data FilterExp
  = HasTag Text
  | NotTag Text
  | HasDue Text
  | HasStatus (Maybe DerivedState) -- Should be `Either`
  | InvalidFilter Text
  deriving (Show)


tagParser :: ReadP FilterExp
tagParser = do
  _ <- char '+'
  aTag <- munch (not . isSpace)
  pure $ HasTag $ T.pack aTag


notTagParser :: ReadP FilterExp
notTagParser = do
  _ <- char '-'
  aTag <- munch (not . isSpace)
  pure $ NotTag $ T.pack aTag


dueParser :: ReadP FilterExp
dueParser = do
  _ <- string "due:"
  utcStr <- munch (not . isSpace)
  pure $ HasDue $ T.pack utcStr


stateParser :: ReadP FilterExp
stateParser = do
  _ <- string "state:"
  stateStr <- munch (not . isSpace)
  pure $ HasStatus $ textToDerivedState $ T.pack stateStr


filterExpParser :: ReadP FilterExp
filterExpParser =
  do
    tagParser
    <++ notTagParser
    <++ dueParser
    <++ stateParser
    <++ (InvalidFilter . T.pack <$> munch1 (not . isSpace))


filterExpsParser :: ReadP [FilterExp]
filterExpsParser = do
  val <- sepBy1 filterExpParser skipSpaces
  eof
  return val


parseFilterExps :: Text -> IO ()
parseFilterExps input =
  P.print $ readP_to_S filterExpsParser $ T.unpack input


{-| Returns (operator, where-query) tuple
 TODO: Should be `FilterExp -> Maybe (Text, Text)`
-}
filterToSql :: FilterExp -> (Text, Text)
filterToSql = \case
  HasTag tag -> ("INTERSECT", "tag LIKE " <> quoteText tag)
  NotTag tag -> ("EXCEPT", "tag LIKE " <> quoteText tag)
  HasDue utc -> ("INTERSECT", "due_utc < datetime(" <> quoteText utc <> ")")
  HasStatus (Just taskState) -> ("INTERSECT", derivedStateToQuery taskState)
  -- Following cases should never be called, as they are filtered out
  HasStatus Nothing -> ("", "")
  InvalidFilter _ -> ("", "")


isValidFilter :: FilterExp -> Bool
isValidFilter = \case
  InvalidFilter _ -> False
  HasStatus Nothing -> False
  _ -> True


runFilter ::
  Config -> DateTime -> Connection -> [Text] -> Maybe Int -> IO (Doc AnsiStyle)
runFilter conf now connection exps availableLinesMb = do
  let
    parserResults = readP_to_S filterExpsParser $ T.unpack $ unwords exps
    filterHelp =
      "Filter expressions must be a list of key[:value] entries."
        <> hardline
    dieWithError err = do
      hPutDoc stderr $ annotate (colr conf Red) err
      exitFailure

  case parserResults of
    [(filterExps, _)] -> do
      let
        ppInvalidFilter = \case
          (InvalidFilter error) ->
            dquotes (pretty error)
              <+> "is an invalid filter."
                <> hardline
                <> filterHelp
          (HasStatus Nothing) ->
            "Filter contains an invalid state value"
          _ ->
            "The functions should not be called with a valid function"
        errors = P.filter (not . isValidFilter) filterExps
        sqlQuery = getFilterQuery filterExps Nothing availableLinesMb

      tasks <- query_ connection sqlQuery

      if P.length errors > 0
        then dieWithError $ vsep (fmap ppInvalidFilter errors)
        else
          if P.length tasks <= 0
            then pure "No tasks available for the given filter."
            else
              formatTasksColor
                conf
                connection
                now
                (wasListTruncated availableLinesMb tasks)
                tasks
    _ -> dieWithError filterHelp


-- TODO: Increase performance of this query
getFilterQuery :: [FilterExp] -> Maybe Text -> Maybe Int -> Query
getFilterQuery filterExps orderByMb availableLinesMb = do
  let
    filterTuple =
      filterToSql <$> P.filter isValidFilter filterExps

    queries =
      filterTuple <&> \(operator, whereQuery) ->
        operator
          <> "\n\
             \SELECT tasks.ulid\n\
             \FROM tasks\n\
             \LEFT JOIN task_to_tag ON tasks.ulid IS task_to_tag.task_ulid\n\
             \WHERE "
          <> whereQuery
          <> "\n\
             \GROUP BY tasks.ulid"

    ulidsQuery =
      "SELECT tasks.ulid FROM tasks\n"
        <> unlines queries

    orderBy = Query $ case orderByMb of
      Nothing ->
        "ORDER BY \n\
        \  priority DESC,\n\
        \  due_utc ASC,\n\
        \  ulid DESC\n"
      Just orderByTxt ->
        "ORDER BY \n" <> orderByTxt <> "\n"

  FullTask.selectQuery
    <> "FROM ("
    <> Query ulidsQuery
    <> ") tasks1\n\
       \LEFT JOIN tasks_view ON tasks1.ulid IS tasks_view.ulid\n"
    <> orderBy
    <> Query
      ( "LIMIT "
          <> ( show @P.Int $ case availableLinesMb of
                Nothing -> -1 -- No limit
                Just availableLines -> availableLines
             )
          <> "\n"
      )


columnToDoc :: Config -> Int -> Column -> Doc AnsiStyle
columnToDoc conf idColWidth = do
  let strong = maybeBold conf <> maybeUnderlined conf

  \case
    IdCol ->
      annotate
        (conf.idStyle <> strong)
        (fill idColWidth "Id")
    PrioCol ->
      annotate
        (conf.priorityStyle <> strong)
        (fill (colToWidth conf idColWidth PrioCol) "Prio")
    OpenedUTCCol ->
      annotate
        (conf.dateStyle <> strong)
        (fill (colToWidth conf idColWidth OpenedUTCCol) "Opened UTC")
    AgeCol ->
      annotate
        (conf.dateStyle <> strong)
        (fill (colToWidth conf idColWidth AgeCol) "Age")
    DueCol ->
      annotate
        (conf.dateStyle <> strong)
        (fill (colToWidth conf idColWidth DueCol) "Due")
    BodyCol ->
      annotate
        (conf.bodyStyle <> strong)
        (fill (colToWidth conf idColWidth BodyCol) "Body")
    TagsCol ->
      annotate
        (conf.tagStyle <> strong)
        (fill (colToWidth conf idColWidth TagsCol) "Tags")
    EmptyCol ->
      mempty


wasListTruncated :: Maybe Int -> [a] -> Bool
wasListTruncated availableLinesMb list = case availableLinesMb of
  Nothing -> False
  Just availableLines -> P.length list P.>= availableLines


formatTasks :: Config -> DateTime -> Int -> Bool -> [FullTask] -> Doc AnsiStyle
formatTasks conf now idColWidth isTruncated tasks =
  if P.length tasks == 0
    then pretty noTasksWarning
    else do
      let
        docHeader =
          concatWith (<++>) $
            ( conf.columns
                & P.filter (/= EmptyCol)
                <&> columnToDoc conf idColWidth
            )
              <> [line]

      docHeader
        <> vsep (fmap (formatTaskLine conf now idColWidth) tasks)
        <> line
        <> if isTruncated
          then
            annotate
              (colr conf Yellow)
              ( "This list is truncated. "
                  <> "List all by piping into `cat` or `less`."
              )
          else mempty


formatTasksColor ::
  Config -> Connection -> DateTime -> Bool -> [FullTask] -> IO (Doc AnsiStyle)
formatTasksColor conf connection now isTruncated tasks = do
  confNorm <- applyColorMode conf
  totalTaskCount <- getTotalTaskCount connection
  let
    -- Use at least the number of tasks in the list for ID length calculation.
    -- Establishes a minimum amount of tasks in the database.
    minCount = max (fromIntegral totalTaskCount) (fromIntegral $ P.length tasks)
    idColWidth = getIdLength minCount
  pure $ formatTasks confNorm now idColWidth isTruncated tasks


getProgressBar :: Config -> Integer -> Double -> Doc AnsiStyle
getProgressBar conf maxWidthInChars progress =
  let
    barWidth = floor (progress * fromInteger maxWidthInChars)
    remainingWidth = fromIntegral $ maxWidthInChars - barWidth
  in
    annotate
      (bgColrDull conf Green <> colrDull conf Green)
      (pretty $ P.take (fromIntegral barWidth) $ P.repeat '#')
      <>
      -- (annotate (bgColrDull conf Green) $ fill (fromIntegral barWidth) "" <>
      annotate (bgColrDull conf Black) (fill remainingWidth "")


formatTagLine ::
  Config -> Int -> (Text, Integer, Integer, Double) -> Doc AnsiStyle
formatTagLine conf maxTagLength (tag, open_count, closed_count, progress) =
  let
    barWidth = toInteger $ progressBarWidth conf
    progressPercentage =
      if progress == 0
        then "     "
        else
          pretty
            ( T.justifyRight 3 ' ' $
                T.pack $
                  showFFloat (Just 0) (progress * 100) ""
            )
            <+> "%"
  in
    fill maxTagLength (pretty tag)
      <++> pretty (T.justifyRight (T.length "open") ' ' $ show open_count)
      <++> pretty (T.justifyRight (T.length "closed") ' ' $ show closed_count)
      <++> progressPercentage
      <+> getProgressBar conf barWidth progress


formatTags :: Config -> [(Text, Integer, Integer, Double)] -> Doc AnsiStyle
formatTags conf tagTuples = do
  let
    percWidth = 6 -- Width of e.g. 100 %
    progressWith = conf.progressBarWidth + percWidth
    firstOf4 (a, _, _, _) = a
    maxTagLength =
      tagTuples
        <&> (T.length . firstOf4)
        & P.maximum

  if P.null tagTuples
    then
      annotate
        (colr conf Yellow)
        "⚠️ No tags available"
    else
      annotate (maybeBold conf <> maybeUnderlined conf) (fill maxTagLength "Tag")
        <++> annotate (maybeBold conf <> maybeUnderlined conf) "Open"
        <++> annotate (maybeBold conf <> maybeUnderlined conf) "Closed"
        <++> annotate (maybeBold conf <> maybeUnderlined conf) (fill progressWith "Progress")
        <> line
        <> vsep (fmap (formatTagLine conf maxTagLength) tagTuples)


listTags :: Config -> Connection -> IO (Doc AnsiStyle)
listTags conf connection = do
  tags <- query_ connection $ Query "SELECT * FROM tags"

  pure $ formatTags conf tags


listProjects :: Config -> Connection -> IO (Doc AnsiStyle)
listProjects conf connection = do
  tags <-
    query_
      connection
      [sql|
        SELECT *
        FROM tags
        WHERE
          "open" > 0 AND
          closed > 0
      |]

  pure $
    if P.null tags
      then
        annotate (colr conf Yellow) $
          "⚠️ No projects available yet. "
            <> "Tag some tasks to populate this list."
      else formatTags conf tags


listNotes :: Config -> Connection -> IO (Doc AnsiStyle)
listNotes conf connection = do
  (notes :: [TaskToNote]) <-
    query_
      connection
      [sql|
        SELECT ulid, task_ulid, note
        FROM task_to_note
        ORDER BY ulid DESC
      |]

  let
    taskIdWidth = 7 -- TODO: Use dynamic width
    noteWidth = getIdLength $ fromIntegral $ P.length notes
    docHeader =
      annotate
        (conf.idStyle <> maybeBold conf <> maybeUnderlined conf)
        (fill taskIdWidth "Task ID")
        <++> annotate
          (conf.idStyle <> maybeBold conf <> maybeUnderlined conf)
          (fill noteWidth "ID")
        <++> annotate
          (conf.dateStyle <> maybeBold conf <> maybeUnderlined conf)
          "Created UTC"
        <++> annotate (maybeBold conf <> maybeUnderlined conf) "Note"
        <++> line

    showNote note =
      annotate
        conf.idStyle
        (fill 7 $ pretty $ T.takeEnd taskIdWidth note.task_ulid)
        <++> annotate
          conf.idStyle
          (fill noteWidth $ pretty $ T.takeEnd noteWidth note.ulid)
        <++> annotate
          conf.dateStyle
          ( note.ulid
              & ulidTextToDateTime
              <&> timePrint ISO8601_Date
              & pretty
          )
        <++> pretty note.note
        <> line

  pure $ docHeader <> vsep (notes <&> showNote)


getStats :: Config -> Connection -> IO (Doc AnsiStyle)
getStats conf connection = do
  -- Update SQLite's internal statistics for better query optimization
  execute_ connection "ANALYZE"

  [NumRows numOfTasksTotal] <-
    query_ connection $
      Query "SELECT count(1) FROM tasks"
  [NumRows numOfTasksOpen] <-
    query_ connection $
      Query "SELECT count(1) FROM tasks WHERE closed_utc IS NULL"
  [NumRows numOfTasksClosed] <-
    query_ connection $
      Query "SELECT count(1) FROM tasks WHERE closed_utc IS NOT NULL"
  [NumRows numOfTasksDone] <-
    query_ connection $
      Query "SELECT count(1) FROM tasks WHERE state IS 'Done'"
  [NumRows numOfTasksObsolete] <-
    query_ connection $
      Query "SELECT count(1) FROM tasks WHERE state IS 'Obsolete'"
  [NumRows numOfTasksDeletable] <-
    query_ connection $
      Query "SELECT count(1) FROM tasks WHERE state IS 'Deletable'"

  -- Fetch all task ULIDs with their states to calculate average age per state
  taskData :: [(Text, Maybe Text)] <-
    query_ connection $
      Query "SELECT ulid, state FROM tasks"

  nowElapsed <- timeCurrentP
  let now = timeFromElapsedP nowElapsed :: DateTime

  -- Helper function to calculate average age for a subset of tasks
  let
    calculateAvgAge :: [(Text, Maybe Text)] -> Text
    calculateAvgAge tasks =
      if P.null tasks
        then "-"
        else
          let
            ages =
              tasks
                & P.mapMaybe
                  ( \(ulid, _) -> do
                      createdDateTime <- ulidTextToDateTime ulid
                      let diff = timeDiff now createdDateTime
                      pure diff
                  )
            totalSeconds = P.sum $ fmap (\(Seconds s) -> s) ages
            count = P.length ages
          in
            if count > 0
              then formatDuration $ Seconds $ totalSeconds `P.div` fromIntegral count
              else "-"

    -- Calculate average age for each state
    avgAgeAll = calculateAvgAge taskData
    avgAgeOpen = calculateAvgAge $ P.filter (\(_, state) -> isNothing state) taskData
    avgAgeClosed = calculateAvgAge $ P.filter (\(_, state) -> isJust state) taskData
    avgAgeDone = calculateAvgAge $ P.filter (\(_, state) -> state == Just "Done") taskData
    avgAgeObsolete = calculateAvgAge $ P.filter (\(_, state) -> state == Just "Obsolete") taskData
    avgAgeDeletable =
      calculateAvgAge $ P.filter (\(_, state) -> state == Just "Deletable") taskData

    widthKey = 14
    widthValue = max 5 $ fromIntegral $ numDigits 10 numOfTasksTotal
    widthAge = 6
    formatLine (name :: Text) (numTasks :: Integer) (avgAge :: Text) =
      let
        numTotalInt :: Double = fromIntegral numOfTasksTotal
        numTasksInt :: Double = fromIntegral numTasks
        share = T.pack $ showFFloat (Just 3) (numTasksInt / numTotalInt) ""
      in
        fill widthKey (pretty name)
          <++> fill
            widthValue
            (pretty $ T.justifyRight widthValue ' ' $ show numTasks)
          <++> fill 7 (pretty share)
          <++> pretty (T.justifyRight widthAge ' ' avgAge)

  pure $
    annotate (maybeBold conf <> maybeUnderlined conf) (fill widthKey "State")
      <++> annotate (maybeBold conf <> maybeUnderlined conf) (fill widthValue "Value")
      <++> annotate (maybeBold conf <> maybeUnderlined conf) (fill 7 "Share")
      <++> annotate (maybeBold conf <> maybeUnderlined conf) (fill widthAge "Avg Age")
      <> line
      <> vsep
        [ formatLine "Any" numOfTasksTotal avgAgeAll
        , formatLine "Open" numOfTasksOpen avgAgeOpen
        , formatLine "Closed" numOfTasksClosed avgAgeClosed
        , formatLine "└─ Done" numOfTasksDone avgAgeDone
        , formatLine "└─ Obsolete" numOfTasksObsolete avgAgeObsolete
        , formatLine "└─ Deletable" numOfTasksDeletable avgAgeDeletable
        ]
