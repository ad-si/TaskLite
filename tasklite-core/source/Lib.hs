{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use maybe" #-}

{-|
Functions to create, update, and delete tasks / tags / notes
-}
module Lib where

import Protolude as P (
  Applicative (liftA2, pure),
  Bool (..),
  Char,
  Double,
  Down (Down),
  Either,
  Eq (..),
  FilePath,
  Float,
  Floating (logBase),
  Foldable (foldMap, length, maximum, null),
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
  any,
  catMaybes,
  const,
  decodeUtf8,
  dropWhile,
  either,
  encodeUtf8,
  exitFailure,
  filter,
  forM,
  forM_,
  fromIntegral,
  fromMaybe,
  fst,
  getArgs,
  head,
  identity,
  intersperse,
  isJust,
  isNothing,
  isSpace,
  lastMay,
  listToMaybe,
  maybe,
  maybeToEither,
  not,
  on,
  otherwise,
  print,
  realToFrac,
  repeat,
  reverse,
  show,
  snd,
  sortBy,
  sortOn,
  stderr,
  take,
  takeWhile,
  unlines,
  unwords,
  ($),
  ($>),
  (&),
  (&&),
  (.),
  (<$>),
  (<&>),
  (||),
 )
import Protolude qualified as P

import Control.Arrow ((>>>))
import Data.Aeson as Aeson (KeyValue ((.=)), encode, object)
import Data.Coerce (coerce)
import Data.Generics (Data, constrFields, toConstr)
import Data.Hourglass (
  DateTime (dtTime),
  Duration (durationHours, durationMinutes),
  ElapsedP,
  ISO8601_Date (ISO8601_Date),
  Minutes (Minutes),
  Time (timeFromElapsedP),
  TimeOfDay (todNSec),
  timeAdd,
  timePrint,
 )
import Data.List (nub)
import Data.Text as T (
  breakOn,
  dropEnd,
  intercalate,
  isPrefixOf,
  justifyRight,
  length,
  pack,
  replace,
  replicate,
  take,
  takeEnd,
  toLower,
  unpack,
  unwords,
  words,
 )
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Time.Clock (UTCTime)
import Data.Time.ISO8601.Duration qualified as Iso
import Data.ULID (ULID, getULID)
import Data.Yaml as Yaml (encode)
import Database.SQLite.Simple (Only (Only))
import Database.SQLite.Simple as Sql (
  Connection,
  FromRow (..),
  NamedParam ((:=)),
  Query (Query),
  SQLData (SQLText),
  ToRow,
  changes,
  execute,
  executeNamed,
  field,
  open,
  query,
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
  vcat,
  vsep,
  (<+>),
 )
import Prettyprinter.Render.Terminal (
  AnsiStyle,
  Color (Black, Green, Red),
  bgColorDull,
  bold,
  color,
  colorDull,
  hPutDoc,
  putDoc,
  underlined,
 )
import Prettyprinter.Util (reflow)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Posix.User (getEffectiveUserName)
import System.Process (readProcess)
import Text.Huzzy qualified as Huzzy
import Text.ParserCombinators.ReadP as ReadP (
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
import Time.System (dateCurrent, timeCurrentP)

import Config (
  Config (
    bodyClosedStyle,
    bodyStyle,
    bodyWidth,
    closedStyle,
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
    utcFormat,
    utcFormatShort
  ),
  HookSet (pre),
  HooksConfig (add),
  defaultConfig,
 )
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
import Note (Note (body, ulid))
import Task (
  DerivedState (IsOpen),
  Task,
  TaskState (..),
  derivedStateToQuery,
  getStateHierarchy,
  textToDerivedState,
  zeroTask,
 )
import Task qualified
import TaskToNote (TaskToNote (..))
import TaskToTag (TaskToTag (..))
import Utils (
  IdText,
  ListModifiedFlag (..),
  applyColorMode,
  dateTimeToUtcTime,
  executeHooks,
  numDigits,
  parseUlidText,
  parseUtc,
  setDateTime,
  ulidTextToDateTime,
  utcFormatReadable,
  utcTimeToDateTime,
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


insertTask :: Connection -> Task -> IO ()
insertTask connection task = do
  let recordFields = task & getRecordFields
  execute
    connection
    ( Query $
        "INSERT INTO tasks("
          <> (recordFields & T.intercalate ", ")
          <> ") VALUES ("
          <> ((recordFields $> "?") & T.intercalate ", ")
          <> ")"
    )
    (toRow task)


insertRecord :: (ToRow r, Data r) => Connection -> Text -> r -> IO ()
insertRecord connection tableName record = do
  let recordFields = record & getRecordFields
  execute
    connection
    ( Query $
        "INSERT INTO "
          <> tableName
          <> "("
          <> (recordFields & T.intercalate ", ")
          <> ") VALUES ("
          <> ((recordFields $> "?") & T.intercalate ", ")
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


insertTags :: Connection -> Maybe DateTime -> Task -> [Text] -> IO ()
insertTags connection mbCreatedUtc task tags = do
  let uniqueTags = nub tags
  taskToTags <- forM uniqueTags $ \tag -> do
    tagUlid <- getULID
    pure $
      TaskToTag
        { ulid =
            mbCreatedUtc
              <&> setDateTime tagUlid
              & fromMaybe tagUlid
              & show
              & T.toLower
        , task_ulid = task.ulid
        , tag = tag
        }

  -- TODO: Insert all tags at once
  P.forM_ taskToTags $ \taskToTag ->
    insertRecord connection "task_to_tag" taskToTag


insertNotes :: Connection -> Maybe DateTime -> Task -> [Note] -> IO ()
insertNotes connection mbCreatedUtc task notes = do
  taskToNotes <- forM notes $ \theNote -> do
    let
      noteUlidTxt = theNote.ulid
      mbNoteUlid = parseUlidText noteUlidTxt
      mbNewUlid = do
        createdUtc <- mbCreatedUtc
        noteUlid <- mbNoteUlid

        pure $ show $ setDateTime noteUlid createdUtc

    pure $
      TaskToNote
        { ulid =
            mbNewUlid
              & fromMaybe noteUlidTxt
              & T.toLower
        , task_ulid = task.ulid
        , note = theNote.body
        }

  P.forM_ taskToNotes $ \taskToNote ->
    insertRecord connection "task_to_note" taskToNote


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
    insertRecord connection "task_to_note" taskToNote


formatElapsedP :: Config -> IO ElapsedP -> IO Text
formatElapsedP conf =
  fmap (pack . timePrint conf.utcFormat)


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
          <&> (timePrint utcFormatReadable >>> pack)
    createdUtcMb =
      metadata
        & P.filter isCreatedUtc
        <&> T.replace "created:" ""
        & P.lastMay
        >>= parseUtc
  in
    (body, tags, dueUtcMb, createdUtcMb)


getTriple :: Config -> IO (ULID, Text, [Char])
getTriple conf = do
  ulid <- getULID
  -- TODO: Set via a SQL trigger
  modified_utc <- formatElapsedP conf timeCurrentP
  effectiveUserName <- getEffectiveUserName

  pure (ulid, modified_utc, effectiveUserName)


addTask :: Config -> Connection -> [Text] -> IO (Doc AnsiStyle)
addTask conf connection bodyWords = do
  (ulid, modified_utc, effectiveUserName) <- getTriple conf
  let
    (body, tags, dueUtcMb, createdUtcMb) = parseTaskBody bodyWords
    task =
      zeroTask
        { Task.ulid = T.toLower $ show $ case createdUtcMb of
            Nothing -> ulid
            Just createdUtc -> setDateTime ulid createdUtc
        , Task.body = body
        , Task.due_utc = dueUtcMb
        , Task.modified_utc = modified_utc
        , Task.user = T.pack effectiveUserName
        }

  args <- getArgs
  preAddResult <-
    executeHooks
      ( TL.toStrict $
          TL.decodeUtf8 $
            Aeson.encode $
              object
                [ "arguments" .= args
                , "taskToAdd" .= task
                -- TODO: Add tags and notes to task
                ]
      )
      (conf.hooks & add & pre)
  putDoc preAddResult

  insertRecord connection "tasks" task
  insertTags connection Nothing task tags

  pure $
    "🆕 Added task"
      <+> dquotes (pretty task.body)
      <+> "with id"
      <+> dquotes (pretty task.ulid)


logTask :: Config -> Connection -> [Text] -> IO (Doc AnsiStyle)
logTask conf connection bodyWords = do
  (ulid, modified_utc, effectiveUserName) <- getTriple conf
  let
    (body, extractedTags, dueUtcMb, createdUtcMb) = parseTaskBody bodyWords
    tags = extractedTags <> ["log"]
    task =
      zeroTask
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

  insertTask connection task
  insertTags connection Nothing task tags
  pure $
    "📝 Logged task"
      <+> dquotes (pretty task.body)
      <+> "with id"
      <+> dquotes (pretty task.ulid)


execWithTask
  :: Config
  -> Connection
  -> Text
  -> (Task -> IO (Doc AnsiStyle))
  -> IO (Doc AnsiStyle)
execWithTask conf connection idSubstr callback = do
  tasks <-
    query
      connection
      ( Query $
          "SELECT *\n"
            <> ("FROM \"" <> conf.tableName <> "\"\n")
            <> "WHERE ulid LIKE ?\n"
      )
      ["%" <> idSubstr :: Text]
      :: IO [Task]

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
        callback $ fromMaybe zeroTask $ P.head tasks
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


setReadyUtc
  :: Config -> Connection -> DateTime -> [IdText] -> IO (Doc AnsiStyle)
setReadyUtc conf connection datetime ids = do
  let utcText = pack $ timePrint conf.utcFormat datetime

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


waitFor
  :: Config -> Connection -> Iso.Duration -> [Text] -> IO (Doc AnsiStyle)
waitFor conf connection duration ids = do
  docs <- forM ids $ \idSubstr ->
    execWithTask conf connection idSubstr $ \task -> do
      now <- timeCurrentP
      let
        nowAsText = (pack . timePrint conf.utcFormat) now
        threeDays =
          (pack . timePrint conf.utcFormat)
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


reviewTasksIn
  :: Config
  -> Connection
  -> Iso.Duration
  -> [Text]
  -> IO (Doc AnsiStyle)
reviewTasksIn conf connection duration ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      now <- timeCurrentP
      let
        xDays =
          (pack . timePrint conf.utcFormat)
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
              else getResultMsg "🔎 Finished review" task

  pure $ vsep docs


showDateTime :: Config -> DateTime -> Text
showDateTime conf =
  pack . timePrint conf.utcFormat


showEither :: Config -> Either a UTCTime -> Maybe Text
showEither conf theEither =
  theEither
    & either (const Nothing) Just
    <&> (utcTimeToDateTime >>> showDateTime conf)


-- TODO: Eliminate code duplication with createNextRecurrence
createNextRepetition
  :: Config -> Connection -> Task -> IO (Maybe (Doc ann))
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
              <&> (timePrint conf.utcFormat >>> pack)
              & fromMaybe ""
        }

  insertRecord connection "tasks" newTask

  tags <-
    query
      connection
      [sql|
        SELECT tag
        FROM task_to_tag
        WHERE task_ulid == ?
      |]
      (Only task.ulid)

  liftIO $ insertTags connection Nothing newTask (tags & P.concat)

  liftIO $
    pure $
      Just $
        "➡️  Created next task"
          <+> dquotes (pretty newTask.body)
          <+> "in repetition series"
          <+> dquotes (pretty newTask.group_ulid)
          <+> "with id"
          <+> dquotes (pretty newUlidText)


-- TODO: Eliminate code duplication with createNextRepetition
createNextRecurrence
  :: Config -> Connection -> Task -> IO (Maybe (Doc ann))
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

    nextDueMb =
      liftA2
        Iso.addDuration
        isoDurEither
        (maybeToEither "Task has no due UTC" (dueUtcMb <&> dateTimeToUtcTime))

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
              <&> (timePrint conf.utcFormat >>> pack)
              & fromMaybe ""
        }

  tags <-
    query
      connection
      [sql|
        SELECT tag
        FROM task_to_tag
        WHERE task_ulid == ?
      |]
      (Only task.ulid)

  liftIO $ insertTags connection Nothing newTask (tags & P.concat)

  liftIO $
    pure $
      Just $
        "➡️  Created next task"
          <+> dquotes (pretty task.body)
          <+> "in recurrence series"
          <+> dquotes (pretty task.group_ulid)
          <+> "with id"
          <+> dquotes (pretty newUlidText)


doTasks :: Config -> Connection -> Maybe [Text] -> [Text] -> IO (Doc AnsiStyle)
doTasks conf connection noteWordsMaybe ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      if isJust task.closed_utc
        then
          pure $
            "⚠️  Task"
              <+> dquotes (pretty task.ulid)
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
              <> "✅ Finished task"
                <+> dquotes (pretty task.body)
                <+> "with id"
                <+> dquotes (pretty task.ulid)
              <> fromMaybe "" (logMessageMaybe <&> (hardline <>))

  pure $ vsep docs


endTasks :: Config -> Connection -> [Text] -> IO (Doc AnsiStyle)
endTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      setClosedWithState connection task $ Just Obsolete
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
              <+> "is already marked as obsolete"
          else
            "⏹  Marked task"
              <+> prettyBody
              <+> "with id"
              <+> prettyId
              <+> "as obsolete"

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


repeatTasks
  :: Config -> Connection -> Iso.Duration -> [IdText] -> IO (Doc AnsiStyle)
repeatTasks conf connection duration ids = do
  let durationIsoText = decodeUtf8 $ Iso.formatDuration duration

  docs <- forM ids $ \idSubstr ->
    execWithTask conf connection idSubstr $ \task -> do
      groupUlid <- formatUlid getULID

      executeNamed
        connection
        [sql|
          UPDATE tasks
          SET
            repetition_duration = :repetition_duration,
            group_ulid = :group_ulid
          WHERE ulid == :ulid
        |]
        [ ":repetition_duration" := durationIsoText
        , ":group_ulid" := groupUlid
        , ":ulid" := task.ulid
        ]

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


recurTasks
  :: Config -> Connection -> Iso.Duration -> [IdText] -> IO (Doc AnsiStyle)
recurTasks conf connection duration ids = do
  let durationIsoText = decodeUtf8 $ Iso.formatDuration duration

  docs <- forM ids $ \idSubstr ->
    execWithTask conf connection idSubstr $ \task -> do
      groupUlid <- formatUlid getULID

      executeNamed
        connection
        [sql|
          UPDATE tasks
          SET
            recurrence_duration = :recurrence_duration,
            group_ulid = :group_ulid
          WHERE ulid == :ulid
        |]
        [ ":recurrence_duration" := durationIsoText
        , ":group_ulid" := groupUlid
        , ":ulid" := task.ulid
        ]

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


formatTaskForInfo
  :: Config
  -> DateTime
  -> (FullTask, [TaskToTag], [TaskToNote])
  -> Doc AnsiStyle
formatTaskForInfo conf now (taskV, tags, notes) =
  let
    mkGreen = annotate (color Green)
    grayOut = annotate (colorDull Black)
    stateHierarchy = getStateHierarchy now $ cpTimesAndState taskV
    mbCreatedUtc =
      fmap
        (pack . timePrint (utcFormat defaultConfig))
        (ulidTextToDateTime taskV.ulid)
    tagsPretty =
      tags
        <&> ( \t ->
                annotate (tagStyle conf) (pretty t.tag)
                  <++> maybe
                    mempty
                    (grayOut . pretty . pack . timePrint conf.utcFormat)
                    (ulidTextToDateTime t.ulid)
                  <++> grayOut (pretty t.ulid)
            )
    notesPretty =
      notes
        <&> ( \n ->
                maybe
                  mempty
                  (grayOut . pretty . pack . timePrint conf.utcFormat)
                  (ulidTextToDateTime n.ulid)
                  <++> grayOut (pretty n.ulid)
                  <> hardline
                  <> indent 2 (reflow n.note)
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
              <+> annotate (dueStyle conf) (pretty v)
              <> hardline
        )
  in
    hardline
      <> annotate bold (reflow $ FullTask.body taskV)
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
                                . pack
                                . timePrint
                                  (utcFormatShort conf)
                            )
                            (ulidTextToDateTime n.ulid)
                            <++> align (reflow n.note)
                      )
                  & vsep
              )
                <> hardline
                <> hardline
         )
      <> "   State:"
        <+> mkGreen (pretty stateHierarchy)
      <> hardline
      <> "Priority:"
        <+> annotate
          (priorityStyle conf)
          (pretty $ FullTask.priority taskV)
      <> hardline
      <> "    ULID:"
        <+> grayOut (pretty $ FullTask.ulid taskV)
      <> hardline
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
      <> "User:"
        <+> mkGreen (pretty $ FullTask.user taskV)
      <> hardline
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
              annotate underlined "Tags Detailed:"
                <> hardline
                <> hardline
                <> vsep tagsPretty
                <> hardline
                <> hardline
         )
      <> ( if P.null notes
            then mempty
            else
              annotate underlined "Notes Detailed:"
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


randomTask :: Config -> Connection -> IO (Doc AnsiStyle)
randomTask conf connection = do
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


findTask :: Connection -> Text -> IO (Doc AnsiStyle)
findTask connection aPattern = do
  tasks :: [(Text, Text, Maybe [Text], Maybe [Text], Maybe Text)] <-
    query_ connection $
      [sql|
        SELECT ulid, body, tags, notes, metadata
        FROM tasks_view
      |]

  let
    ulidWidth = 5
    numOfResults = 8
    minimumScore = 4
    ulidColor = Green
    preTag = "\x1b[4m\x1b[34m" -- Set color to blue and underline text
    postTag = "\x1b[0m" -- Reset styling
    metaNorm metadata =
      metadata
        & fromMaybe ""
        & T.replace ",\"" ", \""
        & T.replace "\":" "\": "
        & T.replace "\"" ""
    matchFunc =
      Huzzy.match
        Huzzy.IgnoreCase
        (preTag, postTag)
        identity
        aPattern

    -- Calculate fuzzy score for each part individually
    -- and pick the highest one
    scoreFunc (ulid, theBody, _, mbNotes, mbMetadata) =
      let
        scoreParts =
          [ matchFunc theBody
          , matchFunc (maybe "" unwords mbNotes)
          , -- TODO: Find good way to include tags
            -- , matchFunc (maybe "" unwords mbTags)
            matchFunc (metaNorm mbMetadata)
          , matchFunc ulid
          ]
        highestScore = P.maximum $ 0 : (catMaybes scoreParts <&> Huzzy.score)
        combinedText =
          scoreParts
            & catMaybes
            <&> (Huzzy.rendered >>> reflow)
            & P.intersperse mempty
            & vcat
      in
        (highestScore, ulid, combinedText)

    fstOf3 (x, _, _) = x
    tasksScored =
      tasks
        <&> scoreFunc
        & P.filter ((> minimumScore) . fstOf3)
        & sortOn (Down . fstOf3)
    moreResults = P.length tasksScored - numOfResults
    header =
      annotate (underlined <> color ulidColor) (fill ulidWidth "ULID")
        <++> annotate underlined (fill 20 "Task")
        <> hardline
    body =
      tasksScored
        & P.take numOfResults
        <&> ( \(_, ulid, combinedText) ->
                annotate
                  (color ulidColor)
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
              (color Red)
              ("There are " <> pretty moreResults <> " more results available")
            <> hardline
        else hardline

  pure $ header <> body <> footer


-- TODO: Use Continuation monad to avoid callback hell
-- withConnectCont :: Text -> ContT a IO Connection
-- withConnectCont dbPath =
--     ContT $ withConnection dbPath

addTag :: Config -> Connection -> Text -> [IdText] -> IO (Doc AnsiStyle)
addTag conf connection tag ids = do
  docs <- forM ids $ \idSubstr ->
    execWithTask conf connection idSubstr $ \task -> do
      now <- fmap (pack . timePrint conf.utcFormat) timeCurrentP
      ulid <- formatUlid getULID

      insertRecord
        connection
        "task_to_tag"
        TaskToTag{ulid, task_ulid = task.ulid, tag}

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
        "🏷  Added tag"
          <+> dquotes (pretty tag)
          <+> "to task"
          <+> prettyBody
          <+> "with id"
          <+> prettyId

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

      pure $ getResultMsg ("💥 Removed tag \"" <> pretty tag <> "\"") task

  pure $ vsep docs


addNote :: Config -> Connection -> Text -> [IdText] -> IO (Doc AnsiStyle)
addNote conf connection noteBody ids = do
  docs <- forM ids $ \idSubstr ->
    execWithTask conf connection idSubstr $ \task -> do
      now <- timeCurrentP <&> (timePrint conf.utcFormat >>> pack)
      ulid <- formatUlid getULID

      insertRecord
        connection
        "task_to_note"
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


setDueUtc :: Config -> Connection -> DateTime -> [IdText] -> IO (Doc AnsiStyle)
setDueUtc conf connection datetime ids = do
  let
    utcText :: Text
    utcText = pack $ timePrint conf.utcFormat datetime

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


getResultMsg :: Doc AnsiStyle -> Task -> Doc AnsiStyle
getResultMsg msg task = do
  let
    prettyBody = dquotes $ pretty task.body
    prettyId = dquotes $ pretty task.ulid

  msg <+> "of task" <+> prettyBody <+> "with id" <+> prettyId


uncloseTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
uncloseTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      executeNamed
        connection
        [sql|
          UPDATE tasks
          SET closed_utc = NULL
          WHERE ulid == :ulid
        |]
        [":ulid" := task.ulid]

      pure $ getResultMsg "💥 Removed close timestamp and state field" task

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
          WHERE ulid == :ulid
        |]
        [":ulid" := task.ulid]

      pure $ getResultMsg "💥 Removed due timestamp" task

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
          WHERE ulid == :ulid
        |]
        [":ulid" := task.ulid]

      pure $ getResultMsg "💥 Removed waiting and review timestamps" task

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
          WHERE ulid == :ulid
        |]
        [":ulid" := task.ulid]

      pure $ getResultMsg "💥 Removed awake timestamp" task

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
          WHERE ulid == :ulid
        |]
        [":ulid" := task.ulid]

      pure $ getResultMsg "💥 Removed ready timestamp" task

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
          WHERE ulid == :ulid
        |]
        [":ulid" := task.ulid]

      pure $ getResultMsg "💥 Removed review timestamp" task

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
          WHERE ulid == :ulid
        |]
        [":ulid" := task.ulid]

      pure $ getResultMsg "💥 Removed repetition duration" task

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
          WHERE ulid == :ulid
        |]
        [":ulid" := task.ulid]

      pure $ getResultMsg "💥 Removed recurrence duration" task

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

      pure $ getResultMsg "💥 Removed all tags" task

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

      pure $ getResultMsg "💥 Removed all notes" task

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
          WHERE ulid == :ulid
        |]
        [":ulid" := task.ulid]

      pure $ getResultMsg "💥 Removed priority adjustment" task

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
          WHERE ulid == :ulid
        |]
        [":ulid" := task.ulid]

      pure $ getResultMsg "💥 Removed metadata" task

  pure $ vsep docs


duplicateTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
duplicateTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      dupeUlid <- formatUlid getULID
      -- TODO: Check if modified_utc can be set via an SQL trigger
      modified_utc <- formatElapsedP conf timeCurrentP

      let dupeTask =
            task
              { Task.ulid = dupeUlid
              , Task.due_utc = Nothing
              , Task.awake_utc = Nothing
              , Task.closed_utc = Nothing
              , Task.modified_utc = modified_utc
              , Task.state = Nothing
              }

      insertRecord connection "tasks" dupeTask

      tags <-
        query
          connection
          [sql|
            SELECT tag
            FROM task_to_tag
            WHERE task_ulid == ?
          |]
          (Only task.ulid)

      liftIO $ insertTags connection Nothing dupeTask (tags & P.concat)

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
        if numOfChanges == 0
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
    tuple = breakOn "." (show number)
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
  annotate (tagStyle conf)
    . (annotate (color Black) "+" <>)
    . pretty


formatTaskLine :: Config -> DateTime -> Int -> FullTask -> Doc AnsiStyle
formatTaskLine conf now taskWidth task =
  let
    id = pretty $ T.takeEnd taskWidth task.ulid
    createdUtc =
      fmap
        (pack . timePrint ISO8601_Date)
        (ulidTextToDateTime task.ulid)
    tags = fromMaybe [] task.tags
    closedUtcMaybe =
      task.closed_utc
        >>= parseUtc
          <&> timePrint conf.utcFormat
    dueUtcMaybe =
      task.due_utc
        >>= parseUtc
          <&> T.replace " 00:00:00" ""
          . T.pack
          . timePrint conf.utcFormat
    dueIn offset =
      let dateMaybe = task.due_utc >>= parseUtc
      in  isJust dateMaybe && dateMaybe < Just (now `timeAdd` offset)
    multilineIndent = 2
    hangWidth =
      taskWidth
        + 2
        + dateWidth conf
        + 2
        + prioWidth conf
        + 2
        + multilineIndent
    hhsep = concatWith (<++>)
    isEmptyDoc doc = show doc /= ("" :: Text)
    isOpen = isNothing task.closed_utc
    grayOutIfDone doc =
      if isOpen
        then annotate (bodyStyle conf) doc
        else annotate (bodyClosedStyle conf) doc
    -- redOut onTime doc = if onTime
    --   then annotate (bodyStyle conf) doc
    --   else annotate (color Red) doc
    taskLine =
      createdUtc <&> \taskDate ->
        hang hangWidth $
          hhsep $
            P.filter
              isEmptyDoc
              [ annotate conf.idStyle id
              , annotate
                  (priorityStyle conf)
                  ( pretty $
                      justifyRight 4 ' ' $
                        showAtPrecision 1 $
                          realToFrac $
                            fromMaybe 0 task.priority
                  )
              , annotate (dateStyle conf) (pretty taskDate)
              , pretty
                  ( case task.review_utc >>= parseUtc of
                      Nothing -> "" :: Text
                      Just date_ -> if date_ < now then "🔎 " else ""
                  )
                  <> ( if dueIn mempty{durationHours = 24} && isOpen
                        then "⚠️️  "
                        else ""
                     )
                  <> ( if dueIn mempty && isOpen
                        then annotate (color Red) (reflow task.body)
                        else grayOutIfDone (reflow task.body)
                     )
              , annotate (dueStyle conf) (pretty dueUtcMaybe)
              , annotate (closedStyle conf) (pretty closedUtcMaybe)
              , hsep (tags <&> formatTag conf)
              , if not $ P.null task.notes
                  then "📝"
                  else ""
              ]
  in
    fromMaybe
      ( "Id"
          <+> dquotes (pretty task.ulid)
          <+> "is an invalid ulid and could not be converted to a datetime"
      )
      taskLine


getIdLength :: Float -> Int
getIdLength numOfItems =
  -- TODO: Calculate idLength by total number of tasks, not just of the viewed
  let
    targetCollisionChance = 0.01 -- Targeted likelihood of id collisions
    sizeOfAlphabet = 32 -- Crockford's base 32 alphabet
  in
    ceiling (logBase sizeOfAlphabet (numOfItems / targetCollisionChance)) + 1


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
        query
          connection
          [sql|
            SELECT count(1)
            FROM ?
          |]
          (Only $ tableName conf)

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
                vsep (fmap (annotate (color Red) . ppInvalidFilter) errors)
                  <> hardline
                  <> hardline
            else Nothing

      -- TODO: Increase performance of this query
      tasks <- query_ connection (getFilterQuery filterExps)

      pure $ fromMaybe (pretty $ P.length (tasks :: [FullTask])) errorsDoc


-- TODO: Print number of remaining tasks and how to display them at the bottom
headTasks :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
headTasks conf now connection = do
  tasks <-
    query
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
        LIMIT ?
      |]
      (Only $ headCount conf)

  formatTasksColor conf now tasks


newTasks :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
newTasks conf now connection = do
  tasks <-
    query
      connection
      [sql|
        SELECT *
        FROM tasks_view
        WHERE closed_utc IS NULL
        ORDER BY ulid DESC
        LIMIT ?
      |]
      (Only $ headCount conf)

  formatTasksColor conf now tasks


listOldTasks :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
listOldTasks conf now connection = do
  tasks <-
    query
      connection
      [sql|
        SELECT *
        FROM tasks_view
        WHERE closed_utc IS NULL
        ORDER BY ulid ASC
        LIMIT ?
      |]
      (Only $ headCount conf)

  formatTasksColor conf now tasks


openTasks :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
openTasks conf now connection = do
  tasks <-
    query_
      connection
      [sql|
        SELECT *
        FROM tasks_view
        WHERE closed_utc IS NULL
        ORDER BY priority DESC, due_utc ASC, ulid DESC
      |]
  formatTasksColor conf now tasks


modifiedTasks
  :: Config
  -> DateTime
  -> Connection
  -> ListModifiedFlag
  -> IO (Doc AnsiStyle)
modifiedTasks conf now connection listModifiedFlag = do
  tasks <-
    query_
      connection
      [sql|
        SELECT *
        FROM tasks_view
        ORDER BY modified_utc DESC
      |]

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

  formatTasksColor conf now filteredTasks


overdueTasks :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
overdueTasks conf now connection = do
  tasks <-
    query_
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
          ulid DESC
      |]

  formatTasksColor conf now tasks


doneTasks :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
doneTasks conf now connection = do
  tasks <-
    query
      connection
      [sql|
        SELECT *
        FROM tasks_view
        WHERE
          closed_utc IS NOT NULL AND
          state == 'Done'
        ORDER BY closed_utc DESC
        LIMIT ?
      |]
      (Only $ headCount conf)

  formatTasksColor conf now tasks


obsoleteTasks :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
obsoleteTasks conf now connection = do
  tasks <-
    query
      connection
      [sql|
        SELECT *
        FROM tasks_view
        WHERE
          closed_utc IS NOT NULL AND
          state == 'Obsolete'
        ORDER BY ulid DESC
        LIMIT ?
      |]
      (Only $ headCount conf)

  formatTasksColor conf now tasks


deletableTasks :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
deletableTasks conf now connection = do
  tasks <-
    query
      connection
      [sql|
        SELECT *
        FROM tasks_view
        WHERE
          closed_utc IS NOT NULL
          AND state == 'Deletable'
        ORDER BY ulid DESC
        LIMIT ?
      |]
      (Only $ headCount conf)
  formatTasksColor conf now tasks


listRepeating :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
listRepeating conf now connection = do
  tasks <-
    query_
      connection
      [sql|
        SELECT *
        FROM tasks_view
        WHERE repetition_duration IS NOT NULL
        ORDER BY repetition_duration DESC
      |]

  formatTasksColor conf now tasks


listRecurring :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
listRecurring conf now connection = do
  tasks <-
    query_
      connection
      [sql|
        SELECT *
        FROM tasks_view
        WHERE recurrence_duration IS NOT NULL
        ORDER BY recurrence_duration DESC
      |]

  formatTasksColor conf now tasks


listReady :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
listReady conf now connection = do
  tasks <-
    query
      connection
      [sql|
        SELECT *
        FROM tasks_view
        WHERE
          (ready_utc IS NULL OR
            (ready_utc IS NOT NULL AND ready_utc < datetime('now'))
          ) AND
          closed_utc IS NULL
        ORDER BY
          priority DESC,
          due_utc ASC,
          ulid DESC
        LIMIT ?
      |]
      (Only $ headCount conf)

  formatTasksColor conf now tasks


listWaiting :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
listWaiting conf now connection = do
  tasks <-
    query_
      connection
      [sql|
        SELECT *
        FROM tasks_view
        WHERE closed_utc IS NULL
          AND waiting_utc IS NOT NULL
          AND (review_utc > datetime('now') OR review_utc IS NULL)
        ORDER BY waiting_utc DESC
      |]

  formatTasksColor conf now tasks


listAll :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
listAll conf now connection = do
  tasks <-
    query_
      connection
      [sql|
        SELECT *
        FROM tasks_view
        ORDER BY ulid ASC
      |]

  formatTasksColor conf now tasks


listNoTag :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
listNoTag conf now connection = do
  tasks <-
    query_
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
      |]

  formatTasksColor conf now tasks


getWithTag :: Connection -> Maybe DerivedState -> [Text] -> IO [FullTask]
getWithTag connection stateMaybe tags = do
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
           \  ulid DESC\n"

  query_ connection mainQuery


listWithTag :: Config -> DateTime -> Connection -> [Text] -> IO (Doc AnsiStyle)
listWithTag conf now connection tags = do
  tasks <- getWithTag connection Nothing tags
  formatTasksColor conf now tasks


queryTasks :: Config -> DateTime -> Connection -> Text -> IO (Doc AnsiStyle)
queryTasks conf now connection sqlQuery = do
  tasks <-
    query_ connection $
      Query $
        "SELECT * FROM tasks_view WHERE " <> sqlQuery
  formatTasksColor conf now tasks


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
  pure $ HasTag $ pack aTag


notTagParser :: ReadP FilterExp
notTagParser = do
  _ <- char '-'
  aTag <- munch (not . isSpace)
  pure $ NotTag $ pack aTag


dueParser :: ReadP FilterExp
dueParser = do
  _ <- string "due:"
  utcStr <- munch (not . isSpace)
  pure $ HasDue $ pack utcStr


stateParser :: ReadP FilterExp
stateParser = do
  _ <- string "state:"
  stateStr <- munch (not . isSpace)
  pure $ HasStatus $ textToDerivedState $ pack stateStr


filterExpParser :: ReadP FilterExp
filterExpParser =
  do
    tagParser
    <++ notTagParser
    <++ dueParser
    <++ stateParser
    <++ (InvalidFilter . pack <$> munch1 (not . isSpace))


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
  HasTag tag -> ("intersect", "tag like '" <> tag <> "'")
  NotTag tag -> ("except", "tag like '" <> tag <> "'")
  HasDue utc -> ("intersect", "due_utc < datetime('" <> utc <> "')")
  HasStatus (Just taskState) -> ("intersect", derivedStateToQuery taskState)
  -- Following cases should never be called, as they are filtered out
  HasStatus Nothing -> ("", "")
  InvalidFilter _ -> ("", "")


isValidFilter :: FilterExp -> Bool
isValidFilter = \case
  InvalidFilter _ -> False
  HasStatus Nothing -> False
  _ -> True


runFilter :: Config -> DateTime -> Connection -> [Text] -> IO (Doc AnsiStyle)
runFilter conf now connection exps = do
  let
    parserResults = readP_to_S filterExpsParser $ T.unpack $ unwords exps
    filterHelp =
      "Filter expressions must be a list of key[:value] entries."
        <> hardline
    dieWithError err = do
      hPutDoc stderr $ annotate (color Red) err
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
        isStateExp = \case (HasStatus _) -> True; _ -> False
        updatedFilterExps =
          if P.any isStateExp filterExps
            then filterExps
            else HasStatus (Just IsOpen) : filterExps
        sqlQuery = getFilterQuery updatedFilterExps

      tasks <- query_ connection sqlQuery

      if P.length errors > 0
        then dieWithError $ vsep (fmap ppInvalidFilter errors)
        else formatTasksColor conf now tasks
    _ -> dieWithError filterHelp


-- TODO: Increase performance of this query
getFilterQuery :: [FilterExp] -> Query
getFilterQuery filterExps = do
  let
    filterTuple = filterToSql <$> P.filter isValidFilter filterExps

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

  FullTask.selectQuery
    <> "FROM ("
    <> Query ulidsQuery
    <> ") tasks1\n\
       \LEFT JOIN tasks_view ON tasks1.ulid IS tasks_view.ulid\n\
       \ORDER BY \n\
       \  priority DESC,\n\
       \  due_utc ASC,\n\
       \  ulid DESC\n"


formatTasks :: Config -> DateTime -> [FullTask] -> Doc AnsiStyle
formatTasks conf now tasks =
  if P.length tasks == 0
    then pretty noTasksWarning
    else
      let
        strong = bold <> underlined
        taskWidth = getIdLength $ fromIntegral $ P.length tasks
        docHeader =
          annotate
            (idStyle conf <> strong)
            (fill taskWidth "Id")
            <++> annotate
              (priorityStyle conf <> strong)
              (fill (prioWidth conf) "Prio")
            <++> annotate
              (dateStyle conf <> strong)
              (fill (dateWidth conf) "Opened UTC")
            <++> annotate
              (bodyStyle conf <> strong)
              (fill (bodyWidth conf) "Body")
            <++> line
      in
        docHeader
          <> vsep (fmap (formatTaskLine conf now taskWidth) tasks)
          <> line


formatTasksColor :: Config -> DateTime -> [FullTask] -> IO (Doc AnsiStyle)
formatTasksColor conf now tasks = do
  confNorm <- applyColorMode conf
  pure $ formatTasks confNorm now tasks


getProgressBar :: Integer -> Double -> Doc AnsiStyle
getProgressBar maxWidthInChars progress =
  let
    barWidth = floor (progress * fromInteger maxWidthInChars)
    remainingWidth = fromIntegral $ maxWidthInChars - barWidth
  in
    annotate
      (bgColorDull Green <> colorDull Green)
      (pretty $ P.take (fromIntegral barWidth) $ P.repeat '#')
      <>
      -- (annotate (bgColorDull Green) $ fill (fromIntegral barWidth) "" <>
      annotate (bgColorDull Black) (fill remainingWidth "")


formatTagLine
  :: Config -> Int -> (Text, Integer, Integer, Double) -> Doc AnsiStyle
formatTagLine conf maxTagLength (tag, open_count, closed_count, progress) =
  let
    barWidth = toInteger $ progressBarWidth conf
    progressPercentage =
      if progress == 0
        then "     "
        else
          pretty
            ( justifyRight 3 ' ' $
                T.pack $
                  showFFloat (Just 0) (progress * 100) ""
            )
            <+> "%"
  in
    fill maxTagLength (pretty tag)
      <++> pretty (justifyRight (T.length "open") ' ' $ show open_count)
      <++> pretty (justifyRight (T.length "closed") ' ' $ show closed_count)
      <++> progressPercentage
      <+> getProgressBar barWidth progress


formatTags :: Config -> [(Text, Integer, Integer, Double)] -> Doc AnsiStyle
formatTags conf tagTuples =
  let
    percWidth = 6 -- Width of e.g. 100 %
    progressWith = conf.progressBarWidth + percWidth
    firstOf4 (a, _, _, _) = a
    maxTagLength =
      tagTuples
        <&> (T.length . firstOf4)
        & P.maximum
  in
    annotate (bold <> underlined) (fill maxTagLength "Tag")
      <++> annotate (bold <> underlined) "Open"
      <++> annotate (bold <> underlined) "Closed"
      <++> annotate (bold <> underlined) (fill progressWith "Progress")
      <> line
      <> vsep (fmap (formatTagLine conf maxTagLength) tagTuples)


listTags :: Config -> Connection -> IO (Doc AnsiStyle)
listTags conf connection = do
  tags <- query_ connection $ Query "SELECT * FROM tags"

  pure $ formatTags conf tags


listProjects :: Config -> Connection -> IO (Doc AnsiStyle)
listProjects conf connection = do
  tags <-
    query_ connection $
      [sql|
        SELECT *
        FROM tags
        WHERE
          "open" > 0 AND
          closed > 0
      |]

  pure $ formatTags conf tags


getStats :: Config -> Connection -> IO (Doc AnsiStyle)
getStats _ connection = do
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

  let
    widthKey = 12
    widthValue = max 5 $ fromIntegral $ numDigits 10 numOfTasksTotal
    formatLine (name :: Text) (numTasks :: Integer) =
      let
        numTotalInt :: Double = fromIntegral numOfTasksTotal
        numTasksInt :: Double = fromIntegral numTasks
        share = T.pack $ showFFloat (Just 3) (numTasksInt / numTotalInt) ""
      in
        fill widthKey (pretty name)
          <++> fill
            widthValue
            (pretty $ justifyRight widthValue ' ' $ show numTasks)
          <++> pretty share

  pure $
    annotate (bold <> underlined) (fill widthKey "State")
      <++> annotate (bold <> underlined) (fill widthValue "Value")
      <++> annotate (bold <> underlined) "Share"
      <> line
      <> vsep
        [ formatLine "Any" numOfTasksTotal
        , formatLine "Open" numOfTasksOpen
        , formatLine "Closed" numOfTasksClosed
        , formatLine "└─ Done" numOfTasksDone
        , formatLine "└─ Obsolete" numOfTasksObsolete
        , formatLine "└─ Deletable" numOfTasksDeletable
        ]
