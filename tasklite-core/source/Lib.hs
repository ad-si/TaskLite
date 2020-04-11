{-|
Functions to create, update, and delete tasks / tags / notes
-}

module Lib where

import Protolude as P

import Data.Hourglass
import Data.Text as T
import qualified Data.Time.ISO8601.Duration as Iso8601
import Data.ULID
import Data.Coerce
import Data.Yaml as Yaml
import Database.Beam hiding (char)
import Database.Beam.Sqlite
import Database.Beam.Schema.Tables
import Database.SQLite.Simple as Sql
import Numeric
import System.Directory
import System.FilePath ((</>))
import System.IO as SIO
import System.Process (readProcess)
import System.Posix.User (getEffectiveUserName)
import qualified Text.Huzzy as Huzzy
import Text.ParserCombinators.ReadP as ReadP
import GHC.Unicode (isSpace)
import Time.System
import Text.Read (readMaybe)
import Data.Text.Prettyprint.Doc as Pp hiding ((<>))
import Data.Text.Prettyprint.Doc.Util
import Data.Text.Prettyprint.Doc.Render.Terminal
import Unsafe (unsafeHead)

import Utils
import Task
import TaskView
import FullTask
import Note
import TaskToNote
import TaskToTag
import Config


noTasksWarning :: Text
noTasksWarning = "No tasks available. "
  <> "Run `tasklite help` to learn how to create tasks."


newtype NumRows = NumRows Integer
  deriving (Eq, Ord, Read, Show)

instance FromRow NumRows where
  fromRow = NumRows <$> field


data TaskLiteDb f = TaskLiteDb
  { _tldbTasks :: f (TableEntity TaskT)
  , _tldbTaskToTag :: f (TableEntity TaskToTagT)
  , _tldbTaskToNote :: f (TableEntity TaskToNoteT)
  , _tldbTasksView :: f (ViewEntity TaskViewT)
  } deriving Generic

instance Database be TaskLiteDb


taskLiteDb :: DatabaseSettings be TaskLiteDb
taskLiteDb = defaultDbSettings `withDbModification`
  dbModification
    { _tldbTaskToTag = modifyTable identity $
        tableModification
          { TaskToTag.task_ulid = TaskUlid (fieldNamed "task_ulid") }
    , _tldbTaskToNote = modifyTable identity $
        tableModification
          { TaskToNote.task_ulid = TaskUlid (fieldNamed "task_ulid") }
    }


getDbPath :: Config -> IO FilePath
getDbPath conf = do
  pure $ (dataDir conf) </> (dbName conf)


setupConnection :: Config -> IO Connection
setupConnection conf = do
  createDirectoryIfMissing True $ dataDir conf
  open $ (dataDir conf) </> (dbName conf)


execWithConn :: Config -> (Connection -> IO a) -> IO a
execWithConn conf func = do
  createDirectoryIfMissing True $ dataDir conf
  withConnection
    ((dataDir conf) </> (dbName conf))
    func


-- | For use with `runBeamSqliteDebug`
writeToLog :: Config -> [Char] -> IO ()
writeToLog conf message = do
  let logFile = (dataDir conf) </> "log.sql"
  -- Use System.IO so it doesn't have to be converted to Text first
  SIO.appendFile logFile $ message <> "\n"


insertTask :: Connection -> Task -> IO ()
insertTask connection task = do
  runBeamSqlite connection $ runInsert $
    insert (_tldbTasks taskLiteDb) $
    insertValues [task]


replaceTask :: Connection -> Task -> IO ()
replaceTask connection task = do
  runBeamSqlite connection $ runUpdate $
    save (_tldbTasks taskLiteDb) task


insertTags :: Connection -> Maybe DateTime -> TaskUlid -> [Text] -> IO ()
insertTags connection mbCreatedUtc taskUlid tags = do
  taskToTags <- forM tags $ \tag -> do
    tagUlid <- getULID
    pure $ TaskToTag
      (mbCreatedUtc
          <&> setDateTime tagUlid
          & fromMaybe tagUlid
          & show
          & toLower)
      taskUlid
      tag

  runBeamSqlite connection $ runInsert $
    insert (_tldbTaskToTag taskLiteDb) $
    insertValues taskToTags


insertNotes :: Connection -> Maybe DateTime -> TaskUlid -> [Note] -> IO ()
insertNotes connection mbCreatedUtc primKey notes = do
  taskToNotes <- forM notes $ \theNote -> do
    let
      noteUlidTxt = Note.ulid theNote
      mbNoteUlid = parseUlidText $ noteUlidTxt
      mbNewUlid = do
        createdUtc <- mbCreatedUtc
        noteUlid <- mbNoteUlid

        pure $ show $ setDateTime noteUlid createdUtc

    pure $ TaskToNote
      (toLower $ fromMaybe noteUlidTxt mbNewUlid)
      primKey
      (Note.body theNote)

  runBeamSqlite connection $ runInsert $
    insert (_tldbTaskToNote taskLiteDb) $
    insertValues taskToNotes


-- | Tuple is (Maybe createdUtc, noteBody)
insertNoteTuples :: Connection -> TaskUlid -> [(Maybe DateTime, Text)] -> IO ()
insertNoteTuples connection taskUlid notes = do
  taskToNotes <- forM notes $ \(mbCreatedUtc, noteBody) -> do
    noteUlid <- getULID
    pure $ TaskToNote
      (mbCreatedUtc
          <&> setDateTime noteUlid
          & fromMaybe noteUlid
          & show
          & toLower)
      taskUlid
      noteBody

  runBeamSqlite connection $ runInsert $
    insert (_tldbTaskToNote taskLiteDb) $
    insertValues taskToNotes


formatElapsedP :: Config -> IO ElapsedP -> IO Text
formatElapsedP conf =
  fmap (pack . timePrint (utcFormat conf))


formatUlid :: IO ULID -> IO Text
formatUlid =
  fmap (toLower . show)


-- | Parses the body of the tasks and extracts all meta data
-- | Returns a tuple (body, tags, due_utc)
-- TODO: Replace with parsec implementation
parseTaskBody :: Config -> [Text] -> (Text, [Text], Maybe Text)
parseTaskBody conf bodyWords =
  let
    isTag = ("+" `T.isPrefixOf`)
    isDueUtc = ("due:" `T.isPrefixOf`)
    isMeta word = isTag word || isDueUtc word
    -- Handle case when word is actually a text
    bodyWordsReversed = bodyWords & T.unwords & T.words & P.reverse
    body = bodyWordsReversed
      & P.dropWhile isMeta
      & P.reverse
      & unwords
    metadata = bodyWordsReversed
      & P.takeWhile isMeta
      & P.reverse
    tags = fmap (T.replace "+" "") (P.filter isTag metadata)
    dueUtc = metadata
      & P.filter isDueUtc
      <&> T.replace "due:" ""
      & P.lastMay
      >>= parseUtc
      <&> pack . timePrint (utcFormat conf)
  in
    (body, tags, dueUtc)


getTriple :: Config -> IO (Text, Text, [Char])
getTriple conf = do
  ulid <- formatUlid getULID
  modified_utc <- formatElapsedP conf timeCurrentP -- TODO: Set via a SQL trigger
  effectiveUserName <- getEffectiveUserName

  pure (ulid, modified_utc, effectiveUserName)


addTask :: Config -> Connection -> [Text] -> IO (Doc AnsiStyle)
addTask conf connection bodyWords = do
  (ulid, modified_utc, effectiveUserName) <- getTriple conf
  let
    (body, tags, due_utc) = parseTaskBody conf bodyWords
    task = zeroTask
      { Task.ulid = ulid
      , Task.body = body
      , Task.due_utc = due_utc
      , Task.modified_utc = modified_utc
      , Task.user = T.pack effectiveUserName
      }

  insertTask connection task
  insertTags connection Nothing (primaryKey task) tags
  pure $
    "🆕 Added task" <+> dquotes (pretty $ Task.body task)
    <+> "with id" <+> dquotes (pretty $ Task.ulid task)


logTask :: Config -> Connection -> [Text] -> IO (Doc AnsiStyle)
logTask conf connection bodyWords = do
  (ulid, modified_utc, effectiveUserName) <- getTriple conf
  let
    (body, extractedTags, due_utc) = parseTaskBody conf bodyWords
    tags = extractedTags <> ["log"]
    task = zeroTask
      { Task.ulid = ulid
      , Task.body = body
      , Task.state = Just Done
      , Task.due_utc = due_utc
      , Task.closed_utc = Just modified_utc
      , Task.user = T.pack effectiveUserName
      , Task.modified_utc = modified_utc
      }

  insertTask connection task
  insertTags connection Nothing (primaryKey task) tags
  pure $
    "📝 Logged task" <+> dquotes (pretty $ Task.body task)
    <+> "with id" <+> dquotes (pretty $ Task.ulid task)


execWithTask ::
  Config -> Connection -> Text
  -> (Task -> IO (Doc AnsiStyle)) -> IO (Doc AnsiStyle)
execWithTask conf connection idSubstr callback = do
  tasks <- (query connection
      (Query $ "select * from " <> tableName conf <> " where `ulid` like ?")
      ["%"  <> idSubstr :: Text]
    ) :: IO [Task]

  let
    numOfTasks = P.length tasks
    ulidLength = 26
    prefix = if (T.length idSubstr) == ulidLength then "" else "…"
    quote = dquotes . pretty

  if
    | numOfTasks == 0 -> pure $
        "⚠️  Task" <+> quote (prefix <> idSubstr) <+> "does not exist"
    | numOfTasks == 1 ->
        callback $ unsafeHead tasks
    | numOfTasks > 1 -> pure $
        "⚠️  Id slice" <+> (quote idSubstr) <+> "is not unique."
        <+> "It could refer to one of the following tasks:"
        <++> (P.foldMap
            (\task ->
              (annotate (idStyle conf) (pretty $ Task.ulid task))
              <++> (pretty $ Task.body task) <> hardline)
            tasks
          )
    | otherwise -> pure "This case should not be possible"


setStateAndClosed :: Connection -> TaskUlid -> Maybe TaskState -> IO ()
setStateAndClosed connection taskUlid theTaskState = do
  runBeamSqlite connection $ runUpdate $
    update (_tldbTasks taskLiteDb)
      (\task -> mconcat
                [ (Task.state task) <-. val_ theTaskState
                , (Task.review_utc task) <-. val_ Nothing
                -- closed_utc is set via an SQL trigger
                ])
      (\task -> primaryKey task ==. val_ taskUlid &&.
                (Task.state task) /=. val_ theTaskState)


setReadyUtc ::
  Config -> Connection -> DateTime -> [IdText] -> IO (Doc AnsiStyle)
setReadyUtc conf connection datetime ids = do
  let
    utcText :: Text
    utcText = pack $ timePrint (utcFormat conf) datetime

  docs <- forM ids $ \idSubstr ->
    execWithTask conf connection idSubstr $ \task -> do
      let
        taskUlid@(TaskUlid idText) = primaryKey task
        prettyBody = dquotes (pretty $ Task.body task)
        prettyId = dquotes (pretty idText)

      runBeamSqlite connection $ runUpdate $
        update (_tldbTasks taskLiteDb)
          (\task_ -> mconcat [(Task.ready_utc task_) <-. val_ (Just utcText)])
          (\task_ -> primaryKey task_ ==. val_ taskUlid)

        -- TODO: Update modified_utc via SQL trigger

      pure $ "📅 Set ready UTC of task" <+> prettyBody
            <+> "with id" <+> prettyId
            <+> "to" <+> dquotes (pretty utcText)

  pure $ vsep docs


waitFor :: Config -> Connection -> Duration -> [Text] -> IO (Doc AnsiStyle)
waitFor conf connection duration ids = do
  docs <- forM ids $ \idSubstr ->
    execWithTask conf connection idSubstr $ \task -> do
      now <- timeCurrentP
      let
        taskUlid@(TaskUlid idText) = primaryKey task
        nowAsText = (pack . timePrint (utcFormat conf)) now
        threeDays = (pack . timePrint (utcFormat conf)) (now `timeAdd` duration)
        prettyBody = dquotes (pretty $ Task.body task)
        prettyId = dquotes (pretty idText)

      runBeamSqlite connection $ runUpdate $
        update (_tldbTasks taskLiteDb)
          (\theTask ->
            mconcat [ (Task.waiting_utc theTask) <-. val_ (Just nowAsText)
            , (Task.review_utc theTask) <-. val_ (Just threeDays)
            ]
          )
          (\theTask -> primaryKey theTask ==. val_ taskUlid)

      numOfChanges <- changes connection

      pure $ if numOfChanges == 0
        then "⚠️  An error occurred while moving task"
              <+> prettyBody <> "with id" <+> prettyId <+> "into waiting mode"
        else "⏳  Set waiting UTC and review UTC for task"
              <+> prettyBody <+> "with id" <+> prettyId

  pure $ vsep docs


waitTasks :: Config -> Connection -> [Text] -> IO (Doc AnsiStyle)
waitTasks conf connection =
  waitFor conf connection $ mempty { durationHours = 72 }


reviewTasksIn :: Config -> Connection
  -> Duration -> [Text] -> IO (Doc AnsiStyle)
reviewTasksIn conf connection days ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      now <- timeCurrentP
      let
        taskUlid@(TaskUlid idText) = primaryKey task
        xDays = (pack . timePrint (utcFormat conf)) (now `timeAdd` days)
        prettyBody = dquotes (pretty $ Task.body task)
        prettyId = dquotes (pretty idText)
        warningStart = "⚠️  Task" <+> prettyBody <+> "with id" <+> prettyId

      if Task.closed_utc task /= Nothing
      then pure $ warningStart <+> "is already closed"
      else do
        runBeamSqlite connection $ runUpdate $
          update (_tldbTasks taskLiteDb)
            (\theTask -> (Task.review_utc theTask) <-. val_ (Just xDays))
            (\theTask -> primaryKey theTask ==. val_ taskUlid)

        numOfChanges <- changes connection

        pure $
          if numOfChanges == 0
          then warningStart <+> "could not be reviewed"
          else getResultMsg "🔎 Finished review" task

  pure $ vsep docs


-- TODO: Replace with proper IS08601 duration parser
parseIsoDuration :: Text -> Maybe Duration
parseIsoDuration isoDuration =
  if "PT" `T.isPrefixOf` isoDuration && "M" `T.isSuffixOf` isoDuration
  then
    let
      minutes :: Maybe Int64
      minutes = readMaybe $ unpack $ (T.drop 2 . T.dropEnd 1) isoDuration
    in
      fmap
        (\mins -> mempty { durationMinutes = Minutes mins })
        minutes
  else Nothing


createNextRepetition
  :: Config -> Connection -> Task -> IO (Maybe (Doc ann))
createNextRepetition conf connection task = do
  newUlidText <- formatUlid getULID
  let
    taskUlid = primaryKey task
    nowMaybe = ulidTextToDateTime newUlidText
    dueUtcMb = (Task.due_utc task) >>= parseUtc
    showDateTime = pack . timePrint (utcFormat conf)
    repIsoDur = (Task.repetition_duration task) >>= parseIsoDuration
    nextDueMb = liftA2 timeAdd
      (if nowMaybe < dueUtcMb then dueUtcMb else nowMaybe)
      repIsoDur

  -- TODO: Investigate why this isn't working and replace afterwards
  -- runBeamSqlite connection $ runInsert $
  --   insert (_tldbTasks taskLiteDb) $
  --   insertValues [ task
  --     { Task.ulid = val_ newUlidText
  --     , Task.due_utc = nowMaybe + (Task.repetition_duration task)
  --     }
  --   ]

  runBeamSqlite connection $ do
    runInsert $ insert (_tldbTasks taskLiteDb) $ insertFrom $ do
      originalTask <- filter_
        (\theTask -> primaryKey theTask ==. val_ taskUlid)
        (all_ $ _tldbTasks taskLiteDb)

      pure originalTask
        { Task.ulid = val_ newUlidText
        , Task.due_utc = val_ $ fmap showDateTime nextDueMb
        , Task.awake_utc = val_ $
            fmap showDateTime $ liftA2 timeAdd
              ((Task.awake_utc task) >>= parseUtc)
              repIsoDur
        , Task.ready_utc = val_ $
            fmap showDateTime $ liftA2 timeAdd
              ((Task.ready_utc task) >>= parseUtc)
              repIsoDur
        }

    -- Duplicate tags
    tags <- runSelectReturningList $ select $
      filter_ (\tag -> TaskToTag.task_ulid tag ==. val_ taskUlid) $
      all_ (_tldbTaskToTag taskLiteDb)

    liftIO $ insertTags
      connection
      Nothing
      (TaskUlid newUlidText)
      (fmap TaskToTag.tag tags)

  liftIO $ pure $ Just $ "➡️  Created next task"
    <+> dquotes (pretty $ Task.body task)
    <+> "in repetition series" <+> dquotes (pretty $ Task.group_ulid task)
    <+> "with id" <+> dquotes (pretty newUlidText)


createNextRecurrence
  :: Config -> Connection -> Task -> IO (Maybe (Doc ann))
createNextRecurrence conf connection task = do
  newUlidText <- formatUlid getULID
  let
    taskUlid = primaryKey task
    dueUtcMb = (Task.due_utc task) >>= parseUtc

    showDateTime :: DateTime -> Text
    showDateTime = pack . timePrint (utcFormat conf)

    durTextEither = maybeToEither
                        "Task has no recurrence duration"
                        (Task.recurrence_duration task)
    isoDurEither =
      durTextEither
      <&> encodeUtf8
      >>= Iso8601.parseDuration

    showEither e = e
      & (either (const Nothing) Just)
      <&> utcTimeToDateTime
      <&> showDateTime

    nextDueMb = liftA2 Iso8601.addDuration isoDurEither
      (maybeToEither "Task has no due UTC" (dueUtcMb <&> dateTimeToUtcTime))

  runBeamSqlite connection $ do
    runInsert $ insert (_tldbTasks taskLiteDb) $ insertFrom $ do
      originalTask <- filter_
        (\theTask -> primaryKey theTask ==. val_ taskUlid)
        (all_ $ _tldbTasks taskLiteDb)

      pure originalTask
        { Task.ulid = val_ newUlidText
        , Task.due_utc = val_ $ nextDueMb & showEither

        , Task.awake_utc = val_ $
            (liftA2 Iso8601.addDuration isoDurEither
              (maybeToEither "Task has no awake UTC"
                ((Task.awake_utc task) >>= parseUtc <&> dateTimeToUtcTime)))
            & showEither

        , Task.ready_utc = val_ $
            (liftA2 Iso8601.addDuration isoDurEither
              (maybeToEither "Task has no ready UTC"
                ((Task.ready_utc task) >>= parseUtc <&> dateTimeToUtcTime)))
            & showEither

        }

    -- Duplicate tags
    tags <- runSelectReturningList $ select $
      filter_ (\tag -> TaskToTag.task_ulid tag ==. val_ taskUlid) $
      all_ (_tldbTaskToTag taskLiteDb)

    liftIO $ insertTags
      connection
      Nothing
      (TaskUlid newUlidText)
      (fmap TaskToTag.tag tags)

  liftIO $ pure $ Just $ "➡️  Created next task"
    <+> dquotes (pretty $ Task.body task)
    <+> "in recurrence series" <+> dquotes (pretty $ Task.group_ulid task)
    <+> "with id" <+> dquotes (pretty newUlidText)


doTasks :: Config -> Connection -> Maybe [Text] -> [Text] -> IO (Doc AnsiStyle)
doTasks conf connection noteWordsMaybe ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      let
        taskUlid@(TaskUlid idText) = primaryKey task

      if Task.closed_utc task /= Nothing
      then pure $ "⚠️  Task" <+> dquotes (pretty idText) <+> "is already done"
      else do
        logMessageMaybe <-
          if Task.repetition_duration task /= Nothing
          then createNextRepetition conf connection task
          else
            if Task.recurrence_duration task /= Nothing
            then createNextRecurrence conf connection task
            else pure Nothing

        noteMessageMaybe <- case noteWordsMaybe of
          Nothing -> pure Nothing
          Just noteWords -> liftIO $
            addNote conf connection (unwords noteWords) ids
            >>= pure . Just

        setStateAndClosed connection taskUlid $ Just Done

        pure $
          (fromMaybe "" $ noteMessageMaybe <&> (<> hardline))
          <> "✅ Finished task" <+> dquotes (pretty $ Task.body task)
          <+> "with id" <+> dquotes (pretty idText)
          <> (fromMaybe "" $ logMessageMaybe <&> (hardline <>))

  pure $ vsep docs


endTasks :: Config -> Connection -> [Text] -> IO (Doc AnsiStyle)
endTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      let
        taskUlid@(TaskUlid idText) = primaryKey task
        prettyBody = dquotes (pretty $ Task.body task)
        prettyId = dquotes (pretty idText)

      setStateAndClosed connection taskUlid $ Just Obsolete

      numOfChanges <- changes connection

      pure $ if numOfChanges == 0
        then "⚠️  Task" <+> prettyBody <+> "with id" <+> prettyId
              <+> "is already marked as obsolete"
        else "⏹  Marked task" <+> prettyBody <+> "with id" <+> prettyId
              <+> "as obsolete"

  pure $ vsep docs


trashTasks :: Config -> Connection -> [Text] -> IO (Doc AnsiStyle)
trashTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      let
        taskUlid@(TaskUlid idText) = primaryKey task
        prettyBody = dquotes (pretty $ Task.body task)
        prettyId = dquotes (pretty idText)

      setStateAndClosed connection taskUlid $ Just Deletable

      numOfChanges <- changes connection

      pure $ if numOfChanges == 0
        then "⚠️  Task" <+> prettyBody <+> "with id" <+> prettyId
              <+> "is already marked as deletable"
        else "🗑  Marked task" <+> prettyBody <+> "with id" <+> prettyId
              <+> "as deletable"

  pure $ vsep docs


deleteTasks :: Config -> Connection -> [Text] -> IO (Doc AnsiStyle)
deleteTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      let
        taskUlid@(TaskUlid idText) = primaryKey task
        prettyBody = dquotes (pretty $ Task.body task)
        prettyId = dquotes (pretty idText)

      runBeamSqlite connection $ do
        runDelete $ delete
          (_tldbTasks taskLiteDb)
          (\theTask -> primaryKey theTask ==. val_ taskUlid)

        runDelete $ delete
          (_tldbTaskToTag taskLiteDb)
          (\tag -> TaskToTag.task_ulid tag ==. val_ taskUlid)

        runDelete $ delete
          (_tldbTaskToNote taskLiteDb)
          (\noteValue -> TaskToNote.task_ulid noteValue ==. val_ taskUlid)

        pure $ "❌ Deleted task" <+> prettyBody <+> "with id" <+> prettyId

  pure $ vsep docs


durationToIso :: Duration -> Text
durationToIso dur =
  "PT" <> (show $ (coerce (durationMinutes dur) :: Int64)) <> "M"


repeatTasks ::
  Config -> Connection -> Duration -> [IdText] -> IO (Doc AnsiStyle)
repeatTasks conf connection duration ids = do
  let durIso = durationToIso duration

  docs <- forM ids $ \idSubstr ->
    execWithTask conf connection idSubstr $ \task -> do
      let
        taskUlid@(TaskUlid idText) = primaryKey task
        prettyBody = dquotes (pretty $ Task.body task)
        prettyId = dquotes (pretty idText)

      groupUlid <- formatUlid getULID

      runBeamSqlite connection $ runUpdate $
        update (_tldbTasks taskLiteDb)
          (\task_ -> mconcat
            [ (Task.repetition_duration task_) <-. val_ (Just durIso)
            , (Task.group_ulid task_) <-. val_ (Just groupUlid)
            ])
          (\task_ -> primaryKey task_ ==. val_ taskUlid)

      pure $ "📅 Set repeat duration of task" <+> prettyBody
        <+> "with id" <+> prettyId
        <+> "to" <+> dquotes (pretty $ durIso)

  pure $ vsep docs


recurTasks ::
  Config -> Connection -> Iso8601.Duration -> [IdText] -> IO (Doc AnsiStyle)
recurTasks conf connection duration ids = do
  docs <- forM ids $ \idSubstr ->
    execWithTask conf connection idSubstr $ \task -> do
      let
        taskUlid@(TaskUlid idText) = primaryKey task
        prettyBody = dquotes (pretty $ Task.body task)
        prettyId = dquotes (pretty idText)
        durationIsoText = decodeUtf8 $ Iso8601.formatDuration duration

      groupUlid <- formatUlid getULID

      runBeamSqlite connection $ runUpdate $
        update (_tldbTasks taskLiteDb)
          (\task_ -> mconcat
            [ (Task.recurrence_duration task_) <-. val_ (Just durationIsoText)
            , (Task.group_ulid task_) <-. val_ (Just groupUlid)
            ])
          (\task_ -> primaryKey task_ ==. val_ taskUlid)

      pure $ "📅 Set recurrence duration of task" <+> prettyBody
        <+> "with id" <+> prettyId
        <+> "to" <+> dquotes (pretty $ durationIsoText)

  pure $ vsep docs


adjustPriority :: Config -> Float -> [IdText] -> IO (Doc AnsiStyle)
adjustPriority conf adjustment ids  = do
  dbPath <- getDbPath conf
  withConnection dbPath $ \connection -> do
    docs <- forM ids $ \idSubstr -> do
      execWithTask conf connection idSubstr $ \task -> do
        let
          (TaskUlid idText) = primaryKey task
          prettyBody = dquotes (pretty $ Task.body task)
          prettyId = dquotes (pretty idText)

        -- TODO: Figure out why this doesn't work
        -- runBeamSqlite connection $ runUpdate $
        --   update (_tldbTasks taskLiteDb)
        --     (\task -> [(Task.priority_adjustment task) <-.
        --       fmap (+ adjustment) (current_ (Task.priority_adjustment task))
        --     ])
        --     (\task -> primaryKey task ==. val_ taskUlid)

        execute connection
          (Query "update `tasks` \
            \set `priority_adjustment` = ifnull(`priority_adjustment`, 0) + ? \
            \where `ulid` == ?")
          (adjustment, idText :: Text)

        numOfChanges <- changes connection

        pure $ if numOfChanges == 0
          then "⚠️ An error occurred while adjusting the priority of task"
                <+> prettyBody
          else (if adjustment > 0 then "⬆️  Increased" else "⬇️  Decreased")
                <+> "priority of task" <+> prettyBody <+> "with id"
                <+> prettyId <+> "by" <+> (pretty $ abs adjustment)

    pure $ vsep docs


startTasks :: Config -> Connection -> [Text] -> IO (Doc AnsiStyle)
startTasks conf connection ids = do
  logMessage <- addNote conf connection "start" ids

  pure $ pretty $ T.replace
    "📝  Added a note to"
    "⏳ Started"
    (show logMessage)


stopTasks :: Config -> Connection -> [Text] -> IO (Doc AnsiStyle)
stopTasks conf connection ids = do
  logMessages <- addNote conf connection "stop" ids

  pure $ pretty $ T.replace
    "📝  Added a note to"
    "⌛️ Stopped"
    (show logMessages)


formatTaskForInfo
  :: Config
  -> DateTime
  -> (TaskView, [TaskToTag], [TaskToNote])
  -> Doc AnsiStyle
formatTaskForInfo conf now (taskV, tags, notes) =
  let
    mkGreen = annotate (color Green)
    grayOut = annotate (colorDull Black)
    stateHierarchy = getStateHierarchy now $ cpTimesAndState taskV
    mbCreatedUtc = fmap
      (pack . (timePrint $ utcFormat defaultConfig))
      (ulidTextToDateTime $ TaskView.ulid taskV)
    tagsPretty = tags
      <&> (\t -> (annotate (tagStyle conf) (pretty $ TaskToTag.tag t))
                  <++> (fromMaybe mempty $ fmap
                        (grayOut . pretty . pack . timePrint (utcFormat conf))
                        (ulidTextToDateTime $ TaskToTag.ulid t))
                  <++> (grayOut $ pretty $ TaskToTag.ulid t)
          )
    notesPretty = notes
      <&> (\n -> (fromMaybe mempty $ fmap
                    (grayOut . pretty . pack . timePrint (utcFormat conf))
                    (ulidTextToDateTime $ TaskToNote.ulid n))
                  <++> (grayOut $ pretty $ TaskToNote.ulid n) <> hardline
                  <> (indent 2 $ reflow $ TaskToNote.note n)
                  <> hardline
          )

    mbAwakeUtc = TaskView.awake_utc taskV
    mbReadyUtc = TaskView.ready_utc taskV
    mbWaitingUtc = TaskView.waiting_utc taskV
    mbReviewUtc = TaskView.review_utc taskV
    mbDueUtc = TaskView.due_utc taskV
    mbClosedUtc = TaskView.closed_utc taskV
    mbModifiedUtc = Just $ TaskView.modified_utc taskV

    printIf :: Doc AnsiStyle -> Maybe Text -> Maybe (Doc AnsiStyle)
    printIf name value = fmap
      (\v -> name <+> (annotate (dueStyle conf) $ pretty v) <> hardline)
      value
  in
       hardline
    <> annotate bold (reflow $ TaskView.body taskV) <> hardline
    <> hardline
    <> (if P.null tags
        then mempty
        else (hsep $ (tags <&> TaskToTag.tag) <$$> (formatTag conf)) <> hardline
              <> hardline
        )

    <> (if P.null notes
        then mempty
        else (notes
              <&> (\n -> (fromMaybe mempty $ fmap
                    (grayOut . pretty . pack . timePrint (utcFormatShort conf))
                    (ulidTextToDateTime $ TaskToNote.ulid n))
                    <++> (align $ reflow $ TaskToNote.note n)
                  )
              & vsep)
              <> hardline
              <> hardline
        )

    <> "   State:" <+> mkGreen (pretty stateHierarchy) <> hardline
    <> "Priority:" <+> annotate (priorityStyle conf)
          (pretty $ TaskView.priority taskV) <> hardline
    <> "    ULID:" <+> grayOut (pretty $ TaskView.ulid taskV)
        <> hardline

    <> hardline

    <> ((
          (printIf "🆕  Created  ", mbCreatedUtc) :
          (printIf "☀️   Awake   ", mbAwakeUtc) :
          (printIf "📅   Ready   ", mbReadyUtc) :
          (printIf "⏳  Waiting  ", mbWaitingUtc) :
          (printIf "🔎  Review   ", mbReviewUtc) :
          (printIf "📅    Due    ", mbDueUtc) :
          (printIf "✅   Done    ", mbClosedUtc) :
          (printIf "✏️   Modified ", mbModifiedUtc) :
        [])
        & sortBy (compare `on` snd)
        <&> (\tup -> (fst tup) (snd tup))
        & catMaybes
        & punctuate (pretty ("       ⬇" :: Text))
        & vsep
      )

    <> hardline

    <> (fromMaybe mempty $ (fmap
          (\value -> "Repetition Duration:" <+> (mkGreen $ pretty value)
              <> hardline)
          (TaskView.repetition_duration taskV)
        ))

    <> (fromMaybe mempty $ (fmap
          (\value -> "Recurrence Duration:" <+> (mkGreen $ pretty value)
              <> hardline)
          (TaskView.recurrence_duration taskV)
        ))

    <> (fromMaybe mempty $ (fmap
          (\value -> "Group Ulid:"
              <+> (grayOut $ pretty value)
              <> hardline)
          (TaskView.group_ulid taskV)
        ))

    <> "User:" <+> (mkGreen $ pretty $ TaskView.user taskV) <> hardline

    <> hardline

    <> (fromMaybe mempty $ (fmap
          (\value -> "Metadata:" <> hardline
              <> indent 2 (pretty $ decodeUtf8 $ Yaml.encode value)
              <> hardline
          )
          (TaskView.metadata taskV)
        ))

    <> (if P.null tags
        then mempty
        else (annotate underlined "Tags Detailed:") <> hardline
              <> hardline
              <> vsep tagsPretty <> hardline
              <> hardline
        )

    <> (if P.null notes
        then mempty
        else (annotate underlined "Notes Detailed:") <> hardline
              <> hardline
              <> vsep notesPretty <> hardline
        )


infoTask :: Config -> Connection -> Text -> IO (Doc AnsiStyle)
infoTask conf connection idSubstr = do
  execWithTask conf connection idSubstr $ \task -> do
    let
      taskUlid@(TaskUlid idText) = primaryKey task

    now <- dateCurrent

    runBeamSqlite connection $ do
      (mbFullTask :: Maybe TaskView) <- runSelectReturningOne $ select $
        filter_ (\tsk -> TaskView.ulid tsk ==. val_ idText) $
        allFromView_ (_tldbTasksView taskLiteDb)

      tags <- runSelectReturningList $ select $
        filter_ (\tag -> TaskToTag.task_ulid tag ==. val_ taskUlid) $
        all_ (_tldbTaskToTag taskLiteDb)

      notes <- runSelectReturningList $ select $
        filter_ (\theNote -> TaskToNote.task_ulid theNote ==. val_ taskUlid) $
        all_ (_tldbTaskToNote taskLiteDb)

      pure $ case mbFullTask of
        Nothing -> pretty noTasksWarning
        Just fullTask -> formatTaskForInfo conf now (fullTask, tags, notes)


nextTask :: Config -> Connection -> IO (Doc AnsiStyle)
nextTask conf connection = do
  now <- dateCurrent

  runBeamSqlite connection $ do
    (mbFullTask :: Maybe TaskView) <- runSelectReturningOne $ select $
      limit_ 1  $
      orderBy_ (desc_ . TaskView.priority) $
      filter_ (\tsk -> TaskView.closed_utc tsk ==. val_ Nothing) $
      allFromView_ (_tldbTasksView taskLiteDb)

    case mbFullTask of
      Nothing -> pure $ pretty noTasksWarning
      Just fullTask -> do
        tags <- runSelectReturningList $ select $
          filter_ (\tag -> TaskToTag.task_ulid tag ==.
            (val_ $ TaskUlid $ TaskView.ulid fullTask)) $
          all_ (_tldbTaskToTag taskLiteDb)

        notes <- runSelectReturningList $ select $
          filter_ (\theNote -> TaskToNote.task_ulid theNote ==.
            (val_ $ TaskUlid $ TaskView.ulid fullTask)) $
          all_ (_tldbTaskToNote taskLiteDb)

        pure $ formatTaskForInfo conf now (fullTask, tags, notes)


findTask :: Connection -> Text -> IO (Doc AnsiStyle)
findTask connection aPattern = do
  tasks :: [(Text, Text, Maybe [Text], Maybe [Text], Maybe Text)]
    <- query_ connection $ Query
      "select ulid, body, tags, notes, metadata from tasks_view"

  let
    ulidWidth = 5
    numOfResults = 8
    minimumScore = 4
    ulidColor = Green
    preTag = "\x1b[4m\x1b[34m"  -- ^ Set color to blue and underline text
    postTag = "\x1b[0m"  -- ^ Reset styling
    metaNorm metadata = metadata
      & fromMaybe ""
      & T.replace ",\"" ", \""
      & T.replace "\":" "\": "
      & T.replace "\"" ""
    matchFunc = Huzzy.match
      Huzzy.IgnoreCase
      (preTag, postTag)
      identity
      aPattern

    -- | Calculate fuzzy score for each part individually
    -- and pick the highest one
    scoreFunc = \(ulid, theBody, _, mbNotes, mbMetadata) ->
      let
        scoreParts =
          [ matchFunc theBody
          , matchFunc (maybe "" unwords mbNotes)
          -- TODO: Find good way to include tags
          -- , matchFunc (maybe "" unwords mbTags)
          , matchFunc (metaNorm mbMetadata)
          , matchFunc ulid
          ]
        highestScore = P.maximum $ 0 : (catMaybes scoreParts <&> Huzzy.score)
        combinedText = vcat $ P.intersperse mempty $ (catMaybes scoreParts)
          <&> Huzzy.rendered
          <&> reflow
      in
        (highestScore, ulid, combinedText)

    fstOf3 (x, _, _) = x
    tasksScored = tasks
      <&> scoreFunc
      & P.filter ((> minimumScore) . fstOf3)
      & sortOn (Down . fstOf3)
    moreResults = (P.length tasksScored) - numOfResults
    header =
      annotate (underlined <> color ulidColor) (fill ulidWidth "ULID") <++>
      annotate (underlined) (fill 20 "Task") <>
      hardline
    body =
      tasksScored
        & P.take numOfResults
        <&> (\(_, ulid, combinedText) ->
              annotate (color ulidColor)
                (fill ulidWidth $ pretty $ T.takeEnd ulidWidth ulid)
              <> (indent 2 combinedText))
        & P.intersperse mempty
        & vsep
    footer =
      if moreResults > 0
      then hardline
        <> hardline
        <> annotate (color Red)
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
      let
        taskUlid@(TaskUlid idText) = primaryKey task
        prettyBody = dquotes (pretty $ Task.body task)
        prettyId = dquotes (pretty idText)

      now <- fmap (pack . timePrint (utcFormat conf)) timeCurrentP
      ulid <- formatUlid getULID

      let taskToTag = TaskToTag ulid taskUlid tag

      runBeamSqlite connection $ runInsert $
        insert (_tldbTaskToTag taskLiteDb) $
        insertValues [taskToTag]

      runBeamSqlite connection $ runUpdate $
        update (_tldbTasks taskLiteDb)
          (\task_ -> mconcat [(Task.modified_utc task_) <-. val_ now])
          (\task_ -> primaryKey task_ ==. val_ taskUlid)

      pure $ "🏷  Added tag" <+> dquotes (pretty tag)
            <+> "to task" <+> prettyBody <+> "with id" <+> prettyId

  pure $ vsep docs


addNote :: Config -> Connection -> Text -> [IdText] -> IO (Doc AnsiStyle)
addNote conf connection noteBody ids = do
  docs <- forM ids $ \idSubstr ->
    execWithTask conf connection idSubstr $ \task -> do
      let
        taskUlid@(TaskUlid idText) = primaryKey task
        prettyBody = dquotes (pretty $ Task.body task)
        prettyId = dquotes (pretty idText)

      now <- fmap (pack . timePrint (utcFormat conf)) timeCurrentP
      ulid <- formatUlid getULID

      let taskToNote = TaskToNote ulid taskUlid noteBody

      runBeamSqlite connection $ runInsert $
        insert (_tldbTaskToNote taskLiteDb) $
        insertValues [taskToNote]

      runBeamSqlite connection $ runUpdate $
        update (_tldbTasks taskLiteDb)
          (\task_ -> mconcat [(Task.modified_utc task_) <-. val_ now])
          (\task_ -> primaryKey task_ ==. val_ taskUlid)

      pure $ "🗒  Added a note to task" <+> prettyBody
            <+> "with id" <+> prettyId

  pure $ vsep docs


setDueUtc :: Config -> Connection -> DateTime -> [IdText] -> IO (Doc AnsiStyle)
setDueUtc conf connection datetime ids = do
  let
    utcText :: Text
    utcText = pack $ timePrint (utcFormat conf) datetime

  docs <- forM ids $ \idSubstr ->
    execWithTask conf connection idSubstr $ \task -> do
      let
        taskUlid@(TaskUlid idText) = primaryKey task
        prettyBody = dquotes (pretty $ Task.body task)
        prettyId = dquotes (pretty idText)

      runBeamSqlite connection $ runUpdate $
        update (_tldbTasks taskLiteDb)
          (\task_ -> mconcat [(Task.due_utc task_) <-. val_ (Just utcText)])
          (\task_ -> primaryKey task_ ==. val_ taskUlid)

        -- TODO: Update modified_utc via SQL trigger

      pure $ "📅 Set due UTC of task" <+> prettyBody <+> "with id" <+> prettyId
            <+> "to" <+> dquotes (pretty utcText)

  pure $ vsep docs


getResultMsg :: Doc AnsiStyle -> Task -> Doc AnsiStyle
getResultMsg msg task =
  let
    TaskUlid idText = primaryKey task
    prettyBody = dquotes (pretty $ Task.body task)
    prettyId = dquotes (pretty idText)
  in
    msg <+> "of task" <+> prettyBody <+> "with id" <+> prettyId


uncloseTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
uncloseTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      runBeamSqlite connection $ runUpdate $
        update (_tldbTasks taskLiteDb)
          (\task_ -> mconcat
              [ (Task.closed_utc task_) <-. (val_ Nothing)
              , (Task.state task_) <-. (val_ Nothing)
              ]
          )
          (\task_ -> primaryKey task_ ==. val_ (primaryKey task))

      pure $ getResultMsg "💥 Removed close timestamp and state field" task

  pure $ vsep docs


undueTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
undueTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      runBeamSqlite connection $ runUpdate $
        update (_tldbTasks taskLiteDb)
          (\task_ -> mconcat [(Task.due_utc task_) <-. (val_ Nothing)])
          (\task_ -> primaryKey task_ ==. val_ (primaryKey task))

      pure $ getResultMsg "💥 Removed due timestamp" task

  pure $ vsep docs


unwaitTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
unwaitTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      runBeamSqlite connection $ runUpdate $
        update (_tldbTasks taskLiteDb)
          (\task_ -> mconcat
            [ (Task.waiting_utc task_) <-. (val_ Nothing)
            , (Task.review_utc task_) <-. (val_ Nothing)
            ]
          )
          (\task_ -> primaryKey task_ ==. val_ (primaryKey task))

      pure $ getResultMsg "💥 Removed waiting and review timestamps" task

  pure $ vsep docs


unwakeTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
unwakeTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      runBeamSqlite connection $ runUpdate $
        update (_tldbTasks taskLiteDb)
          (\task_ -> mconcat [(Task.awake_utc task_) <-. (val_ Nothing)])
          (\task_ -> primaryKey task_ ==. val_ (primaryKey task))

      pure $ getResultMsg "💥 Removed awake timestamp" task

  pure $ vsep docs


unreadyTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
unreadyTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      runBeamSqlite connection $ runUpdate $
        update (_tldbTasks taskLiteDb)
          (\task_ -> mconcat [(Task.ready_utc task_) <-. (val_ Nothing)])
          (\task_ -> primaryKey task_ ==. val_ (primaryKey task))

      pure $ getResultMsg "💥 Removed ready timestamp" task

  pure $ vsep docs


unrepeatTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
unrepeatTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      runBeamSqlite connection $ runUpdate $
        update (_tldbTasks taskLiteDb)
          (\task_ -> mconcat
              [(Task.repetition_duration task_) <-. (val_ Nothing)])
          (\task_ -> primaryKey task_ ==. val_ (primaryKey task))

      pure $ getResultMsg "💥 Removed repetition duration" task

  pure $ vsep docs


unrecurTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
unrecurTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      runBeamSqlite connection $ runUpdate $
        update (_tldbTasks taskLiteDb)
          (\task_ -> mconcat
              [(Task.recurrence_duration task_) <-. (val_ Nothing)])
          (\task_ -> primaryKey task_ ==. val_ (primaryKey task))

      pure $ getResultMsg "💥 Removed recurrence duration" task

  pure $ vsep docs


untagTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
untagTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      runBeamSqlite connection $ runDelete $ delete
        (_tldbTaskToTag taskLiteDb)
        (\tag -> TaskToTag.task_ulid tag ==. val_ (primaryKey task))

      pure $ getResultMsg "💥 Removed all tags" task

  pure $ vsep docs


unnoteTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
unnoteTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      runBeamSqlite connection $ runDelete $ delete
        (_tldbTaskToNote taskLiteDb)
        (\noteValue ->
            TaskToNote.task_ulid noteValue ==. val_ (primaryKey task)
        )

      pure $ getResultMsg "💥 Removed all notes" task

  pure $ vsep docs


unprioTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
unprioTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      runBeamSqlite connection $ runUpdate $
        update (_tldbTasks taskLiteDb)
          (\task_ -> mconcat
              [(Task.priority_adjustment task_) <-. (val_ Nothing)])
          (\task_ -> primaryKey task_ ==. val_ (primaryKey task))

      pure $ getResultMsg "💥 Removed priority adjustment" task

  pure $ vsep docs


unmetaTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
unmetaTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      runBeamSqlite connection $ runUpdate $
        update (_tldbTasks taskLiteDb)
          (\task_ -> mconcat [(Task.metadata task_) <-. (val_ Nothing)])
          (\task_ -> primaryKey task_ ==. val_ (primaryKey task))

      pure $ getResultMsg "💥 Removed metadata" task

  pure $ vsep docs


duplicateTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
duplicateTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      let
        taskUlid@(TaskUlid idText) = primaryKey task
        prettyBody = dquotes (pretty $ Task.body task)
        prettyId = dquotes (pretty idText)

      dupeUlid <- formatUlid getULID
      -- TODO: Check if modified_utc can be set via an SQL trigger
      modified_utc <- formatElapsedP conf timeCurrentP

      -- Duplicate task
      runBeamSqlite connection $ do
        runInsert $ insert (_tldbTasks taskLiteDb) $ insertFrom $ do
          -- TODO: Remove as original task is already in scope
          originalTask <- filter_
            (\task_ -> primaryKey task_ ==. val_ taskUlid)
            (all_ $ _tldbTasks taskLiteDb)

          pure originalTask
            { Task.ulid = val_ dupeUlid
            , Task.due_utc = val_ Nothing
            , Task.awake_utc = val_ Nothing
            , Task.closed_utc = val_ Nothing
            , Task.modified_utc = val_ modified_utc
            }

        -- Duplicate tags
        tags <- runSelectReturningList $ select $
          filter_ (\tag -> TaskToTag.task_ulid tag ==. val_ taskUlid) $
          all_ (_tldbTaskToTag taskLiteDb)

        liftIO $ insertTags
          connection
          Nothing
          (TaskUlid dupeUlid)
          (fmap TaskToTag.tag tags)

        -- Duplicate notes
        notes <- runSelectReturningList $ select $
          filter_ (\theNote -> TaskToNote.task_ulid theNote ==. val_ taskUlid) $
          all_ (_tldbTaskToNote taskLiteDb)

        let
          noteTuples = fmap
            (\theNote ->
              ( ulidTextToDateTime (TaskToNote.ulid theNote)
              , TaskToNote.note theNote)
              )
            notes
        liftIO $ insertNoteTuples
          connection
          (TaskUlid dupeUlid)
          noteTuples


      numOfChanges <- changes connection

      pure $ if numOfChanges == 0
        then "⚠️  Task" <+> prettyBody <+> "with id" <+> prettyId
              <+> "could not be duplicated"
        else "👯  Created a duplicate of task" <+> prettyBody
              <+> "(id:" <+> (pretty idText) <+> ")"
              <+> "with id" <+> (pretty dupeUlid)

  pure $ vsep docs


showAtPrecision :: Int -> Double -> Text
showAtPrecision numOfDigits number =
  let
    tuple = breakOn "." (show number)
    clipDecimalPart = if snd tuple == ".0"
      then T.replace ".0" (T.replicate (1 + numOfDigits) " ")
      else T.take (1 + numOfDigits)
  in
    fst tuple <> if numOfDigits /= 0
      then (clipDecimalPart . snd) tuple
      else ""


formatTag :: Pretty a => Config -> a -> Doc AnsiStyle
formatTag conf =
  annotate (tagStyle conf)
  . (annotate (color Black) "+" <>)
  . pretty


formatTaskLine :: Config -> DateTime -> Int -> FullTask -> Doc AnsiStyle
formatTaskLine conf now taskUlidWidth task =
  let
    id = pretty $ T.takeEnd taskUlidWidth $ FullTask.ulid task
    createdUtc = fmap
      (pack . timePrint ISO8601_Date)
      (ulidTextToDateTime $ FullTask.ulid task)
    body = FullTask.body task
    tags = fromMaybe [] $ FullTask.tags task
    closedUtcMaybe = (FullTask.closed_utc task)
      >>= parseUtc
      <&> timePrint (utcFormat conf)
    dueUtcMaybe = (FullTask.due_utc task)
      >>= parseUtc
      <&> T.replace " 00:00:00" "" . T.pack . timePrint (utcFormat conf)
    dueIn offset =
      let dateMaybe = (FullTask.due_utc task) >>= parseUtc
      in isJust dateMaybe && dateMaybe < Just (now `timeAdd` offset)
    multilineIndent = 2
    hangWidth = taskUlidWidth + 2
      + (dateWidth conf) + 2
      + (prioWidth conf) + 2
      + multilineIndent
    hhsep = concatWith (<++>)
    isEmptyDoc doc = (show doc) /= ("" :: Text)
    isOpen = isNothing $ FullTask.closed_utc task
    grayOutIfDone doc = if isOpen
      then annotate (bodyStyle conf) doc
      else annotate (bodyClosedStyle conf) doc
    -- redOut onTime doc = if onTime
    --   then annotate (bodyStyle conf) doc
    --   else annotate (color Red) doc
    taskLine = createdUtc <$$> \taskDate ->
      hang hangWidth $ hhsep $ P.filter isEmptyDoc (
        annotate (idStyle conf) id :
        annotate (priorityStyle conf) (pretty $ justifyRight 4 ' '
          $ showAtPrecision 1 $ realToFrac
          $ fromMaybe 0 (FullTask.priority task)) :
        annotate (dateStyle conf) (pretty taskDate) :
        (pretty $ case FullTask.review_utc task >>= parseUtc of
          Nothing -> "" :: Text
          Just date_ -> if date_ < now then "🔎" else "") :
        (if dueIn mempty { durationHours = 24 } && isOpen
          then "⚠️️  "
          else "") <>
          (if dueIn mempty && isOpen
            then annotate (color Red) (reflow body)
            else grayOutIfDone (reflow body)) :
        annotate (dueStyle conf) (pretty dueUtcMaybe) :
        annotate (closedStyle conf) (pretty closedUtcMaybe) :
        hsep (tags <$$> (formatTag conf)) :
        (if (not $ P.null $ FullTask.notes task) then "📝" else "") :
        [])
  in
    fromMaybe
      ("Id" <+> dquotes (pretty $ FullTask.ulid task) <+>
        "is an invalid ulid and could not be converted to a datetime")
      taskLine


getIdLength :: Float -> Int
getIdLength numOfItems =
  -- TODO: Calculate idLength by total number of tasks, not just of the viewed
  let
    targetCollisionChance = 0.01  -- Targeted likelihood of id collisions
    sizeOfAlphabet = 32  -- Crockford's base 32 alphabet
  in
    ceiling (logBase sizeOfAlphabet (numOfItems / targetCollisionChance)) + 1


countTasks :: Config -> Connection -> Maybe [Text] -> IO (Doc AnsiStyle)
countTasks conf connection filterExpression = do
  let
    parserResults = readP_to_S filterExpsParser $
      T.unpack (unwords $ fromMaybe [""] filterExpression)
    filterMay = listToMaybe parserResults

  case filterMay of
    Nothing -> do
      [NumRows taskCount] <- query_ connection $ Query $
        "select count(1) from `" <> tableName conf <> "`"

      pure $ pretty taskCount

    Just (filterExps, _) -> do
      let
        ppInvalidFilter = \case
          (InvalidFilter error) ->
              (dquotes $ pretty error) <+> "is an invalid filter"
          (HasStatus Nothing) -> "Filter contains an invalid state value"
          _ -> "The functions should not be called with a valid function"
        errors = P.filter (not . isValidFilter) filterExps
        errorsDoc = if (P.length errors) > 0
          then Just $
            vsep (fmap (annotate (color Red) . ppInvalidFilter) errors)
            <> hardline <> hardline
          else Nothing

      -- TODO: Increase performance of this query
      tasks <- query_ connection (getFilterQuery filterExps)

      pure $ fromMaybe (pretty $ P.length (tasks :: [FullTask])) errorsDoc


-- TODO: Print number of remaining tasks and how to display them at the bottom
headTasks :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
headTasks conf now connection = do
  tasks <- query_ connection $ Query $
    -- TODO: Add `wait_utc` < datetime('now')"
    "select * from tasks_view \
    \where closed_utc is null \
    \order by `priority` desc \
    \limit " <> show (headCount conf)
  pure $ formatTasks conf now tasks


newTasks :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
newTasks conf now connection = do
  tasks <- query_ connection $ Query $
    "select * from `tasks_view` \
    \where closed_utc is null \
    \order by `ulid` desc limit " <> show (headCount conf)
  pure $ formatTasks conf now tasks


listOldTasks :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
listOldTasks conf now connection = do
  tasks <- query_ connection $ Query $
    "select * from `tasks_view` \
    \where closed_utc is null \
    \order by `ulid` asc limit " <> show (headCount conf)
  pure $ formatTasks conf now tasks


openTasks :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
openTasks conf now connection = do
  tasks <- query_ connection $ Query
    "select * from `tasks_view` \
    \where closed_utc is null \
    \order by `priority` desc"
  pure $ formatTasks conf now tasks


overdueTasks :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
overdueTasks conf now connection = do
  tasks <- query_ connection $ Query
    "select * from `tasks_view` \
    \where closed_utc is null and due_utc < datetime('now') \
    \order by `priority` desc"
  pure $ formatTasks conf now tasks


doneTasks :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
doneTasks conf now connection = do
  tasks <- query_ connection $ Query $
    "select * from tasks_view \
    \where closed_utc is not null and state is 'Done' \
    \order by closed_utc desc limit " <> show (headCount conf)
  pure $ formatTasks conf now tasks


obsoleteTasks :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
obsoleteTasks conf now connection = do
  tasks <- query_ connection $ Query $
    "select * from tasks_view \
    \where closed_utc is not null and state is 'Obsolete' \
    \order by ulid desc limit " <> show (headCount conf)
  pure $ formatTasks conf now tasks


deletableTasks :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
deletableTasks conf now connection = do
  tasks <- query_ connection $ Query $
    "select * from tasks_view \
    \where closed_utc is not null and state is 'Deletable' \
    \order by ulid desc limit " <> show (headCount conf)
  pure $ formatTasks conf now tasks


listRepeating :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
listRepeating conf now connection = do
  tasks <- query_ connection $ Query $
    "select * from tasks_view \
    \where repetition_duration is not null \
    \order by repetition_duration desc"

  pure $ formatTasks conf now tasks


listReady :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
listReady conf now connection = do
  tasks <- query_ connection $ Query $
    "select * from tasks_view \
    \where (ready_utc is null \
    \or (ready_utc is not null and ready_utc < datetime('now'))) \
    \and closed_utc is null \
    \order by priority desc \
    \limit " <> show (headCount conf)

  pure $ formatTasks conf now tasks


listWaiting :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
listWaiting conf now connection = do
  tasks <- query_ connection
    "select * from tasks_view \
    \where closed_utc is null \
      \and waiting_utc is not null \
      \and (review_utc > datetime('now') or review_utc is null) \
    \order by waiting_utc desc"

  pure $ formatTasks conf now tasks


listAll :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
listAll conf now connection = do
  tasks <- query_ connection
    "select * from tasks_view order by ulid asc"
  pure $ formatTasks conf now tasks


listNoTag :: Config -> DateTime -> Connection -> IO (Doc AnsiStyle)
listNoTag conf now connection = do
  tasks <-  query_ connection
    "select * from tasks_view \
    \where closed_utc is null and tags is null \
    \order by priority desc"
  pure $ formatTasks conf now tasks


listWithTag :: Config -> DateTime -> Connection -> [Text] -> IO (Doc AnsiStyle)
listWithTag conf now connection tags = do
  let
    getTagQuery =
      (T.intercalate " or ") . (fmap (("tag like '" <>) . (<> "'")))

    ulidsQuery = "\
      \select tasks.ulid \
      \from tasks \
      \left join task_to_tag on tasks.ulid is task_to_tag.task_ulid \
      \where " <> (getTagQuery tags) <> " \
      \group by tasks.ulid \
      \having count(tag) = " <> show (P.length tags)

    mainQuery = FullTask.selectQuery <> "\
      \from (" <> ulidsQuery <> ") tasks1\n\
      \left join tasks_view on tasks1.ulid is tasks_view.ulid\n\
      \order by priority desc"

  -- TODO: Use beam to execute query
  tasks <- query_ connection $ Query mainQuery
  pure $ formatTasks conf now tasks


queryTasks :: Config -> DateTime -> Connection -> Text -> IO (Doc AnsiStyle)
queryTasks conf now connection sqlQuery = do
  tasks <- query_ connection $ Query $
    "select * from `tasks_view` where " <> sqlQuery
  pure $ formatTasks conf now tasks


runSql :: Config -> Text -> IO (Doc AnsiStyle)
runSql conf sqlQuery = do
  result <- readProcess "sqlite3"
    [ (dataDir conf) </> (dbName conf)
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
  deriving Show

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
filterExpParser = do
      tagParser
  <++ notTagParser
  <++ dueParser
  <++ stateParser
  <++ ((InvalidFilter . pack) <$> munch1 (not . isSpace))


filterExpsParser :: ReadP [FilterExp]
filterExpsParser = do
  val <- sepBy1 filterExpParser skipSpaces
  eof
  return val


parseFilterExps :: Text -> IO ()
parseFilterExps input =
  P.print $ readP_to_S filterExpsParser $ T.unpack input


-- | Returns (operator, where-query) tuple
-- TODO: Should be `FilterExp -> Maybe (Text, Text)`
filterToSql :: FilterExp -> (Text, Text)
filterToSql = \case
  HasTag tag          -> ("intersect", "tag like '" <> tag <> "'")
  NotTag tag          -> ("except", "tag like '" <> tag <> "'")
  HasDue utc          -> ("intersect", "due_utc < datetime('" <> utc <>"')")
  HasStatus (Just taskState) -> ("intersect", derivedStateToQuery taskState)
  -- Following cases should never be called, as they are filtered out
  HasStatus Nothing   -> ("", "")
  InvalidFilter _     -> ("", "")


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
            (dquotes $ pretty error) <+> "is an invalid filter." <> hardline
            <> filterHelp
          (HasStatus Nothing) ->
            "Filter contains an invalid state value"
          _ ->
            "The functions should not be called with a valid function"
        errors = P.filter (not . isValidFilter) filterExps
        sqlQuery = getFilterQuery filterExps

      tasks <- query_ connection sqlQuery

      if (P.length errors) > 0
      then
        dieWithError $ vsep (fmap ppInvalidFilter errors)
      else
        pure $ formatTasks conf now tasks

    _ -> dieWithError filterHelp


-- TODO: Increase performance of this query
getFilterQuery :: [FilterExp] -> Query
getFilterQuery filterExps =
  let
    filterTuple = filterToSql <$> P.filter isValidFilter filterExps

    queries = filterTuple <&> \(operator, whereQuery) ->
      operator <> "\n\
      \select tasks.ulid\n\
      \from tasks\n\
      \left join task_to_tag on tasks.ulid is task_to_tag.task_ulid\n\
      \where " <> whereQuery <> "\n\
      \group by tasks.ulid"

    ulidsQuery
      =  "select tasks.ulid from tasks\n"
      <> unlines queries

    mainQuery = FullTask.selectQuery <> "\
      \from (" <> ulidsQuery <> ") tasks1\n\
      \left join tasks_view on tasks1.ulid is tasks_view.ulid\n\
      \order by priority desc, due_utc asc"
  in
    Query mainQuery



formatTasks :: Config -> DateTime -> [FullTask] -> Doc AnsiStyle
formatTasks conf now tasks  =
  if P.length tasks == 0
  then pretty noTasksWarning
  else
    let
      strong = bold <> underlined
      taskUlidWidth = getIdLength $ fromIntegral $ P.length tasks
      docHeader =
             annotate (idStyle conf <> strong)
                (fill taskUlidWidth "Id")
        <++> annotate (priorityStyle conf <> strong)
                (fill (prioWidth conf) "Prio")
        <++> annotate (dateStyle conf <> strong)
                (fill (dateWidth conf) "Opened UTC")
        <++> annotate (bodyStyle conf <> strong)
                (fill (bodyWidth conf) "Body")
        <++> line
    in
      docHeader <>
      vsep (fmap (formatTaskLine conf now taskUlidWidth) tasks) <>
      line


getProgressBar :: Integer -> Double -> Doc AnsiStyle
getProgressBar maxWidthInChars progress =
  let
    barWidth = floor (progress * (fromInteger maxWidthInChars))
    remainingWidth = fromIntegral $ maxWidthInChars - barWidth
  in
    annotate (bgColorDull Green <> colorDull Green)
      (pretty $ P.take (fromIntegral barWidth) $ P.repeat '#') <>
    -- (annotate (bgColorDull Green) $ fill (fromIntegral barWidth) "" <>
    annotate (bgColorDull Black) (fill remainingWidth "")


formatTagLine ::
  Config -> Int -> (Text, Integer, Integer, Double) -> Doc AnsiStyle
formatTagLine conf maxTagLength (tag, open_count, closed_count, progress) =
  let
    barWidth = toInteger $ progressBarWidth conf
    progressPercentage =
      if progress == 0
      then "     "
      else
        pretty (justifyRight 3 ' ' $ T.pack $
          showFFloat (Just 0) (progress * 100) "")
        <+> "%"
  in
    fill maxTagLength (pretty tag)
    <++> pretty (justifyRight (T.length "open") ' ' $ show open_count)
    <++> pretty (justifyRight (T.length "closed") ' ' $ show closed_count)
    <++> progressPercentage <+> (getProgressBar barWidth progress)


formatTags :: Config -> [(Text, Integer, Integer, Double)] -> Doc AnsiStyle
formatTags conf tagTuples =
  let
    percWidth = 6  -- Width of e.g. 100 %
    progressWith = (progressBarWidth conf) + percWidth
    firstOf4 (a, _, _, _) = a
    maxTagLength = tagTuples
      <&> (T.length . firstOf4)
      & P.maximum
  in
    annotate (bold <> underlined) (fill maxTagLength "Tag")
    <++> (annotate (bold <> underlined) "Open")
    <++> (annotate (bold <> underlined) "Closed")
    <++> annotate (bold <> underlined) (fill progressWith "Progress")
    <> line
    <> vsep (fmap (formatTagLine conf maxTagLength) tagTuples)


listTags :: Config -> Connection -> IO (Doc AnsiStyle)
listTags conf connection = do
  tags <- query_ connection $ Query "select * from tags"

  pure $ formatTags conf tags


listProjects :: Config -> Connection -> IO (Doc AnsiStyle)
listProjects conf connection = do
  tags <- query_ connection $
    Query "select * from tags where open > 0 and closed > 0"

  pure $ formatTags conf tags


getStats :: Config -> Connection -> IO (Doc AnsiStyle)
getStats _ connection = do
  [NumRows numOfTasks] <- query_ connection $
    Query "select count(1) from tasks"
  [NumRows numOfTasksOpen] <- query_ connection $
    Query "select count(1) from tasks where closed_utc is null"
  [NumRows numOfTasksClosed] <- query_ connection $
    Query "select count(1) from tasks where closed_utc is not null"

  let
    lengthOfKey = 10
    lengthOfValue = 10

  pure $
         annotate (bold <> underlined) (fill lengthOfKey "Metric")
    <++> annotate (bold <> underlined) (fill lengthOfValue "Value")
    <++> annotate (bold <> underlined) "Share"
    <> line
    <> vsep (
      fill lengthOfKey (pretty ("Tasks" :: Text))
        <++> fill lengthOfValue (pretty (numOfTasks :: Integer))
        <++> pretty ("1.0" :: Text) :
      fill lengthOfKey (pretty ("Open" :: Text))
        <++> fill lengthOfValue (pretty (numOfTasksOpen :: Integer))
        <++> pretty (showAtPrecision 3 $
            (fromIntegral numOfTasksOpen) / (fromIntegral numOfTasks)
          ) :
      fill lengthOfKey (pretty ("Closed" :: Text))
        <++> fill lengthOfValue (pretty (numOfTasksClosed :: Integer))
        <++> pretty (showAtPrecision 3 $
            (fromIntegral numOfTasksClosed) / (fromIntegral numOfTasks)
          ) :
    [])
