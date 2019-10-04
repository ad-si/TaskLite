{-|
Functions to create, update, and delete tasks / tags / notes
-}

module Lib where

import Protolude as P

import Data.Hourglass
import Data.Text as T
import Data.ULID
import Data.Coerce
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
import qualified Text.Fuzzy as Fuzzy
import Text.ParserCombinators.ReadP as ReadP
import GHC.Unicode (isSpace)
import Time.System
import Text.Read (readMaybe)
import Data.Text.Prettyprint.Doc hiding ((<>))
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
  , _tldbTasksView :: f (ViewEntity TaskView)
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


insertTags :: Connection -> TaskUlid -> [Text] -> IO ()
insertTags connection taskUlid tags = do
  taskToTags <- forM tags $ \tag -> do
    tagUlid <- fmap (toLower . show) getULID
    pure $ TaskToTag tagUlid taskUlid tag

  runBeamSqlite connection $ runInsert $
    insert (_tldbTaskToTag taskLiteDb) $
    insertValues taskToTags


insertNotes :: Connection -> TaskUlid -> [Note] -> IO ()
insertNotes connection primKey notes = do
  taskToNotes <- forM notes $ \theNote -> do
    pure $ TaskToNote (Note.ulid theNote) primKey (Note.body theNote)

  runBeamSqlite connection $ runInsert $
    insert (_tldbTaskToNote taskLiteDb) $
    insertValues taskToNotes


-- | Tuple is (Maybe createdUtc, noteBody)
insertNoteTuples :: Connection -> TaskUlid -> [(Maybe DateTime, Text)] -> IO ()
insertNoteTuples connection taskUlid notes = do
  taskToNotes <- forM notes $ \(createdUtc, noteBody) -> do
    noteUlid <- getULID
    pure $ TaskToNote
      ((toLower . show . maybe noteUlid (setDateTime noteUlid))
        createdUtc)
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
  insertTags connection (primaryKey task) tags
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
  insertTags connection (primaryKey task) tags
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
      (\task -> mconcat [ (Task.state task) <-. val_ theTaskState
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


reviewTasks :: Config -> Connection -> [Text] -> IO (Doc AnsiStyle)
reviewTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      now <- timeCurrentP
      let
        taskUlid@(TaskUlid idText) = primaryKey task
        threeDays = (pack . timePrint (utcFormat conf))
          (now `timeAdd` mempty { durationHours = 72 })
        prettyBody = dquotes (pretty $ Task.body task)
        prettyId = dquotes (pretty idText)

      runBeamSqlite connection $ runUpdate $
        update (_tldbTasks taskLiteDb)
          (\theTask -> mconcat
            [(Task.review_utc theTask) <-. val_ (Just threeDays)])
          (\theTask -> primaryKey theTask ==. val_ taskUlid)

      numOfChanges <- changes connection

      pure $ if numOfChanges == 0
        then "⚠️  Task" <+> prettyBody <+> "with id" <+> prettyId
              <+> "could not be reviewed"
        else "🔎 Finished review for task" <+> prettyBody
              <+> "with id" <+> prettyId

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
          if Task.repetition_duration task == Nothing
          then pure Nothing
          else do
            newUlid <- formatUlid getULID
            let
              nowMaybe = ulidTextToDateTime newUlid
              dueUtc = (Task.due_utc task) >>= parseUtc
              showDateTime = pack . timePrint (utcFormat conf)
              nextDue = liftA2 timeAdd
                (if nowMaybe < dueUtc then dueUtc else nowMaybe)
                ((Task.repetition_duration task) >>= parseIsoDuration)

            -- TODO: Investigate why this isn't working and replace afterwards
            -- runBeamSqlite connection $ runInsert $
            --   insert (_tldbTasks taskLiteDb) $
            --   insertValues [ task
            --     { Task.ulid = val_ newUlid
            --     , Task.due_utc = nowMaybe + (Task.repetition_duration task)
            --     }
            --   ]

            runBeamSqlite connection $ do
              runInsert $ insert (_tldbTasks taskLiteDb) $ insertFrom $ do
                originalTask <- filter_
                  (\theTask -> primaryKey theTask ==. val_ taskUlid)
                  (all_ $ _tldbTasks taskLiteDb)

                pure originalTask
                  { Task.ulid = val_ newUlid
                  , Task.due_utc = val_ $ fmap showDateTime nextDue
                  , Task.awake_utc = val_ $
                      fmap showDateTime $ liftA2 timeAdd
                        ((Task.awake_utc task) >>= parseUtc)
                        ((Task.repetition_duration task) >>= parseIsoDuration)
                  , Task.ready_utc = val_ $
                      fmap showDateTime $ liftA2 timeAdd
                        ((Task.ready_utc task) >>= parseUtc)
                        ((Task.repetition_duration task) >>= parseIsoDuration)
                  }

              -- Duplicate tags
              tags <- runSelectReturningList $ select $
                filter_ (\tag -> TaskToTag.task_ulid tag ==. val_ taskUlid) $
                all_ (_tldbTaskToTag taskLiteDb)

              liftIO $ insertTags
                connection
                (TaskUlid newUlid)
                (fmap TaskToTag.tag tags)

            liftIO $ pure $ Just $ "➡️  Created next task"
              <+> dquotes (pretty $ Task.body task)
              <+> "in repetition series"
              <+> dquotes (pretty $ Task.group_ulid task)

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
        else "🚮  Marked task" <+> prettyBody <+> "with id" <+> prettyId
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


infoTask :: Config -> Connection -> Text -> IO (Doc AnsiStyle)
infoTask conf connection idSubstr = do
  execWithTask conf connection idSubstr $ \task -> do
    let
      taskUlid@(TaskUlid idText) = primaryKey task

    tasks <- query connection
      (Query "select * from tasks_view where ulid is ?")
      [idText :: Text]

    runBeamSqlite connection $ do
      tags <- runSelectReturningList $ select $
        filter_ (\tag -> TaskToTag.task_ulid tag ==. val_ taskUlid) $
        all_ (_tldbTaskToTag taskLiteDb)

      notes <- runSelectReturningList $ select $
        filter_ (\theNote -> TaskToNote.task_ulid theNote ==. val_ taskUlid) $
        all_ (_tldbTaskToNote taskLiteDb)

      let
        -- TODO: Colorize all YAML keys
        mkGreen = annotate (color Green)
        yamlList = (hang 2) . ("-" <+>)
        rmLastLine = unlines . P.reverse . P.drop 1 . P.reverse . lines . show
        formattedTask priority =
          pretty task <> hardline
          <> mkGreen "priority:" <+> (pretty priority) <> hardline
          <> mkGreen "tags:\n"
          <> indent 2 (vsep $ fmap
              (yamlList . pretty . rmLastLine . pretty) tags)
          <> hardline
          <> mkGreen "notes:\n"
          <> indent 2 (vsep $ fmap
              (yamlList . pretty . rmLastLine . pretty) notes)

      pure $ case P.head (tasks :: [FullTask]) of
        Nothing -> pretty noTasksWarning
        Just fullTask -> formattedTask (priority fullTask :: Maybe Float)


nextTask :: Connection -> IO (Doc AnsiStyle)
nextTask connection = do
  let
    stateNullQuery = "select * from `tasks_view` where state is NULL "
    orderByAndLimit = "order by `priority` desc limit 1"
  tasks <- query_ connection $ Query $ stateNullQuery <> orderByAndLimit

  pure $ case P.head (tasks :: [FullTask]) of
    Nothing -> pretty noTasksWarning
    Just task -> pretty task


findTask :: Connection -> Text -> IO (Doc AnsiStyle)
findTask connection aPattern = do
  tasks <- query_ connection $ Query
    "select ulid, body, tags, notes, metadata from tasks_view"

  let
    scoreWidth = 5
    numOfResults = 8
    results = Fuzzy.filter
      aPattern
      (tasks :: [(Text, Text, Maybe [Text], Maybe [Text], Maybe Text)])
      "\x1b[4m\x1b[32m" -- Set underline and color to green
      "\x1b[0m"
      (\(ulid, theBody, tags, notes, metadata) -> unwords
        [ ulid
        , "\n"
        , theBody
        , maybe "" unwords tags
        , maybe "" unwords notes
        , T.replace "\",\"" "\", \"" $ fromMaybe "" metadata
        ])
      False -- Case insensitive
    moreResults = (P.length results) - numOfResults
    header =
      annotate (underlined <> color Red) (fill scoreWidth "Score") <++>
      annotate (underlined) (fill 10 "Task") <>
      hardline
    body =
      results
        & P.take numOfResults
        <&> (\result ->
              annotate (color Red)
                (fill scoreWidth $ pretty $ Fuzzy.score result)
              <++> (align (reflow $ Fuzzy.rendered result)))
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
      ulid <- fmap (toLower . show) getULID

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
      ulid <- fmap (toLower . show) getULID

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


undueTasks :: Config -> Connection -> [IdText] -> IO (Doc AnsiStyle)
undueTasks conf connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithTask conf connection idSubstr $ \task -> do
      let
        taskUlid@(TaskUlid idText) = primaryKey task
        prettyBody = dquotes (pretty $ Task.body task)
        prettyId = dquotes (pretty idText)

      runBeamSqlite connection $ runUpdate $
        update (_tldbTasks taskLiteDb)
          (\task_ -> mconcat [(Task.due_utc task_) <-. (val_ Nothing)])
          (\task_ -> primaryKey task_ ==. val_ taskUlid)

      pure $ "💥 Removed due UTC of task" <+> prettyBody
            <+> "with id" <+> prettyId

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


formatTaskLine :: Config -> DateTime -> Int -> FullTask -> Doc AnsiStyle
formatTaskLine conf now taskUlidWidth task =
  let
    id = pretty $ T.takeEnd taskUlidWidth $ FullTask.ulid task
    createdUtc = fmap
      (pack . timePrint ISO8601_Date)
      (ulidTextToDateTime $ FullTask.ulid task)
    body = FullTask.body task
    tags = fromMaybe [] $ FullTask.tags task
    formatTag = annotate (tagStyle conf)
      . (annotate (color Black) "+" <>)
      . pretty
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
        hsep (tags <$$> formatTag) :
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
    \where waiting_utc is not null \
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
    parserResults = readP_to_S filterExpsParser $ T.unpack (unwords exps)
    filterMay = listToMaybe parserResults

  case filterMay of
    Nothing -> pure "This case should be impossible as the parser doesn't fail"
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
        sqlQuery = getFilterQuery filterExps

      tasks <- query_ connection sqlQuery

      pure $ fromMaybe (formatTasks conf now tasks) errorsDoc


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
