{-|
Functions to create, update, and delete tasks / tags / notes
-}

module Lib where

import Protolude as P

import Data.Hourglass
import Data.Text as T
import Data.ULID
import Database.Beam
import Database.Beam.Sqlite
import Database.Beam.Schema.Tables
import Database.SQLite.Simple as Sql
import Numeric
import System.Directory
import System.IO as SIO
import System.Process (readProcess)
import System.Posix.User (getEffectiveUserName)
import qualified Text.Fuzzy as Fuzzy
import Text.ParserCombinators.ReadP as ReadP
import GHC.Unicode (isSpace)
import Time.System
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Text.Prettyprint.Doc.Util
import Data.Text.Prettyprint.Doc.Render.Terminal
import Unsafe (unsafeHead)

import Utils
import Task as Task
import TaskView
import FullTask as FullTask
import Note as Note
import TaskToNote as TaskToNote
import TaskToTag as TaskToTag
import Config


getMainDir :: FilePath -> FilePath
getMainDir = (<> "/" <> (mainDir conf) )


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


getDbPath :: IO FilePath
getDbPath = do
  homeDir <- getHomeDirectory
  pure $ (getMainDir homeDir) <> "/" <> (dbName conf)


setupConnection :: IO Connection
setupConnection = do
  homeDir <- getHomeDirectory
  createDirectoryIfMissing True $ getMainDir homeDir
  open $ (getMainDir homeDir) <> "/" <> (dbName conf)


execWithConn :: (Connection -> IO a) -> IO a
execWithConn func = do
  homeDir <- getHomeDirectory
  createDirectoryIfMissing True $ getMainDir homeDir
  withConnection
    ((getMainDir homeDir) <> "/" <> (dbName conf))
    func


writeToLog :: [Char] -> IO ()
writeToLog message = do
  homeDir <- getHomeDirectory
  let logFile = (getMainDir homeDir) <> "/log.sql"
  -- Use System.IO so it doesn't have to be converted to Text first
  SIO.appendFile logFile $ message <> "\n"


insertTask :: Connection -> Task -> IO ()
insertTask connection task = do
  runBeamSqliteDebug writeToLog connection $ runInsert $
    insert (_tldbTasks taskLiteDb) $
    insertValues [task]


insertTags :: Connection -> TaskUlid -> [Text] -> IO ()
insertTags connection taskUlid tags = do
  taskToTags <- forM tags $ \tag -> do
    tagUlid <- fmap (toLower . show) getULID
    pure $ TaskToTag tagUlid taskUlid tag

  runBeamSqliteDebug writeToLog connection $ runInsert $
    insert (_tldbTaskToTag taskLiteDb) $
    insertValues taskToTags


insertNotes :: Connection -> TaskUlid -> [Note] -> IO ()
insertNotes connection primKey notes = do
  taskToNotes <- forM notes $ \theNote -> do
    pure $ TaskToNote (Note.ulid theNote) primKey (Note.body theNote)

  runBeamSqliteDebug writeToLog connection $ runInsert $
    insert (_tldbTaskToNote taskLiteDb) $
    insertValues taskToNotes


-- | Tuple is (Maybe createdUtc, noteBody)
insertNoteTuples :: Connection -> TaskUlid -> [(Maybe DateTime, Text)] -> IO ()
insertNoteTuples connection taskUlid notes = do
  taskToNotes <- forM notes $ \(createdUtc, noteBody) -> do
    noteUlid <- getULID
    pure $ TaskToNote
      (fromMaybe
        (toLower $ show noteUlid)
        (fmap (toLower . show . setDateTime noteUlid) createdUtc))
      taskUlid
      noteBody

  runBeamSqliteDebug writeToLog connection $ runInsert $
    insert (_tldbTaskToNote taskLiteDb) $
    insertValues taskToNotes


formatElapsedP :: IO ElapsedP -> IO Text
formatElapsedP =
  fmap (pack . (timePrint $ utcFormat conf))


formatUlid :: IO ULID -> IO Text
formatUlid =
  fmap (toLower . show)


-- | Parses the body of the tasks and extracts all meta data
-- | Returns a tuple (body, tags, due_utc)
-- TODO: Replace with parsec implementation
parseTaskBody :: [Text] -> (Text, [Text], Maybe Text)
parseTaskBody bodyWords =
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
      <&> pack . (timePrint $ utcFormat conf)
  in
    (body, tags, dueUtc)


addTask :: Connection -> [Text] -> IO (Doc AnsiStyle)
addTask connection bodyWords = do
  ulid <- formatUlid getULID
  modified_utc <- formatElapsedP timeCurrentP
  effectiveUserName <- getEffectiveUserName
  let
    (body, tags, due_utc) = parseTaskBody bodyWords
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
    "🆕 Added task" <+> (dquotes $ pretty $ Task.body task)
    <+> "with ulid" <+> (dquotes $ pretty $ Task.ulid task)


logTask :: Connection -> [Text] -> IO (Doc AnsiStyle)
logTask connection bodyWords = do
  ulid <- formatUlid getULID
  -- TODO: Set via a SQL trigger
  modified_utc <- formatElapsedP timeCurrentP
  effectiveUserName <- getEffectiveUserName
  let
    (body, extractedTags, due_utc) = parseTaskBody bodyWords
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
    "📝 Logged task" <+> (dquotes $ pretty $ Task.body task)
    <+> "with ulid" <+> (dquotes $ pretty $ Task.ulid task)


execWithId ::
  Connection -> Text -> (TaskUlid -> IO (Doc AnsiStyle)) -> IO (Doc AnsiStyle)
execWithId connection idSubstr callback = do
  tasks <- (query connection
      (Query $ "select * from " <> tableName conf <> " where `ulid` like ?")
      ["%"  <> idSubstr :: Text]
    ) :: IO [Task]

  let
    numOfTasks = P.length tasks
    ulidLength = 26
    prefix = if (T.length idSubstr) == ulidLength
      then ""
      else "…"
    quote = dquotes . pretty

  if
    | numOfTasks == 0 -> pure $
        "⚠️  Task" <+> (quote $ prefix <> idSubstr) <+> "does not exist"
    | numOfTasks == 1 ->
        callback $ primaryKey $ unsafeHead tasks
    | numOfTasks > 1 -> pure $
        "⚠️  Id slice" <+> (quote idSubstr) <+> "is not unique."
        <+> "Please use a longer slice!"
    | otherwise -> pure "This case should not be possible"


setStateAndClosed :: Connection -> TaskUlid -> Maybe TaskState -> IO ()
setStateAndClosed connection taskUlid theTaskState = do
  runBeamSqliteDebug writeToLog connection $ runUpdate $
    update (_tldbTasks taskLiteDb)
      (\task -> [(Task.state task) <-. val_ theTaskState])
      (\task -> primaryKey task ==. val_ taskUlid &&.
                (Task.state task) /=. val_ theTaskState)


doTasks :: Connection -> [Text] -> IO (Doc AnsiStyle)
doTasks connection ids = do
  docs <- forM ids $ \idSubstr -> do
    doc <- execWithId connection idSubstr $ \taskUlid@(TaskUlid idText) -> do
      setStateAndClosed connection taskUlid $ Just Done

      numOfChanges <- changes connection

      pure $ pretty $ if numOfChanges == 0
        then "⚠️  Task \"" <> idText <> "\" is already done"
        else "✅ Finished task \"" <> idText <> "\""
    pure doc
  pure $ vsep docs


endTasks :: Connection -> [Text] -> IO (Doc AnsiStyle)
endTasks connection ids = do
  docs <- forM ids $ \idSubstr -> do
    doc <- execWithId connection idSubstr $ \taskUlid@(TaskUlid idText) -> do
      setStateAndClosed connection taskUlid $ Just Obsolete

      numOfChanges <- changes connection

      pure $ pretty $ if numOfChanges == 0
        then "⚠️  Task \"" <> idText <> "\" is already marked as obsolete"
        else "⏹  Marked task \"" <> idText <> "\" as obsolete"
    pure doc
  pure $ vsep docs


deleteTasks :: Connection -> [Text] -> IO (Doc AnsiStyle)
deleteTasks connection ids = do
  docs <- forM ids $ \idSubstr -> do
    doc <- execWithId connection idSubstr $ \taskUlid@(TaskUlid idText) -> do
      runBeamSqlite connection $ do
        runDelete $ delete
          (_tldbTasks taskLiteDb)
          (\task -> primaryKey task ==. val_ taskUlid)

        runDelete $ delete
          (_tldbTaskToTag taskLiteDb)
          (\tag -> TaskToTag.task_ulid tag ==. val_ taskUlid)

        runDelete $ delete
          (_tldbTaskToNote taskLiteDb)
          (\noteValue -> TaskToNote.task_ulid noteValue ==. val_ taskUlid)

        pure $ pretty ("❌ Deleted task \"" <> idText <> "\"" :: Text)
    pure doc
  pure $ vsep docs


adjustPriority :: Float -> [IdText] -> IO (Doc AnsiStyle)
adjustPriority adjustment ids  = do
  dbPath <- getDbPath
  withConnection dbPath $ \connection -> do
    docs <- forM ids $ \idSubstr -> do
      doc <- execWithId connection idSubstr $ \(TaskUlid idText) -> do
        -- TODO: Figure out why this doesn't work
        -- runBeamSqliteDebug writeToLog connection $ runUpdate $
        --   update (_tldbTasks taskLiteDb)
        --     (\task -> [(Task.priority_adjustment task) <-.
        --       fmap (+ adjustment) (current_ (Task.priority_adjustment task))
        --     ])
        --     (\task -> primaryKey task ==. val_ taskUlid)

        execute connection
          (Query $ "update `tasks` \
            \set `priority_adjustment` = ifnull(`priority_adjustment`, 0) + ? \
            \where `ulid` == ?")
          (adjustment, idText :: Text)

        numOfChanges <- changes connection

        pure $ pretty $ if numOfChanges == 0
          then
            "⚠️ An error occured \
            \while adjusting the priority of task \"" <> idText <> "\""
          else (
            (if adjustment > 0 then "⬆️  Increased" else "⬇️  Decreased")
            <> " priority of task \""
            <> idText <> "\" by " <> (show $ abs adjustment)
          )

      pure doc
    pure $ vsep docs


startTasks :: Connection -> [Text] -> IO (Doc AnsiStyle)
startTasks connection ids = do
  logMessage <- addNote connection "start" ids

  pure $ pretty $ T.replace
    "🗒  Added a note to"
    "⏳ Started"
    (show logMessage)


stopTasks :: Connection -> [Text] -> IO (Doc AnsiStyle)
stopTasks connection ids = do
  logMessages <- addNote connection "stop" ids

  pure $ pretty $ T.replace
    "🗒  Added a note to"
    "⌛️ Stopped"
    (show logMessages)


infoTask :: Connection -> Text -> IO (Doc AnsiStyle)
infoTask connection idSubstr = do
  execWithId connection idSubstr $ \taskUlid@(TaskUlid idText)-> do
    tasks <- query connection
      (Query "select * from tasks_view where ulid is ?")
      [idText :: Text]

    runBeamSqlite connection $ do
      task <- runSelectReturningOne $
        lookup_ (_tldbTasks taskLiteDb) taskUlid

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
    selectQuery = "select * from `tasks_view` where state is NULL "
    orderByAndLimit = "order by `priority` desc limit 1"
  tasks <- query_ connection $ Query $ selectQuery <> orderByAndLimit

  pure $ case P.head (tasks :: [FullTask]) of
    Nothing -> pretty noTasksWarning
    Just task -> pretty task


findTask :: Connection -> Text -> IO (Doc AnsiStyle)
findTask connection pattern = do
  tasks <- query_ connection $ Query $
    "select ulid, body, tags, notes, metadata from tasks_view"

  let
    scoreWidth = 5
    numOfResults = 8
    results = Fuzzy.filter
      pattern
      (tasks :: [(Text, Text, Maybe [Text], Maybe [Text], Maybe Text)])
      "\x1b[4m\x1b[32m" -- Set underline and color to green
      "\x1b[0m"
      (\(ulid, theBody, tags, notes, metadata) -> unwords
        [ ulid
        , "\n"
        , theBody
        , fromMaybe "" (unwords <$> tags)
        , fromMaybe "" (unwords <$> notes)
        , T.replace "\",\"" "\", \"" $ fromMaybe "" metadata
        ])
      False -- Case insensitive
    moreResults = (P.length results) - numOfResults
    header =
      (annotate (underlined <> color Red) $ fill scoreWidth "Score") <++>
      (annotate (underlined) $ fill 10 "Task") <>
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


addTag :: Connection -> Text -> [IdText] -> IO (Doc AnsiStyle)
addTag connection tag ids = do
  docs <- forM ids $ \idSubstr ->
    execWithId connection idSubstr $ \taskUlid@(TaskUlid idText) -> do
      now <- fmap (pack . (timePrint $ utcFormat conf)) timeCurrentP
      ulid <- fmap (toLower . show) getULID

      let taskToTag = TaskToTag ulid taskUlid tag

      runBeamSqliteDebug writeToLog connection $ runInsert $
        insert (_tldbTaskToTag taskLiteDb) $
        insertValues [taskToTag]

      runBeamSqliteDebug writeToLog connection $ runUpdate $
        update (_tldbTasks taskLiteDb)
          (\task -> [(Task.modified_utc task) <-. val_ now])
          (\task -> primaryKey task ==. val_ taskUlid)

      pure $ "🏷  Added tag" <+> (dquotes $ pretty tag)
        <+> "to task" <+> (dquotes $ pretty idText)

  pure $ vsep docs


addNote :: Connection -> Text -> [IdText] -> IO (Doc AnsiStyle)
addNote connection noteBody ids = do
  docs <- forM ids $ \idSubstr ->
    execWithId connection idSubstr $ \taskUlid@(TaskUlid idText) -> do
      now <- fmap (pack . (timePrint $ utcFormat conf)) timeCurrentP
      ulid <- fmap (toLower . show) getULID

      let taskToNote = TaskToNote ulid taskUlid noteBody

      runBeamSqliteDebug writeToLog connection $ runInsert $
        insert (_tldbTaskToNote taskLiteDb) $
        insertValues [taskToNote]

      runBeamSqliteDebug writeToLog connection $ runUpdate $
        update (_tldbTasks taskLiteDb)
          (\task -> [(Task.modified_utc task) <-. val_ now])
          (\task -> primaryKey task ==. val_ taskUlid)

      pure $ "🗒  Added a note to task" <+> (dquotes $ pretty idText)

  pure $ vsep docs


setDueUtc :: Connection -> DateTime -> [IdText] -> IO (Doc AnsiStyle)
setDueUtc connection datetime ids = do
  let
    utcText :: Text
    utcText = pack $ timePrint (utcFormat conf) datetime

  docs <- forM ids $ \idSubstr ->
    execWithId connection idSubstr $ \taskUlid@(TaskUlid idText) -> do
      runBeamSqliteDebug writeToLog connection $ runUpdate $
        update (_tldbTasks taskLiteDb)
          (\task -> [(Task.due_utc task) <-. (val_ $ Just utcText)])
          (\task -> primaryKey task ==. val_ taskUlid)

        -- TODO: Update modified_utc via SQL trigger

      pure $ "📅 Set due UTC to" <+> (dquotes $ pretty utcText)
        <+> "of task" <+> (dquotes $ pretty idText)

  pure $ vsep docs


undueTasks :: Connection -> [IdText] -> IO (Doc AnsiStyle)
undueTasks connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithId connection idSubstr $ \taskUlid@(TaskUlid idText) -> do
      runBeamSqliteDebug writeToLog connection $ runUpdate $
        update (_tldbTasks taskLiteDb)
          (\task -> [(Task.due_utc task) <-. (val_ Nothing)])
          (\task -> primaryKey task ==. val_ taskUlid)

      pure $ pretty ("💥 Removed due UTC of task \"" <> idText <> "\"" :: Text)

  pure $ vsep docs


duplicateTasks :: Connection -> [IdText] -> IO (Doc AnsiStyle)
duplicateTasks connection ids = do
  docs <- forM ids $ \idSubstr -> do
    execWithId connection idSubstr $ \taskUlid@(TaskUlid idText) -> do
      dupeUlid <- formatUlid getULID
      -- TODO: Check if modified_utc can be set via an SQL trigger
      modified_utc <- formatElapsedP timeCurrentP

      -- Duplicate task
      runBeamSqliteDebug writeToLog connection $ do
        runInsert $ insert (_tldbTasks taskLiteDb) $ insertFrom $ do
          task <- filter_
            (\task -> primaryKey task ==. val_ taskUlid)
            (all_ $ _tldbTasks taskLiteDb)

          pure task
            { Task.ulid = val_ dupeUlid
            , Task.due_utc = val_ Nothing
            , Task.sleep_utc = val_ Nothing
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

      pure $ pretty $ if numOfChanges == 0
        then "⚠️  Task \"" <> idText <> "\" could not be duplicated"
        else "👯  Created a duplicate of task \"" <> idText
          <> "\" with id \"" <> dupeUlid <> "\""

  pure $ vsep docs


showAtPrecision :: Double -> Text
showAtPrecision number =
  let tuple = breakOn "." (show number)
  in fst tuple <> (T.replace ".0" "  " . T.take 2 . snd) tuple


formatTaskLine :: Int -> FullTask -> Doc AnsiStyle
formatTaskLine taskUlidWidth task =
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
    multilineIndent = 2
    hangWidth = taskUlidWidth + 2
      + (dateWidth conf) + 2
      + (prioWidth conf) + 2
      + multilineIndent
    hhsep = concatWith (<++>)
    isEmptyDoc doc = (show doc) /= ("" :: Text)
    grayOut isDone doc = if isDone
      then annotate (bodyStyle conf) doc
      else annotate (bodyClosedStyle conf) doc
    taskLine = createdUtc <$$> \taskDate ->
      hang hangWidth $ hhsep $ P.filter isEmptyDoc (
        annotate (idStyle conf) id :
        annotate (priorityStyle conf) (pretty $ justifyRight 4 ' '
          $ showAtPrecision $ realToFrac
          $ fromMaybe 0 (FullTask.priority task)) :
        annotate (dateStyle conf) (pretty taskDate) :
        grayOut (isNothing $ FullTask.closed_utc task) (reflow body) :
        annotate (dueStyle conf) (pretty dueUtcMaybe) :
        annotate (closedStyle conf) (pretty closedUtcMaybe) :
        hsep (tags <$$> formatTag) :
        [])
  in
    fromMaybe
      ("Id" <+> (dquotes $ pretty $ FullTask.ulid task) <+>
        "is an invalid ulid and could not be converted to a datetime")
      taskLine


getIdLength :: Float -> Int
getIdLength numOfItems =
  -- TODO: Calculate idLength by total number of tasks, not just of the viewed
  let
    targetCollisionChance = 0.01  -- Targeted likelihood of id collisions
    sizeOfAlphabet = 32  -- Crockford's base 32 alphabet
  in
    (ceiling $ log
      (numOfItems / targetCollisionChance) / log sizeOfAlphabet) + 1


countTasks :: Filter TaskState -> IO (Doc AnsiStyle)
countTasks taskStateFilter = do
  execWithConn $ \connection -> do
    [NumRows taskCount] <- case taskStateFilter of
      NoFilter -> query_ connection $ Query $
        "select count(*) from `" <> tableName conf <> "`"
      Utils.Only taskState -> query connection
        (Query $ "select count(*) from `" <> tableName conf
          <> "` where `state` == ?")
        [(show taskState) :: Text]

    pure $ pretty taskCount


-- TODO: Print number of remaining tasks and how to display them at the bottom
headTasks :: Connection -> IO (Doc AnsiStyle)
headTasks connection = do
  tasks <- query_ connection $ Query $
    -- TODO: Add `wait_utc` < datetime('now')"
    "select * from tasks_view \
    \where closed_utc is null \
    \order by `priority` desc limit " <> show (headCount conf)
  pure $ formatTasks tasks


newTasks :: Connection -> IO (Doc AnsiStyle)
newTasks connection = do
  tasks <- query_ connection $ Query $
    -- TODO: Add `wait_utc` < datetime('now')"
    "select * from `tasks_view` \
    \where closed_utc is null \
    \order by `ulid` desc limit " <> show (headCount conf)
  pure $ formatTasks tasks


openTasks :: Connection -> IO (Doc AnsiStyle)
openTasks connection = do
  tasks <- query_ connection $ Query $
    "select * from `tasks_view` \
    \where closed_utc is null \
    \order by `ulid` desc"
  pure $ formatTasks tasks


overdueTasks :: Connection -> IO (Doc AnsiStyle)
overdueTasks connection = do
  tasks <- query_ connection $ Query $
    "select * from `tasks_view` \
    \where closed_utc is null and due_utc < datetime('now') \
    \order by `priority` desc"
  pure $ formatTasks tasks


doneTasks :: Connection -> IO (Doc AnsiStyle)
doneTasks connection = do
  tasks <- query_ connection $ Query $
    "select * from tasks_view \
    \where closed_utc is not null and state is 'Done' \
    \order by closed_utc desc limit " <> show (headCount conf)
  pure $ formatTasks tasks


obsoleteTasks :: Connection -> IO (Doc AnsiStyle)
obsoleteTasks connection = do
  tasks <- query_ connection $ Query $
    "select * from tasks_view \
    \where closed_utc is not null and state is 'Obsolete' \
    \order by ulid desc limit " <> show (headCount conf)
  pure $ formatTasks tasks


listWaiting :: Connection -> IO (Doc AnsiStyle)
listWaiting connection = do
  tasks <- query_ connection
    "select * from tasks_view \
    \where state == 'Waiting' \
    \order by priority desc"

  pure $ formatTasks tasks


listAll :: Connection -> IO (Doc AnsiStyle)
listAll connection = do
  tasks <-  query_ connection
    "select * from tasks_view order by priority desc"
  pure $ formatTasks tasks


listNoTag :: Connection -> IO (Doc AnsiStyle)
listNoTag connection = do
  tasks <-  query_ connection
    "select * from tasks_view \
    \where closed_utc is null and tags is null \
    \order by priority desc"
  pure $ formatTasks tasks


listWithTag :: Connection -> [Text] -> IO (Doc AnsiStyle)
listWithTag connection tags = do
  let
    getTagQuery =
      (T.intercalate " or ") . (fmap (("tag like '" <>) . (<> "'")))

    ulidsQuery = "\
      \select tasks.ulid \
      \from tasks \
      \left join task_to_tag on tasks.ulid is task_to_tag.task_ulid \
      \where " <> (getTagQuery tags) <> " \
      \group by tasks.ulid \
      \having count(tag) = " <> (show $ P.length tags)

    mainQuery = "\
      \select\n\
        \tasks_view.ulid as ulid, body, state, due_utc, sleep_utc, closed_utc,\n\
        \modified_utc, tags, notes, priority, metadata, user\n\
      \from (" <> ulidsQuery <> ") tasks1\n\
      \left join tasks_view on tasks1.ulid is tasks_view.ulid\n\
      \order by priority desc"

  tasks <- query_ connection $ Query mainQuery
  pure $ formatTasks tasks


queryTasks :: Connection -> Text -> IO (Doc AnsiStyle)
queryTasks connection sqlQuery = do
  tasks <- query_ connection $ Query $
    "select * from `tasks_view` where " <> sqlQuery
  pure $ formatTasks tasks


runSql :: Text -> IO (Doc AnsiStyle)
runSql sqlQuery = do
  homeDir <- getHomeDirectory
  result <- readProcess "sqlite3"
    [ (getMainDir homeDir) <> "/" <> (dbName conf)
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
  | HasStatus (Maybe TaskState)
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
  utcStr <- munch (not . isSpace)
  pure $ HasStatus $ textToTaskState $ pack utcStr


filterExpParser :: ReadP FilterExp
filterExpParser = do
      tagParser
  <++ notTagParser
  <++ dueParser
  <++ stateParser
  <++ ((InvalidFilter . pack) <$> (munch1 $ not . isSpace))


filterExpsParser :: ReadP [FilterExp]
filterExpsParser = do
  val <- sepBy1 filterExpParser skipSpaces
  eof
  return val


parseFilterExps :: Text -> IO ()
parseFilterExps input =
  P.print $ readP_to_S filterExpsParser $ T.unpack input


-- | Returns (operator, where-query) tuple
filterToSql :: FilterExp -> (Text, Text)
filterToSql = \case
  HasTag tag          -> ("intersect", "tag like '" <> tag <> "'")
  NotTag tag          -> ("except", "tag like '" <> tag <> "'")
  HasDue utc          -> ("intersect", "due_utc < datetime('" <> utc <>"')")
  HasStatus (Just taskState) -> ("intersect", "state is '" <> (show taskState) <> "'")
  HasStatus Nothing   -> ("", "")
  InvalidFilter _     -> ("", "") -- Should never be called


runFilter :: Connection -> [Text] -> IO (Doc AnsiStyle)
runFilter connection exps = do
  let
    parserResults = readP_to_S filterExpsParser $ T.unpack (unwords exps)
    filterMay = listToMaybe parserResults

  case filterMay of
    Nothing -> pure "This case should be impossible as the parser doesn't fail"
    Just (filterExps, _) -> do
      let
        isValid = \case InvalidFilter _ -> False; _ -> True
        ppInvalidFilter (InvalidFilter error) =
          pretty $ "\"" <> error <> "\" is an invalid filter"
        ppInvalidFilter _ =
          "The functions should not be called with a valid function"
        errors = P.filter (not . isValid) filterExps
        errorsDoc = if (P.length errors) > 0
          then Just $
            (vsep $ fmap (annotate (color Red) . ppInvalidFilter) errors)
            <> hardline <> hardline
          else Nothing
        sqlQuery = getFilterQuery filterExps

      tasks <- query_ connection sqlQuery

      pure $
        case errorsDoc of
          Nothing -> formatTasks tasks
          Just doc -> doc


-- TODO: Increase performance of this query
getFilterQuery :: [FilterExp] -> Query
getFilterQuery filterExps =
  let
    isValid = \case InvalidFilter _ -> False; _ -> True
    filterTuple = fmap filterToSql $ P.filter isValid filterExps

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

    mainQuery = "\
      \select\n\
      \  tasks_view.ulid as ulid, body, state, due_utc, sleep_utc, closed_utc,\n\
      \  modified_utc, tags, notes, priority, metadata, user\n\
      \from (" <> ulidsQuery <> ") tasks1\n\
      \left join tasks_view on tasks1.ulid is tasks_view.ulid"
  in
    Query mainQuery



formatTasks :: [FullTask] -> Doc AnsiStyle
formatTasks tasks =
  if P.length tasks == 0
  then pretty noTasksWarning
  else
    let
      strong = bold <> underlined
      taskUlidWidth = getIdLength $ fromIntegral $ P.length tasks
      docHeader =
             (annotate (idStyle conf <> strong) $
                fill taskUlidWidth "Id")
        <++> (annotate (priorityStyle conf <> strong) $
                fill (prioWidth conf) "Prio")
        <++> (annotate (dateStyle conf <> strong) $
                fill (dateWidth conf) "Opened UTC")
        <++> (annotate (bodyStyle conf <> strong) $
                fill (bodyWidth conf) "Body")
        <++> line
    in
      docHeader <>
      (vsep $ fmap (formatTaskLine taskUlidWidth) tasks) <>
      line


getProgressBar :: Integer -> Double -> Doc AnsiStyle
getProgressBar maxWidthInChars progress =
  let
    barWidth = floor (progress * (fromInteger maxWidthInChars))
    remainingWidth = fromIntegral $ maxWidthInChars - barWidth
  in
    (annotate (bgColorDull Green <> colorDull Green) $ pretty $
      P.take (fromIntegral barWidth) $ P.repeat '#') <>
    -- (annotate (bgColorDull Green) $ fill (fromIntegral barWidth) "" <>
    (annotate (bgColorDull Black) $ fill remainingWidth "")


formatTagLine :: Int -> (Text, Integer, Integer, Double) -> Doc AnsiStyle
formatTagLine maxTagLength (tag, open_count, closed_count, progress) =
  let
    barWidth = toInteger $ progressBarWidth conf
    progressPercentage =
      if progress == 0
      then "     "
      else
        (pretty $ justifyRight 3 ' ' $ T.pack $
          showFFloat (Just 0) (progress * 100) "")
        <+> "%"
  in
    (fill maxTagLength $ pretty tag)
    <++> (pretty $ justifyRight (T.length "open") ' ' $ show open_count)
    <++> (pretty $ justifyRight (T.length "closed") ' ' $ show closed_count)
    <++> progressPercentage <+> (getProgressBar barWidth progress)



listTags :: Connection -> IO (Doc AnsiStyle)
listTags connection = do
  tags <- query_ connection $ Query "select * from tags"

  let
    percWidth = 6  -- Width of e.g. 100 %
    progressWith = (progressBarWidth conf) + percWidth
    firstOf4 = \(a, _, _, _) -> a
    maxTagLength = tags
      <&> (T.length . firstOf4)
      & P.maximum

  pure $
         (annotate (bold <> underlined) $ fill maxTagLength "Tag")
    <++> (annotate (bold <> underlined) $ "Open")
    <++> (annotate (bold <> underlined) $ "Closed")
    <++> (annotate (bold <> underlined) $ fill progressWith "Progress")
    <> line
    <> (vsep $ fmap (formatTagLine maxTagLength) tags)

