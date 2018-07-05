module Lib where

import Protolude as P

import Data.Hourglass
import Codec.Crockford as Crock
import Data.Text as T
import Data.ULID
import Database.Beam
import Database.Beam.Sqlite
import Database.Beam.Schema.Tables
import Database.SQLite.Simple as Sql
import Numeric
import System.Directory
import System.Process (readProcess)
import qualified Text.Fuzzy as Fuzzy
import Time.System
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Text.Prettyprint.Doc.Util
import Data.Text.Prettyprint.Doc.Render.Terminal
import Unsafe (unsafeHead)

import Utils
import Task as Task
import FullTask as FullTask
import Note as Note
import TaskToNote as TaskToNote
import TaskToTag as TaskToTag
import DbSetup
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


-- | Record for storing entries of the `tasks_view` table
-- TODO: Use Beam instead of SQLite.Simple
data TaskView f = TaskView
  { _tvTask :: TaskT f
  , _tvTag :: TaskToTagT f
  } deriving Generic
instance Beamable TaskView


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
          { _ttTaskUlid = TaskUlid (fieldNamed "task_ulid") }
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
  connection <- open $ (getMainDir homeDir) <> "/" <> (dbName conf)

  createTaskTable connection
  createTagsTable connection
  createNotesTable connection

  createTaskView connection
  createTagsView connection

  return connection


execWithConn :: (Connection -> IO a) -> IO a
execWithConn func = do
  homeDir <- getHomeDirectory
  createDirectoryIfMissing True $ getMainDir homeDir
  withConnection
    ((getMainDir homeDir) <> "/" <> (dbName conf))
    (\connection -> do
        createTaskTable connection
        func connection
    )


insertTask :: Connection -> Task -> IO ()
insertTask connection task = do
  runBeamSqlite connection $ runInsert $
    insert (_tldbTasks taskLiteDb) $
    insertValues [task]


insertTags :: Connection -> TaskUlid -> [Text] -> IO ()
insertTags connection primKey tags = do
  taskToTags <- forM tags $ \tag -> do
    tagUlid <- fmap (toLower . show) getULID
    pure $ TaskToTag tagUlid primKey tag

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


formatElapsed :: IO Elapsed -> IO Text
formatElapsed =
  fmap (pack . (timePrint $ utcFormat conf))


formatUlid :: IO ULID -> IO Text
formatUlid =
  fmap (toLower . show)


parseTaskBody :: [Text] -> (Text, [Text])
parseTaskBody bodyWords =
  let
    -- Handle case when word is actually a text
    bodyWordsReversed = bodyWords & T.unwords & T.words & P.reverse
    body = bodyWordsReversed
      & P.dropWhile ("+" `T.isPrefixOf`)
      & P.reverse
      & unwords
    tags = bodyWordsReversed
      & P.takeWhile ("+" `T.isPrefixOf`)
      <&> T.replace "+" ""
      & P.reverse
  in
    (body, tags)


addTask :: Connection -> [Text] -> IO (Doc AnsiStyle)
addTask connection bodyWords = do
  ulid <- formatUlid getULID
  modified_utc <- formatElapsed timeCurrent
  let
    (body, tags) = parseTaskBody bodyWords
    task = Task
      { state = Open
      , due_utc = Nothing
      , closed_utc = Nothing
      , priority_adjustment = Nothing
      , metadata = Nothing
      , ..
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
  modified_utc <- formatElapsed timeCurrent
  let
    (body, tags) = parseTaskBody bodyWords
    task = Task
      { state = Done
      , due_utc = Nothing
      , closed_utc = Just modified_utc
      , priority_adjustment = Nothing
      , metadata = Nothing
      , ..
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


setStateAndClosed :: Connection -> TaskUlid -> TaskState -> IO ()
setStateAndClosed connection taskUlid theTaskState = do
  runBeamSqlite connection $ runUpdate $
    update (_tldbTasks taskLiteDb)
      (\task -> [ (Task.state task) <-. val_  theTaskState])
      (\task -> primaryKey task ==. val_ taskUlid &&.
                (Task.state task) /=. val_ theTaskState)


doTasks :: Connection -> [Text] -> IO (Doc AnsiStyle)
doTasks connection ids = do
  docs <- forM ids $ \idSubstr -> do
    doc <- execWithId connection idSubstr $ \taskUlid@(TaskUlid idText) -> do
      setStateAndClosed connection taskUlid Done

      numOfChanges <- changes connection

      pure $ pretty $ if numOfChanges == 0
        then "⚠️  Task \"…" <> idText <> "\" is already done"
        else "✅ Finished task \"…" <> idText <> "\""
    pure doc
  pure $ vsep docs


endTasks :: Connection -> [Text] -> IO (Doc AnsiStyle)
endTasks connection ids = do
  docs <- forM ids $ \idSubstr -> do
    doc <- execWithId connection idSubstr $ \taskUlid@(TaskUlid idText) -> do
      setStateAndClosed connection taskUlid Obsolete

      numOfChanges <- changes connection

      pure $ pretty $ if numOfChanges == 0
        then "⚠️  Task \"…" <> idText <> "\" is already marked as obsolete"
        else "⏹  Marked task \"…" <> idText <> "\" as obsolete"
    pure doc
  pure $ vsep docs


deleteTasks :: Connection -> [Text] -> IO (Doc AnsiStyle)
deleteTasks connection ids = do
  -- TODO: Delete corresponding tags and notes
  docs <- forM ids $ \idSubstr -> do
    doc <- execWithId connection idSubstr $ \(TaskUlid idText) -> do
      execute connection
        (Query $ "delete from `" <> tableName conf <> "` where `ulid` == ?")
        [idText :: Text]

      numOfChanges <- changes connection

      pure $ pretty $ if numOfChanges == 0
        then "⚠️ An error occured while deleting task \"…" <> idText <> "\""
        else "❌ Deleted task \"…" <> idText <> "\""
    pure doc
  pure $ vsep docs


adjustPriority :: Float -> [IdText] -> IO (Doc AnsiStyle)
adjustPriority adjustment ids  = do
  dbPath <- getDbPath
  withConnection dbPath $ \connection -> do
    docs <- forM ids $ \idSubstr -> do
      doc <- execWithId connection idSubstr $ \(TaskUlid idText) -> do
        -- TODO: Figure out why this doesn't work
        -- runBeamSqlite connection $ runUpdate $
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


infoTask :: Connection -> Text -> IO (Doc AnsiStyle)
infoTask connection idSubstr = do
  execWithId connection idSubstr $ \taskUlid -> do
    runBeamSqlite connection $ do
      task <- runSelectReturningOne $
        lookup_ (_tldbTasks taskLiteDb) taskUlid

      tags <- runSelectReturningList $ select $
        filter_ (\tag -> TaskToTag._ttTaskUlid tag ==. val_ taskUlid) $
        all_ (_tldbTaskToTag taskLiteDb)

      notes <- runSelectReturningList $ select $
        filter_ (\theNote -> TaskToNote.task_ulid theNote ==. val_ taskUlid) $
        all_ (_tldbTaskToNote taskLiteDb)

      pure $ pretty $ T.pack $
        (show task) <> (show tags) <> (show notes)


nextTask :: Connection -> IO (Doc AnsiStyle)
nextTask connection = do
  let
    -- TODO: Add "state is 'Waiting' and `wait_utc` < datetime('now')"
    selectQuery = "select * from `tasks_view` where state is 'Open'"
    orderByAndLimit = "order by `priority` desc limit 1"
  tasks <- query_ connection $ Query $ selectQuery <> orderByAndLimit

  pure $ case P.head (tasks :: [FullTask]) of
    Nothing -> pretty noTasksWarning
    Just task -> pretty task


findTask :: Text -> IO (Doc AnsiStyle)
findTask pattern = do
  connection <- setupConnection
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
      now <- fmap (pack . (timePrint $ utcFormat conf)) timeCurrent
      ulid <- fmap (toLower . show) getULID

      let taskToTag = TaskToTag ulid taskUlid tag

      runBeamSqlite connection $ runInsert $
        insert (_tldbTaskToTag taskLiteDb) $
        insertValues [taskToTag]

      runBeamSqlite connection $ runUpdate $
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
      now <- fmap (pack . (timePrint $ utcFormat conf)) timeCurrent
      ulid <- fmap (toLower . show) getULID

      let taskToNote = TaskToNote ulid taskUlid noteBody

      runBeamSqlite connection $ runInsert $
        insert (_tldbTaskToNote taskLiteDb) $
        insertValues [taskToNote]

      runBeamSqlite connection $ runUpdate $
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
      runBeamSqlite connection $ runUpdate $
        update (_tldbTasks taskLiteDb)
          (\task -> [(Task.due_utc task) <-. (val_ $ Just utcText)])
          (\task -> primaryKey task ==. val_ taskUlid)

      pure $ "📅 Set due UTC to" <+> (dquotes $ pretty utcText)
        <+> "of task" <+> (dquotes $ pretty idText)

  pure $ vsep docs


ulidToDateTime :: Text -> Maybe DateTime
ulidToDateTime =
  (fmap $
    timeGetDateTimeOfDay
    . Elapsed
    . (`div` 1000)
  )
  . Crock.decode
  . unpack
  . T.take 10


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
      (ulidToDateTime $ FullTask.ulid task)
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
      <&> timePrint (utcFormat conf)
    multilineIndent = 2
    hangWidth = taskUlidWidth + 2
      + (dateWidth conf) + 2
      + (prioWidth conf) + 2
      + multilineIndent
    taskLine = createdUtc <$$> \taskDate -> hang hangWidth $
           annotate (idStyle conf) id
      <++> annotate (priorityStyle conf) (pretty $ justifyRight 4 ' '
            $ showAtPrecision $ realToFrac
            $ fromMaybe 0 (FullTask.priority task))
      <++> annotate (dateStyle conf) (pretty taskDate)
      <++> annotate (bodyStyle conf) (reflow body)
      <++> annotate (dueStyle conf) (pretty dueUtcMaybe)
      <++> annotate (closedStyle conf) (pretty closedUtcMaybe)
      <++> hsep (tags <$$> formatTag)
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
      \select \
        \tasks_view.ulid as ulid, body, state, due_utc, closed_utc, \
        \modified_utc, tags, notes, priority, metadata \
      \from (" <> ulidsQuery <> ") tasks1 \
      \left join tasks_view on tasks1.ulid is tasks_view.ulid \
      \order by priority desc"

  tasks <- query_ connection $ Query mainQuery
  pure $ formatTasks tasks


queryTasks :: Text -> IO (Doc AnsiStyle)
queryTasks sqlQuery = do
  connection <- setupConnection
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

