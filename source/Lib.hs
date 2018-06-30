module Lib where

import Protolude as P

import Data.Hourglass
import Codec.Crockford as Crock
import Data.Text as T
import qualified Data.Text.IO as T
import Data.ULID
import Database.Beam
import Database.Beam.Sqlite
import Database.Beam.Schema.Tables
import Database.SQLite.Simple as Sql
import System.Directory
import System.Process (readProcess)
import qualified Text.Fuzzy as Fuzzy
import Time.System
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Text.Prettyprint.Doc.Util
import Data.Text.Prettyprint.Doc.Render.Terminal
import Unsafe (unsafeHead)
import Utils
import qualified SqlUtils as SqlU
import Task as Task
import FullTask as FullTask
import Note as Note
import TaskToNote as TaskToNote


data Config = Config
  { tableName :: Text
  , idWidth :: Int
  , idStyle :: AnsiStyle
  , priorityStyle :: AnsiStyle
  , dateStyle :: AnsiStyle
  , bodyStyle :: AnsiStyle
  , closedStyle :: AnsiStyle
  , tagStyle :: AnsiStyle
  , utcFormat :: TimeFormatString
  , mainDir :: FilePath
  , dbName :: FilePath
  , dateWidth :: Int
  , bodyWidth :: Int
  , prioWidth :: Int
  , headCount :: Int
  , maxWidth :: Int
  }


conf :: Config
conf = Config
  { tableName = "tasks"
  , idWidth = 4
  , idStyle = color Green
  , priorityStyle = color Magenta
  , dateStyle = color Yellow
  , bodyStyle = color White
  , closedStyle = color Red
  , tagStyle = color Blue
  , utcFormat = toFormat ("YYYY-MM-DD H:MI:S" :: [Char])
  , mainDir = "tasklite"
  , dbName = "main.db"
  , dateWidth = 10
  , bodyWidth = 10
  , prioWidth = 4
  , headCount = 20
  , maxWidth = 120
  }


getMainDir :: FilePath -> FilePath
getMainDir = (<> "/" <> (mainDir conf) )


noTasksWarning :: Text
noTasksWarning = "No tasks available"


-- | Record for storing entries of the `task_to_tag` table
data TaskToTagT f = TaskToTag
  { _ttUlid :: Columnar f Text -- Ulid
  , _ttTaskUlid :: PrimaryKey TaskT f
  , _ttTag :: Columnar f Text
  } deriving Generic

type TaskToTag = TaskToTagT Identity
type TaskToTagId = PrimaryKey TaskToTagT Identity

-- FIXME: Probably doesn't work because of `PrimaryKey TaskT f`
-- deriving instance Show TaskToTag
-- deriving instance Eq TaskToTag

instance Beamable TaskToTagT

instance Table TaskToTagT where
  data PrimaryKey TaskToTagT f = TaskToTagId (Columnar f Text)
    deriving Generic
  primaryKey = TaskToTagId . _ttUlid
instance Beamable (PrimaryKey TaskToTagT)


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


createTaskTable :: Connection -> IO ()
createTaskTable connection = do
  let
    theTableName = tableName conf
    -- TODO: Replace with beam-migrate based table creation
    createTableQuery = SqlU.getTable theTableName (
      "`ulid` text not null primary key" :
      "`body` text not null" :
      ("`state` text check(`state` in (" <> stateOptions
        <> ")) not null default '" <> show stateDefault <> "'") :
      "`due_utc` text" :
      "`closed_utc` text" :
      "`modified_utc` text not null" :
      "`priority_adjustment` float" :
      "`metadata` text" :
      [])

  SqlU.createTableWithQuery
    connection
    theTableName
    createTableQuery


taskViewQuery :: Text -> Query
taskViewQuery viewName =
  let
    caseStateSql = SqlU.getCase (Just "state") $ [stateDefault ..]
      & fmap (\tState -> (SqlU.getValue tState, case tState of
          Open     ->  0
          Waiting  -> -3
          Done     ->  0
          Obsolete ->  0
        ))
    caseOverdueSql = SqlU.getCase Nothing
      [ ("`due_utc` is null", 0)
      , ("`due_utc` >= datetime('now', '+1 month')", 0)
      , ("`due_utc` >= datetime('now', '+1 week')", 3)
      , ("`due_utc` >= datetime('now')", 6)
      , ("`due_utc` < datetime('now')", 9)
      ]
    selectQuery = SqlU.getSelect
      (
        "`tasks`.`ulid` as `ulid`" :
        "`tasks`.`body` as `body`" :
        "`tasks`.`state` as `state`" :
        "`tasks`.`due_utc` as `due_utc`" :
        "`tasks`.`closed_utc` as `closed_utc`" :
        "`tasks`.`modified_utc`as `modified_utc`" :
        "group_concat(distinct `task_to_tag`.`tag`) as `tags`" :
        "group_concat(distinct `task_to_note`.`note`) as `notes`" :
        "ifnull(`tasks`.`priority_adjustment`, 0.0)\n\
        \  + " <> caseStateSql <> "\n\
        \  + " <> caseOverdueSql <> "\n\
        \  + case count(`task_to_note`.`note`)\n\
        \      when 0 then 0.0\n\
        \      else 1.0\n\
        \    end\n\
        \  + case count(`task_to_tag`.`tag`)\n\
        \      when 0 then 0.0\n\
        \      else 2.0\n\
        \    end\n\
        \as `priority`" :
        "`tasks`.`metadata`as `metadata`" :
        []
      )
      ("`" <> tableName conf <> "` \n\
        \left join task_to_tag on tasks.ulid = task_to_tag.task_ulid \n\
        \left join task_to_note on tasks.ulid = task_to_note.task_ulid \n\
        \")
      "`tasks`.`ulid`"
  in
    SqlU.getView viewName selectQuery


createTaskView :: Connection -> IO ()
createTaskView connection = do
  let
    viewName = "tasks_view"

  SqlU.createTableWithQuery
    connection
    viewName
    (taskViewQuery viewName)

  -- | Update `modified_utc` whenever a task is updated
  -- | (and `modified_utc` itselft isn't changed)
  execute_ connection $ SqlU.createTriggerAfterUpdate "set_modified_utc" "tasks"
    "`new`.`modified_utc` is `old`.`modified_utc`"
    "\
      \update `tasks`\n\
      \set `modified_utc` = datetime('now')\n\
      \where `ulid` = `new`.`ulid`\n\
      \"

  execute_ connection $ SqlU.createTriggerAfterUpdate "set_closed_utc" "tasks"
    "`new`.`state` is 'Done' or `new`.`state` is 'Obsolete'"
    "\
      \update `tasks`\n\
      \set `closed_utc` = datetime('now')\n\
      \where `ulid` = `new`.`ulid`\n\
      \"


createTagsTable :: Connection -> IO ()
createTagsTable connection = do
  let
    theTableName = "task_to_tag"
    createTableQuery = SqlU.getTable theTableName (
      "`ulid` text not null primary key" :
      "`task_ulid` text not null" :
      "`tag` text not null" :
      "foreign key(`task_ulid`) references `" <> tableName conf <> "`(`ulid`)" :
      "constraint `no_duplicate_tags` unique (`task_ulid`, `tag`) " :
      [])

  SqlU.createTableWithQuery
    connection
    theTableName
    createTableQuery


createNotesTable :: Connection -> IO ()
createNotesTable connection = do
  let
    theTableName = "task_to_note"
    createTableQuery = SqlU.getTable theTableName (
      "`ulid` text not null primary key" :
      "`task_ulid` text not null" :
      "`note` text not null" :
      "foreign key(`task_ulid`) references `" <> tableName conf <> "`(`ulid`)" :
      [])

  SqlU.createTableWithQuery
    connection
    theTableName
    createTableQuery


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


insertTask :: Connection -> Task -> IO (Doc AnsiStyle)
insertTask connection task = do
  runBeamSqlite connection $ runInsert $
    insert (_tldbTasks taskLiteDb) $
    insertValues [task]

  pure $ pretty $
    "🆕 Added task \"" <> (Task.body task)
    <> "\" with ulid \"" <> (Task.ulid task) <> "\""


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


addTask :: [Text] -> IO (Doc AnsiStyle)
addTask bodyWords = do
  connection <- setupConnection
  ulid <- fmap (toLower . show) getULID
  modified_utc <- fmap (pack . (timePrint $ utcFormat conf)) timeCurrent
  let
    -- Handle case when word is actually a text
    bodyWordsReversed = bodyWords & T.unwords & T.words & P.reverse
    tags = bodyWordsReversed
      & P.takeWhile ("+" `T.isPrefixOf`)
      <&> T.replace "+" ""
      & P.reverse
    body = bodyWordsReversed
      & P.dropWhile ("+" `T.isPrefixOf`)
      & P.reverse
      & unwords
    task = Task
      { state = Open
      , due_utc = Nothing
      , closed_utc = Nothing
      , priority_adjustment = Nothing
      , metadata = Nothing
      , ..
      }

  insertTags connection (primaryKey task) tags
  insertTask connection task


newtype NumRows = NumRows Integer
  deriving (Eq, Ord, Read, Show)

instance FromRow NumRows where
  fromRow = NumRows <$> field


execWithId ::
  Connection -> Text -> (TaskUlid -> IO (Doc AnsiStyle)) -> IO (Doc AnsiStyle)
execWithId connection idSubstr callback = do
  tasks <- (query connection
      (Query $ "select * from " <> tableName conf <> " where `ulid` like ?")
      ["%"  <> idSubstr :: Text]
    ) :: IO [Task]

  let numOfTasks = P.length tasks

  if
    | numOfTasks == 0 ->
        pure $ pretty $ "⚠️  Task \"…" <> idSubstr <> "\" does not exist"
    | numOfTasks == 1 ->
        callback $ primaryKey $ unsafeHead tasks
    | numOfTasks > 1 ->
        pure $ pretty $ "⚠️  Id slice \"" <> idSubstr <> "\" is not unique. \
          \Please use a longer slice!"


setStateAndClosed :: Connection -> TaskUlid -> TaskState -> IO ()
setStateAndClosed connection taskUlid theTaskState = do
  runBeamSqlite connection $ runUpdate $
    update (_tldbTasks taskLiteDb)
      (\task -> [ (Task.state task) <-. val_  theTaskState])
      (\task -> primaryKey task ==. val_ taskUlid &&.
                (Task.state task) /=. val_ theTaskState)


doTask :: Text -> IO (Doc AnsiStyle)
doTask idSubstr = do
  dbPath <- getDbPath
  withConnection dbPath $ \connection -> do
    execWithId connection idSubstr $ \taskUlid@(TaskUlid idText) -> do
      setStateAndClosed connection taskUlid Done

      numOfChanges <- changes connection

      pure $ pretty $ if numOfChanges == 0
        then "⚠️  Task \"…" <> idText <> "\" is already done"
        else "✅ Finished task \"…" <> idText <> "\""


endTask :: Text -> IO (Doc AnsiStyle)
endTask idSubstr = do
  dbPath <- getDbPath
  withConnection dbPath $ \connection -> do
    execWithId connection idSubstr $ \taskUlid@(TaskUlid idText) -> do
      setStateAndClosed connection taskUlid Obsolete

      numOfChanges <- changes connection

      pure $ pretty $ if numOfChanges == 0
        then "⚠️  Task \"…" <> idText <> "\" is already marked as obsolete"
        else "⏹  Marked task \"…" <> idText <> "\" as obsolete"


deleteTask :: Text -> IO (Doc AnsiStyle)
deleteTask idSubstr = do
  -- TODO: Delete corresponding tags and notes
  dbPath <- getDbPath
  withConnection dbPath $ \connection -> do
    execWithId connection idSubstr $ \(TaskUlid idText) -> do
      execute connection
        (Query $ "delete from `" <> tableName conf <> "` where `ulid` == ?")
        [idText :: Text]

      numOfChanges <- changes connection

      pure $ pretty $ if numOfChanges == 0
        then "⚠️ An error occured while deleting task \"…" <> idText <> "\""
        else "❌ Deleted task \"…" <> idText <> "\""


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


infoTask :: Text -> IO (Doc AnsiStyle)
infoTask idSubstr = do
  dbPath <- getDbPath
  withConnection dbPath $ \connection -> do
    execWithId connection idSubstr $ \(TaskUlid idText) -> do
      tasks <- query connection
        (Query $ "select * from `tasks_view` where `ulid` == ?")
        [idText :: Text]

      pure $ case P.head (tasks :: [FullTask]) of
        Nothing -> pretty
          ("This case should already be handled by `execWithId`" :: Text)
        Just task -> pretty task


nextTask :: IO (Doc AnsiStyle)
nextTask = do
  connection <- setupConnection
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
      (\(ulid, body, tags, notes, metadata) -> unwords
        [ ulid
        , "\n"
        , body
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


-- withConnectCont :: Text -> ContT a IO Connection
-- withConnectCont dbPath =
--     ContT $ withConnection dbPath


addTag :: Text -> [IdText] -> IO (Doc AnsiStyle)
addTag tag ids = do
  dbPath <- getDbPath
  withConnection dbPath $ \connection -> do
    docs <- forM ids $ \idSubstr -> do
      doc <- execWithId connection idSubstr $ \taskUlid@(TaskUlid idText) -> do
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

        pure $ pretty $ "Add tag " <> tag <> " to task \"" <> idText <> "\""
      pure doc
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


showAtPrecision :: Float -> Text
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
    hangWidth = taskUlidWidth + 2 + (dateWidth conf) + 2 + (prioWidth conf) + 2
    taskLine = createdUtc <$$> \taskDate -> hang hangWidth $
           annotate (idStyle conf) id
      <++> annotate (priorityStyle conf) (pretty $ justifyRight 4 ' '
            $ showAtPrecision $ fromMaybe 0 (FullTask.priority task))
      <++> annotate (dateStyle conf) (pretty taskDate)
      <++> annotate (bodyStyle conf) (reflow body)
      <++> annotate (closedStyle conf) (pretty closedUtcMaybe)
      <++> hsep (tags <$$> formatTag )
  in
    fromMaybe
      ("Id" <+> (dquotes $ pretty $ FullTask.ulid task) <+>
        "is an invalid ulid and could not be converted to a datetime")
      taskLine


getIdLength :: Float -> Int
getIdLength numOfItems =
  let
    targetCollisionChance = 0.01  -- Targeted likelihood of id collisions
    sizeOfAlphabet = 32  -- Crockford's base 32 alphabet
  in
    ceiling $
      log (numOfItems / targetCollisionChance) / log sizeOfAlphabet


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


headTasks :: IO (Doc AnsiStyle)
headTasks = do
  connection <- setupConnection
  let
    -- TODO: Add "state is 'Waiting' and `wait_utc` < datetime('now')"
    selectQuery = "select * from `tasks_view` where state is 'Open'"
    orderByAndLimit = "order by `priority` desc limit " <> show (headCount conf)
  tasks <- query_ connection $ Query $ selectQuery <> orderByAndLimit
  pure $ formatTasks tasks


newTasks :: IO (Doc AnsiStyle)
newTasks = do
  connection <- setupConnection
  let
    -- TODO: Add "state is 'Waiting' and `wait_utc` < datetime('now')"
    selectQuery = "select * from `tasks_view` where state is 'Open'"
    orderByAndLimit = "order by `ulid` desc limit " <> show (headCount conf)
  tasks <- query_ connection $ Query $ selectQuery <> orderByAndLimit
  pure $ formatTasks tasks


listTasks :: Filter TaskState -> IO (Doc AnsiStyle)
listTasks taskState = do
  connection <- setupConnection

  let
    selectQuery = "select * from `tasks_view`"
    orderBy = "order by `priority` desc"

  tasks <- case taskState of
    NoFilter          -> query_ connection (selectQuery <> orderBy)
    Utils.Only tState -> query connection
      (selectQuery <> " where `state` == ? " <> orderBy)
      [tState]

  pure $ formatTasks tasks


queryTasks :: Text -> IO (Doc AnsiStyle)
queryTasks query = do
  connection <- setupConnection
  tasks <- query_ connection $ Query $
    "select * from `tasks_view` where " <> query
  pure $ formatTasks tasks


runSql :: Text -> IO (Doc AnsiStyle)
runSql query = do
  homeDir <- getHomeDirectory
  result <- readProcess "sqlite3"
    [ (getMainDir homeDir) <> "/" <> (dbName conf)
    , ".headers on"
    , ".mode csv"
    , ".separator , '\n'"
    , T.unpack query
    ]
    []
  pure $ pretty result


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
