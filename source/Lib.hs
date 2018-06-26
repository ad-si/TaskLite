module Lib where

import Protolude as P

import Data.Aeson as Aeson
import Data.Hourglass
import Codec.Crockford as Crock
import Data.Csv as Csv
import Data.Text as T
import qualified Data.Text.IO as T
import Data.ULID
import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Sqlite
import Database.Beam.Schema.Tables
import Database.Beam.Sqlite.Syntax (SqliteExpressionSyntax)
import Database.SQLite.Simple as Sql
import Database.SQLite.Simple.FromField as Sql.FromField
import Database.SQLite.Simple.ToField as Sql.ToField
import Database.SQLite.Simple.Internal hiding (result)
import Database.SQLite.Simple.Ok
import System.Directory
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
  , tagStyle = color Cyan
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


createTaskView :: Connection -> IO ()
createTaskView connection = do
  let
    viewName = "tasks_view"
    caseStateSql = SqlU.getCase (Just "state") $ [stateDefault ..]
      & fmap (\tState -> (SqlU.getValue tState, case tState of
          Open     ->  0
          Waiting  -> -3
          Done     ->  0
          Obsolete ->  0
        ))
    caseOverdueSql = SqlU.getCase Nothing
      [ ("`due_utc` is null", 0)
      , ("`due_utc` >= datetime('now')", 0)
      , ("`due_utc` < datetime('now')", 10)
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
    createTableQuery = SqlU.getView viewName selectQuery

  SqlU.createTableWithQuery
    connection
    viewName
    createTableQuery

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


insertTask :: Connection -> Task -> IO ()
insertTask connection task = do
  runBeamSqlite connection $ runInsert $
    insert (_tldbTasks taskLiteDb) $
    insertValues [task]

  putText $ "🆕 Added task \"" <> (Task.body task)
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
  taskToNotes <- forM notes $ \note -> do
    pure $ TaskToNote (Note.ulid note) primKey (Note.body note)

  runBeamSqlite connection $ runInsert $
    insert (_tldbTaskToNote taskLiteDb) $
    insertValues taskToNotes


addTask :: Text -> IO ()
addTask bodyAndTags = do
  connection <- setupConnection
  ulid <- fmap (toLower . show) getULID
  now <- fmap (timePrint $ utcFormat conf) timeCurrent
  let
    fragments = splitOn " +" bodyAndTags
    body = fromMaybe "" $ headMay fragments
    tags = fromMaybe [] $ tailMay fragments
    task = Task
      { ulid = ulid
      , body = body
      , state = Open
      , due_utc = Nothing
      , closed_utc = Nothing
      , modified_utc = pack now
      , priority_adjustment = Nothing
      , metadata = Nothing
      }

  insertTags connection (primaryKey task) tags
  insertTask connection task


newtype NumRows = NumRows Integer
  deriving (Eq, Ord, Read, Show)

instance FromRow NumRows where
  fromRow = NumRows <$> field


execWithId :: Connection -> Text -> (TaskUlid -> IO ()) -> IO ()
execWithId connection idSubstr callback = do
  tasks <- (query connection
      (Query $ "select * from " <> tableName conf <> " where `ulid` like ?")
      ["%"  <> idSubstr :: Text]
    ) :: IO [Task]

  let numOfTasks = P.length tasks

  if
    | numOfTasks == 0 ->
        putText $ "⚠️  Task \"…" <> idSubstr <> "\" does not exist"
    | numOfTasks == 1 ->
        callback $ primaryKey $ unsafeHead tasks
    | numOfTasks > 1 ->
        putText $ "⚠️  Id slice \"" <> idSubstr <> "\" is not unique. \
          \Please use a longer slice!"


setStateAndClosed :: Connection -> TaskUlid -> TaskState -> IO ()
setStateAndClosed connection taskUlid theTaskState = do
  runBeamSqlite connection $ runUpdate $
    update (_tldbTasks taskLiteDb)
      (\task -> [ (Task.state task) <-. val_  theTaskState])
      (\task -> primaryKey task ==. val_ taskUlid &&.
                (Task.state task) /=. val_ theTaskState)


doTask :: Text -> IO ()
doTask idSubstr = do
  dbPath <- getDbPath
  withConnection dbPath $ \connection -> do
    execWithId connection idSubstr $ \taskUlid@(TaskUlid idText) -> do
      setStateAndClosed connection taskUlid Done

      numOfChanges <- changes connection

      if numOfChanges == 0
      then putText $ "⚠️  Task \"…" <> idText <> "\" is already done"
      else putText $ "✅ Finished task \"…" <> idText <> "\""


endTask :: Text -> IO ()
endTask idSubstr = do
  dbPath <- getDbPath
  withConnection dbPath $ \connection -> do
    execWithId connection idSubstr $ \taskUlid@(TaskUlid idText) -> do
      setStateAndClosed connection taskUlid Obsolete

      numOfChanges <- changes connection

      if numOfChanges == 0
      then putText $ "⚠️  Task \"…" <> idText <>
        "\" is already marked as obsolete"
      else putText $ "⏹  Marked task \"…" <> idText <> "\" as obsolete"


deleteTask :: Text -> IO ()
deleteTask idSubstr = do
  -- TODO: Delete corresponding tags and notes
  dbPath <- getDbPath
  withConnection dbPath $ \connection -> do
    execWithId connection idSubstr $ \(TaskUlid idText) -> do
      execute connection
        (Query $ "delete from `" <> tableName conf <> "` where `ulid` == ?")
        [idText :: Text]

      numOfChanges <- changes connection

      putText $ if numOfChanges == 0
        then "⚠️ An error occured while deleting task \"…" <> idText <> "\""
        else "❌ Deleted task \"…" <> idText <> "\""


infoTask :: Text -> IO ()
infoTask idSubstr = do
  dbPath <- getDbPath
  withConnection dbPath $ \connection -> do
    execWithId connection idSubstr $ \taskUlid@(TaskUlid idText) -> do
      tasks <- query connection
        (Query $ "select * from `tasks_view` where `ulid` == ?")
        [idText :: Text]

      case P.head (tasks :: [FullTask]) of
        Nothing -> die "This case should never be executed"
        Just task -> putDoc $ pretty $ task


addTag :: Text -> Text -> IO ()
addTag idSubstr tag = do
  dbPath <- getDbPath
  withConnection dbPath $ \connection -> do
    execWithId connection idSubstr $ \taskUlid -> do
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
    date = fmap
      (pack . timePrint ISO8601_Date)
      (ulidToDateTime $ FullTask.ulid task)
    body = FullTask.body task
    -- TODO: Move magic numbers to config
    taskLine = date <$$> \taskDate -> hang (20 + taskUlidWidth) $
           annotate (idStyle conf) id
      <++> annotate (priorityStyle conf) (pretty $ justifyRight 4 ' '
            $ showAtPrecision $ fromMaybe 0 (FullTask.priority task))
      <++> annotate (dateStyle conf) (pretty taskDate)
      <++> annotate (bodyStyle conf) (reflow body)
      <++> annotate (closedStyle conf) (pretty $ FullTask.closed_utc task)
      <++> annotate (tagStyle conf) (pretty $ unwords $
              fmap ("+" <>) (fromMaybe [] $ FullTask.tags task))
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


countTasks :: Filter TaskState -> IO ()
countTasks taskStateFilter = do
  execWithConn $ \connection -> do
    [NumRows taskCount] <- case taskStateFilter of
      NoFilter -> query_ connection $ Query $
        "select count(*) from `" <> tableName conf <> "`"
      Utils.Only taskState -> query connection
        (Query $ "select count(*) from `" <> tableName conf
          <> "` where `state` == ?")
        [(show taskState) :: Text]

    putText (show taskCount)


headTasks :: IO ()
headTasks = do
  connection <- setupConnection
  let
    -- TODO: Add "state is 'Waiting' and `wait_utc` < datetime('now')"
    selectQuery = "select * from `tasks_view` where state is 'Open'"
    orderByAndLimit = "order by `priority` desc limit " <> show (headCount conf)
  tasks <- query_ connection $ Query $ selectQuery <> orderByAndLimit

  printTasks tasks


listTasks :: Filter TaskState -> IO ()
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

  printTasks tasks


printTasks :: [FullTask] -> IO ()
printTasks tasks =
  if P.length tasks == 0
  then liftIO $ die "No tasks available"
  else do
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
      printTasks = T.putStrLn
        . renderStrict
        . layoutPretty defaultLayoutOptions
            {layoutPageWidth = AvailablePerLine (maxWidth conf) 1.0}

    liftIO $ printTasks $
      docHeader <>
      (vsep $ fmap (formatTaskLine taskUlidWidth) tasks) <>
      line
