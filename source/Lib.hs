{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Lib where

import Protolude as P

import Data.Aeson as Aeson
import Data.Hourglass
import Codec.Crockford as Crock
import Data.Csv as Csv
import Data.Text as T
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
import Data.Text.Prettyprint.Doc.Render.Terminal
import Unsafe (unsafeHead)
import Utils
import qualified SqlUtils as SqlU


data Config = Config
  { tableName :: Text
  , idWidth :: Int
  , idStyle :: AnsiStyle
  , priorityStyle :: AnsiStyle
  , dateStyle :: AnsiStyle
  , bodyStyle :: AnsiStyle
  , closedStyle :: AnsiStyle
  , tagStyle :: AnsiStyle
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
  }


data TaskState
  = Open
  | Waiting
  | Done
  | Obsolete
  deriving (Eq, Enum, Ord, Read, Show)

instance Sql.FromField.FromField TaskState where
  fromField f@(Field (SQLText txt) _) = case txt of
    "Open" -> Ok Open
    "Waiting" -> Ok Waiting
    "Done" -> Ok Done
    "Obsolete" -> Ok Obsolete
    _ -> returnError ConversionFailed f "expecting a valid TaskState"
  fromField f = returnError ConversionFailed f "expecting SQLText column type"

instance Sql.ToField.ToField TaskState where
  toField = SQLText . show

instance HasSqlValueSyntax be [Char] => HasSqlValueSyntax be TaskState where
  sqlValueSyntax = autoSqlValueSyntax

instance HasSqlEqualityCheck SqliteExpressionSyntax TaskState

stateDefault :: TaskState
stateDefault = (toEnum 0) :: TaskState

stateOptions :: Text
stateOptions = T.intercalate "," $
  fmap (("'" <>) . (<> "'") . show) [stateDefault ..]


newtype Ulid = Ulid Text

data TaskT f = Task
  { _taskId :: Columnar f Text -- Ulid
  , _taskBody :: Columnar f Text
  , _taskState :: Columnar f TaskState
  , _taskDueUtc :: Columnar f (Maybe Text)
  , _taskClosedUtc :: Columnar f (Maybe Text)
  , _taskModifiedUtc :: Columnar f Text
  , _taskPriorityAdjustment :: Columnar f (Maybe Float)
  } deriving Generic


-- Beam related instances
type Task = TaskT Identity
type TaskId = PrimaryKey TaskT Identity

deriving instance Show Task
deriving instance Eq Task

instance Beamable TaskT

instance Table TaskT where
  data PrimaryKey TaskT f = TaskId (Columnar f Text) deriving Generic
  primaryKey = TaskId . _taskId
instance Beamable (PrimaryKey TaskT)

-- For conversion from SQLite with SQLite.Simple
instance FromRow Task where
  fromRow = Task
    <$> field <*> field <*> field
    <*> field <*> field <*> field
    <*> field


-- | Final user-facing format of tasks
data FullTask = FullTask
  { _ftId :: Text -- Ulid
  , _ftBody :: Text
  , _ftState :: Text -- TaskState
  , _ftDueUtc :: Maybe Text
  , _ftClosedUtc :: Maybe Text
  , _ftModifiedUtc :: Text
  , _ftTags :: Maybe [Text]
  , _ftPriority :: Maybe Float
  } deriving Generic


-- For conversion from SQLite with SQLite.Simple
instance FromRow FullTask where
  fromRow = FullTask
    <$> field <*> field <*> field
    <*> field <*> field <*> field
    <*> field <*> field

instance Sql.FromField.FromField [Text] where
  fromField (Field (SQLText txt) _) = Ok $ split (== ',') txt
  fromField f = returnError ConversionFailed f "expecting SQLText column type"


instance Csv.ToField [Text] where
  toField = encodeUtf8 . (T.intercalate ",")

-- For conversion to CSV
instance ToRecord FullTask
instance ToNamedRecord FullTask
instance DefaultOrdered FullTask

-- For conversion to JSON
instance ToJSON FullTask


data TaskToTagT f = TaskToTag
  { _ttId :: Columnar f Text -- Ulid
  , _ttTaskId :: PrimaryKey TaskT f
  , _ttTag :: Columnar f Text
  } deriving Generic

type TaskToTag = TaskToTagT Identity
type TaskToTagId = PrimaryKey TaskToTagT Identity

-- FIXME: Probably doesn't work because of `PrimaryKey TaskT f`
-- deriving instance Show TaskToTag
-- deriving instance Eq TaskToTag

instance Beamable TaskToTagT

instance Table TaskToTagT where
  data PrimaryKey TaskToTagT f = TaskToTagId (Columnar f Text) deriving Generic
  primaryKey = TaskToTagId . _ttId
instance Beamable (PrimaryKey TaskToTagT)


-- TODO: Use Beam instead of SQLite.Simple
data TaskView f = TaskView
  { _tvTask :: TaskT f
  , _tvTag :: TaskToTagT f
  } deriving Generic
instance Beamable TaskView


data TaskLiteDb f = TaskLiteDb
  { _tldbTasks :: f (TableEntity TaskT)
  , _tldbTaskToTag :: f (TableEntity TaskToTagT)
  , _tldbTasksView :: f (ViewEntity TaskView)
  } deriving Generic

instance Database be TaskLiteDb


taskLiteDb :: DatabaseSettings be TaskLiteDb
taskLiteDb = defaultDbSettings `withDbModification`
  dbModification
    { _tldbTaskToTag = modifyTable identity $
        tableModification
          { _ttTaskId = TaskId (fieldNamed "task_id") }
    }


mainDir :: FilePath -> FilePath
mainDir = (<> "/tasklite")


dbName :: FilePath
dbName = "main.db"


createTaskTable :: Connection -> IO ()
createTaskTable connection = do
  let
    theTableName = tableName conf
    -- TODO: Replace with beam-migrate based table creation
    createTableQuery = SqlU.getTable theTableName (
      "`id` text not null primary key" :
      "`body` text not null" :
      ("`state` text check(`state` in (" <> stateOptions
        <> ")) not null default '" <> show stateDefault <> "'") :
      "`due_utc` text" :
      "`closed_utc` text" :
      "`modified_utc` text not null" :
      "`priority_adjustment` float" :
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
        "`tasks`.`id` as `id`" :
        "`tasks`.`body` as `body`" :
        "`tasks`.`state` as `state`" :
        "`tasks`.`due_utc` as `due_utc`" :
        "`tasks`.`closed_utc` as `closed_utc`" :
        "`tasks`.`modified_utc`as `modified_utc`" :
        "group_concat(`task_to_tag`.`tag`, ',') as `tags`" :
        "ifnull(`tasks`.`priority_adjustment`, 0.0)"
          <> " + " <> caseStateSql <> "\n"
          <> " + " <> caseOverdueSql <> "\n"
          <> " + case count(`task_to_tag`.`tag`) \
              \when 0 then 0.0 else 2.0 end" <> "\n"
          <> " as `priority`" :
        []
      )
      ("`" <> tableName conf <> "` \
        \left join `task_to_tag` on `tasks`.`id` = `task_to_tag`.`task_id`")
      "`tasks`.`id`"
    createTableQuery = SqlU.getView viewName selectQuery

  SqlU.createTableWithQuery
    connection
    viewName
    createTableQuery


createTagsTable :: Connection -> IO ()
createTagsTable connection = do
  let
    theTableName = "task_to_tag"
    createTableQuery = SqlU.getTable theTableName (
      "`id` text not null primary key" :
      "`task_id` text not null" :
      "`tag` text not null" :
      "foreign key(`task_id`) references `" <> tableName conf <> "`(`id`)" :
      "constraint `no_duplicate_tags` unique (`task_id`, `tag`) " :
      [])

  SqlU.createTableWithQuery
    connection
    theTableName
    createTableQuery


getDbPath :: IO FilePath
getDbPath = do
  homeDir <- getHomeDirectory
  pure $ (mainDir homeDir) <> "/" <> dbName


setupConnection :: IO Connection
setupConnection = do
  homeDir <- getHomeDirectory
  createDirectoryIfMissing True $ mainDir homeDir
  connection <- open $ (mainDir homeDir) <> "/" <> dbName

  createTaskTable connection
  createTagsTable connection
  createTaskView connection

  return connection


execWithConn :: (Connection -> IO a) -> IO a
execWithConn func = do
  homeDir <- getHomeDirectory
  createDirectoryIfMissing True $ mainDir homeDir
  withConnection
    ((mainDir homeDir) <> "/" <> dbName)
    (\connection -> do
        createTaskTable connection
        func connection
    )


addTask :: Text -> IO ()
addTask bodyAndTags = do
  connection <- setupConnection
  ulid <- fmap (toLower . show) getULID
  now <- fmap (timePrint ISO8601_DateAndTime) timeCurrent
  let
    fragments = splitOn " +" bodyAndTags
    body = fromMaybe "" $ headMay fragments
    tags = fromMaybe [] $ tailMay fragments
    task = Task ulid body Open Nothing Nothing (pack now) Nothing

  runBeamSqlite connection $ runInsert $
    insert (_tldbTasks taskLiteDb) $
    insertValues [task]

  taskToTags <- forM tags (\tag -> do
    tagUlid <- fmap (toLower . show) getULID
    pure $ TaskToTag tagUlid (primaryKey task) tag)

  runBeamSqlite connection $ runInsert $
    insert (_tldbTaskToTag taskLiteDb) $
    insertValues taskToTags

  putText $ "🆕 Added task \"" <> body <> "\""


newtype NumRows = NumRows Integer
  deriving (Eq, Ord, Read, Show)

instance FromRow NumRows where
  fromRow = NumRows <$> field


execWithId :: Connection -> Text -> (TaskId -> IO ()) -> IO ()
execWithId connection idSubstr callback = do
  rows <- (query connection
      (Query $ "select * from " <> tableName conf <> " where `id` like ?")
      ["%"  <> idSubstr :: Text]
    ) :: IO [Task]

  let numOfRows = P.length rows

  if
    | numOfRows == 0 ->
        putText $ "⚠️  Task \"…" <> idSubstr <> "\" does not exist"
    | numOfRows == 1 ->
        callback $ primaryKey $ unsafeHead rows
    | numOfRows > 1 ->
        putText $ "⚠️  Id slice \"" <> idSubstr <> "\" is not unique. \
          \Please use a longer slice!"


setStateAndClosed :: Connection -> TaskId -> TaskState -> IO ()
setStateAndClosed connection taskId theTaskState = do
  now <- fmap (pack . (timePrint ISO8601_DateAndTime)) timeCurrent

  runBeamSqlite connection $ runUpdate $
    update (_tldbTasks taskLiteDb)
      (\task -> [ (_taskState task) <-. val_  theTaskState
                , (_taskModifiedUtc task) <-. val_ now
                ])
      (\task -> primaryKey task ==. val_ taskId &&.
                (_taskState task) /=. val_ theTaskState)


doTask :: Text -> IO ()
doTask idSubstr = do
  dbPath <- getDbPath
  withConnection dbPath $ \connection -> do
    execWithId connection idSubstr $ \taskId@(TaskId idText) -> do
      setStateAndClosed connection taskId Done

      numOfChanges <- changes connection

      if numOfChanges == 0
      then putText $ "⚠️  Task \"…" <> idText <> "\" is already done"
      else putText $ "✅ Finished task \"…" <> idText <> "\""


endTask :: Text -> IO ()
endTask idSubstr = do
  dbPath <- getDbPath
  withConnection dbPath $ \connection -> do
    execWithId connection idSubstr $ \taskId@(TaskId idText) -> do
      setStateAndClosed connection taskId Obsolete

      numOfChanges <- changes connection

      if numOfChanges == 0
      then putText $ "⚠️  Task \"…" <> idText <>
        "\" is already marked as obsolete"
      else putText $ "⏹  Marked task \"…" <> idText <> "\" as obsolete"


deleteTask :: Text -> IO ()
deleteTask idSubstr = do
  dbPath <- getDbPath
  withConnection dbPath $ \connection -> do
    execWithId connection idSubstr $ \(TaskId idText) -> do
      execute connection
        (Query $ "delete from `" <> tableName conf <> "` where `id` == ?")
        [idText :: Text]

      numOfChanges <- changes connection

      putText $ if numOfChanges == 0
        then "⚠️ An error occured while deleting task \"…" <> idText <> "\""
        else "❌ Deleted task \"…" <> idText <> "\""


addTag :: Text -> Text -> IO ()
addTag idSubstr tag = do
  dbPath <- getDbPath
  withConnection dbPath $ \connection -> do
    execWithId connection idSubstr $ \taskId -> do
      now <- fmap (pack . (timePrint ISO8601_DateAndTime)) timeCurrent
      ulid <- fmap (toLower . show) getULID

      let taskToTag = TaskToTag ulid taskId tag

      runBeamSqlite connection $ runInsert $
        insert (_tldbTaskToTag taskLiteDb) $
        insertValues [taskToTag]

      runBeamSqlite connection $ runUpdate $
        update (_tldbTasks taskLiteDb)
          (\task -> [(_taskModifiedUtc task) <-. val_ now])
          (\task -> primaryKey task ==. val_ taskId)


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


leftFill :: Int -> Doc ann -> Doc ann
leftFill width doc =
  let numSpaces = width - (T.length $ show doc)
  in (P.fold $ P.replicate numSpaces space) <> doc


formatTaskLine :: Int -> FullTask -> Doc AnsiStyle
formatTaskLine taskIdWidth task =
  let
    id = pretty $ T.takeEnd taskIdWidth $ _ftId task
    date = fmap
      (pack . timePrint ISO8601_Date)
      (ulidToDateTime $ _ftId task)
    body = _ftBody task
    taskLine = fmap
      (\taskDate
        -> annotate (idStyle conf) id
        <++> annotate (priorityStyle conf)
              (leftFill 4 $ pretty $ fromMaybe 0 (_ftPriority task))
        <++> annotate (dateStyle conf) (pretty taskDate)
        <++> annotate (bodyStyle conf) (pretty body)
        <++> annotate (closedStyle conf) (pretty $ _ftClosedUtc task)
        <++> annotate (tagStyle conf)
              (pretty $ unwords $ fmap ("+" <>) (fromMaybe [] $ _ftTags task))
        )
      date
  in
    fromMaybe
      ("Id" <+> (dquotes $ pretty $ _ftId task) <+>
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


listTasks :: Filter TaskState -> IO ()
listTasks taskState = do
  connection <- setupConnection

  let
    dateWidth = 10
    bodyWidth = 10
    prioWidth = 4
    strong = bold <> underlined

  rows <- case taskState of
    NoFilter ->
      query_ connection ("select * from `tasks_view`") :: IO [FullTask]
    Utils.Only tState ->
      (query connection
        ("select * from `tasks_view` where `state` == ?")
        [tState]):: IO [FullTask]

  if P.length rows == 0
  then liftIO $ die "No tasks available"
  else do
    let
      taskIdWidth = getIdLength $ fromIntegral $ P.length rows
      docHeader =
             (annotate (idStyle conf <> strong) $ fill taskIdWidth "Id")
        <++> (annotate (priorityStyle conf <> strong) $ fill prioWidth "Prio")
        <++> (annotate (dateStyle conf <> strong) $ fill dateWidth "Opened UTC")
        <++> (annotate (bodyStyle conf <> strong) $ fill bodyWidth "Body")
        <++> line

    liftIO $ putDoc $
      docHeader <>
      (vsep $ fmap (formatTaskLine taskIdWidth) rows) <>
      line


dumpCsv :: IO ()
dumpCsv = do
  execWithConn $ \connection -> do
    rows <- (query_ connection "select * from tasks_view") :: IO [FullTask]

    putStrLn $ Csv.encodeDefaultOrderedByName rows


dumpNdjson :: IO ()
dumpNdjson = do
  execWithConn $ \connection -> do
    rows <- (query_ connection "select * from tasks_view") :: IO [FullTask]

    forM_ rows $ putStrLn . Aeson.encode
