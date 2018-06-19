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


module Lib where

import Protolude as P

import Data.Aeson as Aeson
import Data.Hourglass
import Codec.Crockford as Crock
import Data.Csv as Csv
import Data.Text as T
import Data.ULID
import Database.Beam
import Database.Beam.Sqlite
import Database.Beam.Schema.Tables
import Database.SQLite.Simple as Sql
import Database.SQLite.Simple.FromField as Sql.FromField
import Database.SQLite.Simple.ToField as Sql.ToField
import Database.SQLite.Simple.Internal hiding (result)
import Database.SQLite.Simple.Ok
import System.Directory
import Time.System
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Text.Prettyprint.Doc.Render.Terminal
import Utils


data Config = Config
  { tableName :: Text
  , idWidth :: Int
  , idStyle :: AnsiStyle
  , dateStyle :: AnsiStyle
  , bodyStyle :: AnsiStyle
  , closeStyle :: AnsiStyle
  , tagStyle :: AnsiStyle
  }


conf :: Config
conf = Config
  { tableName = "tasks"
  , idWidth = 4
  , idStyle = color Green
  , dateStyle = color Yellow
  , bodyStyle = color White
  , closeStyle = color Red
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
  toField state = SQLText $ show state


newtype Ulid = Ulid Text

data TaskT f = Task
  { _taskId :: Columnar f Text -- Ulid
  , _taskBody :: Columnar f Text
  , _taskState :: Columnar f Text -- TaskState
  , _taskDueUtc :: Columnar f Text
  , _taskCloseUtc :: Columnar f Text
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
    <*> field <*> field


data FullTask = FullTask
  { _ftId :: Text -- Ulid
  , _ftBody :: Text
  , _ftState :: Text -- TaskState
  , _ftDueUtc :: Maybe Text
  , _ftCloseUtc :: Maybe Text
  , _ftTags :: Maybe [Text]
  } deriving Generic

-- For conversion from SQLite with SQLite.Simple
instance FromRow FullTask where
  fromRow = FullTask
    <$> field <*> field <*> field
    <*> field <*> field <*> field

instance Sql.FromField.FromField [Text] where
  fromField f@(Field (SQLText txt) _) = Ok $ split (== ',') txt
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


createTable :: Connection -> Text -> Query -> IO ()
createTable connection aTableName query = do
  result <- try $ execute_ connection query

  case result :: Either SQLError () of
    Left error ->
      if isSuffixOf "already exists" (sqlErrorDetails error)
      then return ()
      else print error
    Right _ ->
      putText $ "🆕 Create table \"" <> aTableName <> "\""


createTaskTable :: Connection -> IO ()
createTaskTable connection = do
  let
    theTableName = tableName conf
    stateDefault = (toEnum 0) :: TaskState
    stateOptions = T.intercalate "," $
      fmap (("'" <>) . (<> "'") . show) [stateDefault ..]
    -- TODO: Replace with beam-migrate based table creation
    createTableQuery = Query $ "\
      \create table `" <> theTableName <> "` (\
        \`id` text not null, \
        \`body` text not null, \
        \`state` text check(`state` in (" <> stateOptions <>
          ")) not null default '" <> (show stateDefault) <> "', \
        \`due_utc` text not null, \
        \`close_utc` text not null, \
        \primary key(`id`)\
      \)"

  createTable
    connection
    theTableName
    createTableQuery


createTaskView :: Connection -> IO ()
createTaskView connection = do
  let
    viewName = "tasks_view"
    createTableQuery = Query $ "\
      \create view `" <> viewName <> "` as \
      \select \
        \tasks.id as id, \
        \tasks.body as body, \
        \tasks.state as state, \
        \tasks.due_utc as due_utc, \
        \tasks.close_utc as close_utc, \
        \group_concat(task_to_tag.tag, ',') as tags \
      \from `" <> tableName conf <> "` \
        \left join `task_to_tag` on tasks.id = task_to_tag.task_id \
      \group by tasks.id \
      \"

  createTable
    connection
    viewName
    createTableQuery



createTagsTable :: Connection -> IO ()
createTagsTable connection = do
  -- TODO: Replace with beam-migrate based table creation
  let
    theTableName = "task_to_tag"
    createTableQuery = Query $ "\
      \create table `" <> theTableName <> "` ( \
        \`id` text not null, \
        \`task_id` text not null, \
        \`tag` text not null, \
        \primary key(`id`), \
        \foreign key(task_id) references " <> tableName conf <> "(id) \
      \)"

  createTable
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
  let
    fragments = splitOn " +" bodyAndTags
    body = fromMaybe "" $ headMay fragments
    tags = fromMaybe [] $ tailMay fragments
    task = Task ulid body (show Open) "" ""

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


execIfIdExists :: Connection -> Text -> (IO () -> IO ())
execIfIdExists connection idSubstr callback = do
  [NumRows numOfRows] <- query connection
    (Query $ "select count(*) from " <> tableName conf <> " where `id` like ?")
    ["%"  <> idSubstr :: Text]

  if
    | numOfRows > 1 ->
        putText $ "⚠️  Id slice \"" <> idSubstr <> "\" is not unique. \
          \Please use a longer slice!"
    | numOfRows == 0 ->
        putText $ "⚠️  Task \"…" <> idSubstr <> "\" does not exist"
    | otherwise -> callback


setStateAndClose :: Connection -> Text -> TaskState -> IO ()
setStateAndClose connection idSubstr theTaskState = do
  now <- fmap (timePrint ISO8601_DateAndTime) timeCurrent
  execute connection
    (Query $
      "update `" <> tableName conf <> "` \
      \set \
        \`state` = ? ,\
        \`close_utc` = ? \
      \where `id` like ? and `state` != ?")
    ( (show theTaskState) :: Text
    , now
    , "%" <> idSubstr
    , (show theTaskState) :: Text
    )


doTask :: Text -> IO ()
doTask idSubstr = do
  dbPath <- getDbPath
  withConnection dbPath $ \connection -> do
    execIfIdExists connection idSubstr $ do
      setStateAndClose connection idSubstr Done

      numOfChanges <- changes connection

      if numOfChanges == 0
      then putText $ "⚠️  Task \"…" <> idSubstr <> "\" is already done"
      else putText $ "✅ Finished task \"…" <> idSubstr <> "\""


endTask :: Text -> IO ()
endTask idSubstr = do
  dbPath <- getDbPath
  withConnection dbPath $ \connection -> do
    execIfIdExists connection idSubstr $ do
      setStateAndClose connection idSubstr Obsolete

      numOfChanges <- changes connection

      if numOfChanges == 0
      then putText $ "⚠️  Task \"…" <> idSubstr <>
        "\" is already marked as obsolete"
      else putText $ "⏹  Marked task \"…" <> idSubstr <> "\" as obsolete"


deleteTask :: Text -> IO ()
deleteTask idSubstr = do
  dbPath <- getDbPath
  withConnection dbPath $ \connection -> do
    execIfIdExists connection idSubstr $ do
      execute connection
        (Query $ "delete from `" <> tableName conf <> "` where `id` like ?")
        ["%" <> idSubstr]

      numOfChanges <- changes connection

      putText $ if numOfChanges == 0
        then "⚠️ An error occured while deleting task \"…" <> idSubstr <> "\""
        else "❌ Deleted task \"…" <> idSubstr <> "\""


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
        <++> annotate (dateStyle conf) (pretty taskDate)
        <++> annotate (bodyStyle conf) (pretty body)
        <++> annotate (closeStyle conf) (pretty $ _ftCloseUtc task)
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
      docHeader = (annotate (idStyle conf <> strong) $ fill taskIdWidth "Id")
        <++> (annotate (dateStyle conf <> strong) $
          fill dateWidth "Opened UTC")
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
