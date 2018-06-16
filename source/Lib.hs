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

import Data.Hourglass
import Codec.Crockford as Crock
import Data.Text as T
import Data.ULID
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple
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
  }


conf :: Config
conf = Config
  { tableName = "tasks"
  , idWidth = 4
  , idStyle = color Green
  , dateStyle = color Yellow
  , bodyStyle = color White
  , closeStyle = color Red
  }


data TaskState
  = Open
  | Waiting
  | Done
  | Obsolete
  deriving (Eq, Enum, Show)


newtype Ulid = Ulid Text

data TaskT f = Task
  { _taskId :: Columnar f Text -- Ulid
  , _taskBody :: Columnar f Text
  , _taskState :: Columnar f Text -- TaskState
  , _taskDueUtc :: Columnar f Text
  , _taskCloseUtc :: Columnar f Text
  } deriving Generic

type Task = TaskT Identity
type TaskId = PrimaryKey TaskT Identity

deriving instance Show Task
deriving instance Eq Task

instance Beamable TaskT

instance Table TaskT where
  data PrimaryKey TaskT f = TaskId (Columnar f Text) deriving Generic
  primaryKey = TaskId . _taskId
instance Beamable (PrimaryKey TaskT)


data TaskLiteDb f = TaskLiteDb
  { _taskTasks :: f (TableEntity TaskT)
  } deriving Generic

instance Database be TaskLiteDb

taskLiteDb :: DatabaseSettings be TaskLiteDb
taskLiteDb = defaultDbSettings


mainDir :: FilePath -> FilePath
mainDir = (<> "/tasklite")


dbName :: FilePath
dbName = "main.db"


createTable :: Connection -> Text -> IO ()
createTable connection aTableName = do
  let
    stateDefault = (toEnum 0) :: TaskState
    stateOptions = Query $ T.intercalate "," $
      fmap (("'" <>) . (<> "'") . show) [stateDefault ..]
    -- TODO: Replace with beam-migrate based table creation
    createTableQuery = "\
      \create table `" <> Query aTableName <> "` (\
        \`id` text not null, \
        \`body` text not null, \
        \`state` text check(`state` in (" <> stateOptions <>
          ")) not null default '" <> (Query $ show stateDefault) <> "', \
        \`due_utc` text not null, \
        \`close_utc` text not null, \
        \primary key(`id`)\
      \)"

  result <- try $ execute_ connection createTableQuery

  case result :: Either SQLError () of
    Left error ->
      if isSuffixOf "already exists" (sqlErrorDetails error)
      then return ()
      else print error
    Right _ ->
      putStrLn $ "Create table \"" <> aTableName <> "\""


setupConnection :: IO Connection
setupConnection = do
  homeDir <- getHomeDirectory
  createDirectoryIfMissing True $ mainDir homeDir
  connection <- open $ (mainDir homeDir) <> "/" <> dbName
  createTable connection (tableName conf)
  return connection


execWithConn :: (Connection -> IO a) -> IO a
execWithConn func = do
  homeDir <- getHomeDirectory
  createDirectoryIfMissing True $ mainDir homeDir
  withConnection
    ((mainDir homeDir) <> "/" <> dbName)
    (\connection -> do
        createTable connection (tableName conf)
        func connection
    )


addTask :: Text -> IO ()
addTask body = do
  connection <- setupConnection
  ulidUpper <- getULID
  let ulid = ulidUpper & show & toLower

  runBeamSqliteDebug putStrLn connection $ runInsert $
    insert (_taskTasks taskLiteDb) $
    insertValues
      [ Task ulid body (show Open) "" "" ]

  putStrLn $ "Added task \"" <> body <> "\""


newtype NumRows = NumRows Integer
  deriving (Eq, Ord, Read, Show)

instance FromRow NumRows where
  fromRow = NumRows <$> field


closeTask :: Text -> IO ()
closeTask idSubstr = do
  homeDir <- getHomeDirectory
  connection <- open $ (mainDir homeDir) <> "/" <> dbName

  [NumRows numOfRows] <- query connection
    (Query $ "select count(*) from " <> tableName conf <> " where `id` like ?")
    ["%"  <> idSubstr :: Text]

  if
    | numOfRows > 1 ->
        putText $ "Id slice \"" <> idSubstr <> "\" is not unique. \
          \Please use a longer slice!"
    | numOfRows == 0 ->
        putText $ "Task \"…" <> idSubstr <> "\" does not exist"
    | otherwise -> do
        now <- fmap (timePrint ISO8601_DateAndTime) timeCurrent
        execute connection
          (Query $
            "update `" <> tableName conf <> "` \
            \set \
              \`state` = ? ,\
              \`close_utc` = ? \
            \where `id` like ? and `state` != ?")
          ( (show Done) :: Text
          , now
          , "%" <> idSubstr
          , (show Done) :: Text
          )

        numOfChanges <- changes connection

        if numOfChanges == 0
        then putText $ "Task \"…" <> idSubstr <> "\" is already done"
        else putText $ "Closed task \"…" <> idSubstr <> "\""


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


formatTaskLine :: Int -> TaskT Identity -> Doc AnsiStyle
formatTaskLine taskIdWidth task =
  let
    id = pretty $ T.takeEnd taskIdWidth $ _taskId task
    date = fmap
      (pack . timePrint ISO8601_Date)
      (ulidToDateTime $ _taskId task)
    body = _taskBody task
    taskLine = fmap
      (\taskDate
        -> annotate (idStyle conf) id
        <++> annotate (dateStyle conf) (pretty taskDate)
        <++> annotate (bodyStyle conf) (pretty body)
        <++> annotate (closeStyle conf) (pretty $ _taskCloseUtc task)
        )
      date
  in
    fromMaybe
      ("Id" <+> (dquotes $ pretty $ _taskId task) <+>
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

    putStrLn $ ((show taskCount) :: Text)


listTasks :: Filter TaskState -> IO ()
listTasks taskState = do
  connection <- setupConnection

  let
    dateWidth = 10
    bodyWidth = 10
    strong = bold <> underlined

  runBeamSqlite connection $ do
    tasks <- runSelectReturningList $ select $ do
      task <- orderBy_
        (\task -> asc_ (_taskId task))
        (all_ (_taskTasks taskLiteDb))
      case taskState of
        NoFilter -> return ()
        Utils.Only tState -> guard_ $ _taskState task ==. val_ (show tState)
      pure task


    if P.length tasks == 0
    then liftIO $ die "No tasks available"
    else do
      let
        taskIdWidth = getIdLength $ fromIntegral $ P.length tasks
        docHeader = (annotate (idStyle conf <> strong) $ fill taskIdWidth "Id")
          <++> (annotate (dateStyle conf <> strong) $
            fill dateWidth "Opened UTC")
          <++> (annotate (bodyStyle conf <> strong) $ fill bodyWidth "Body")
          <++> line

      liftIO $ putDoc $
        docHeader <>
        (vsep $ fmap (formatTaskLine taskIdWidth) tasks) <>
        line

