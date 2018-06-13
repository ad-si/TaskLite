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

import Protolude

import Data.Hourglass
import Codec.Crockford as Crock
import Data.Text as T
import Data.ULID
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple
import System.Directory
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Text.Prettyprint.Doc.Render.Terminal


data Config = Config
  { idWidth :: Int
  , idStyle :: AnsiStyle
  , dateStyle :: AnsiStyle
  , bodyStyle :: AnsiStyle
  }


conf :: Config
conf = Config
  { idWidth = 4
  , idStyle = color Green
  , dateStyle = color Yellow
  , bodyStyle = color White
  }


data TaskState
  = Open
  | Waiting
  | Done
  | Obsolete
  deriving (Eq, Show)


newtype Ulid = Ulid Text

data TaskT f = Task
  { _taskId :: Columnar f Text -- Ulid
  , _taskBody :: Columnar f Text
  , _taskState :: Columnar f Text -- TaskState
  , _taskDueDate :: Columnar f Text
  , _taskEndDate :: Columnar f Text
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


setupConnection :: IO Connection
setupConnection = do
  homeDir <- getHomeDirectory
  createDirectoryIfMissing True $ mainDir homeDir

  connection <- open $ (mainDir homeDir) <> "/" <> dbName

  -- TODO: Replace with beam-migrate based table creation
  execute_ connection "\
    \create table if not exists 'tasks' (\
      \id varchar not null, \
      \body varchar not null, \
      \state varchar not null, \
      \due_date varchar not null, \
      \end_date varchar not null, \
      \primary key( id )\
    \)"

  return connection


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
    "select count(*) from `tasks` where `id` like ?"
    ["%"  <> idSubstr :: Text] :: IO [NumRows]

  if
    | numOfRows > 1 ->
        putText $ "Id slice \"" <> idSubstr <> "\" is not unique. \
          \Please use a longer slice!"
    | numOfRows == 0 ->
        putText $ "Task \"…" <> idSubstr <> "\" does not exist"
    | otherwise -> do
        execute connection
          "update `tasks` set `state` = ? where `id` like ? and `state` != ?"
          ((show Done) :: Text, "%" <> idSubstr, (show Done) :: Text)

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
        <+> annotate (dateStyle conf) (pretty taskDate)
        <+> annotate (bodyStyle conf) (pretty body)
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


listOpenTasks :: IO ()
listOpenTasks = do
  homeDir <- getHomeDirectory
  connection <- open $ (mainDir homeDir) <> "/" <> dbName

  let
    tasksByCreationUtc =
      orderBy_
      (\task -> asc_ (_taskId task))
      (all_ (_taskTasks taskLiteDb))
    dateWidth = 10
    bodyWidth = 10
    strong = bold <> underlined


  runBeamSqlite connection $ do
    tasks <- runSelectReturningList $ select tasksByCreationUtc

    let
      taskIdWidth = getIdLength $ fromIntegral $ Protolude.length tasks
      docHeader =
        (annotate (idStyle conf <> strong) $ fill taskIdWidth "Id") <+>
        (annotate (dateStyle conf <> strong) $ fill dateWidth "UTC-Date") <+>
        (annotate (bodyStyle conf <> strong) $ fill bodyWidth "Body") <+>
        line

    liftIO $ putDoc $
      docHeader <>
      (vsep $ fmap (formatTaskLine taskIdWidth) tasks) <>
      line

