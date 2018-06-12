{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Lib where

import Protolude

import Control.Category ((>>>))
import Data.Hourglass
import Codec.Crockford as Crock
import Data.Text as T
import Data.ULID
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple
import System.Directory


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


listOpenTasks :: IO ()
listOpenTasks = do
  homeDir <- getHomeDirectory
  connection <- open $ (mainDir homeDir) <> "/" <> dbName

  let
    tasksByCreationUtc =
      orderBy_
      (\task -> asc_ (_taskId task))
      (all_ (_taskTasks taskLiteDb))

  putStrLn (
    "Id   " <>
    "UTC Date   " <>
    "Body" :: [Char])

  runBeamSqlite connection $ do
    tasks <- runSelectReturningList $ select tasksByCreationUtc
    forM_ tasks $ \task ->
      let
        id = takeEnd 4 $ _taskId task
        date = fmap
          (pack . timePrint ISO8601_Date)
          (ulidToDateTime $ _taskId task)
        body = _taskBody task
        taskLine = fmap (\date ->
          id <> " " <>
          date <> " " <>
          body  <> " ")
          date
      in
        putStrLn $ fromMaybe
          ("Id \"" <> _taskId task <> "\" is an invalid ulid \
            \and could not be converted to a datetime")
          taskLine

