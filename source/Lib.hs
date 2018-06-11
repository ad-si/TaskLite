{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Lib where

import Protolude

import qualified Data.Text as T
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

-- instance Database be TaskLiteDb

taskLiteDb :: DatabaseSettings be TaskLiteDb
taskLiteDb = defaultDbSettings


mainDir :: FilePath -> FilePath
mainDir = (<> "/tasklite")


setupConnection :: IO Connection
setupConnection = do
  homeDir <- getHomeDirectory
  createDirectoryIfMissing True $ mainDir homeDir

  connection <- open $ (mainDir homeDir) <> "/main.db"

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
  ulid <- getULID

  runBeamSqliteDebug putStrLn connection $ runInsert $
    insert (_taskTasks taskLiteDb) $
    insertValues
      [ Task (show ulid) body (show Open) "" "" ]

  putStrLn $ "Added task \"" <> body <> "\""



