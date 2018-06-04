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


module Main where

import Protolude

import Lib
import Database.Beam
import Database.Beam.Sqlite

import Database.SQLite.Simple

import qualified Data.Text as T
import Data.ULID

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


main :: IO ()
main = do
  connection <- open "tasklite.db"
  ulid1 <- getULID
  ulid2 <- getULID

  runBeamSqliteDebug putStrLn connection $ runInsert $
    insert (_taskTasks taskLiteDb) $
    insertValues
      [ Task (show ulid1) "Buy milk" (show Open) "2018-07-04 08:02:54" ""
      , Task (show ulid2) "Prepare a milkshake" (show Open) "2018-07-15 21:02:54" ""
      ]
