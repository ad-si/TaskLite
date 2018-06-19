{-# LANGUAGE OverloadedStrings #-}


module SqlUtils where

import Protolude as P

import Data.Text as T
import Database.SQLite.Simple as Sql


getTableSql :: Text -> [Text] -> Query
getTableSql tableName columns = Query $ T.unlines (
  "create table `" <> tableName <> "` (" :
  (T.intercalate ",\n" columns) :
  ");" :
  [])


getSelectSql :: [Text] -> Text -> Text -> Query
getSelectSql selectLines fromStatement groupByColumn = Query $ T.unlines (
  "select" :
  (T.intercalate ",\n" selectLines) :
  "from" :
  fromStatement :
  "group by " <> groupByColumn <> ";":
  [])


getViewSql :: Text -> Query -> Query
getViewSql viewName selectQuery = Query $ T.unlines (
  "create view `" <> viewName <> "` as" :
  fromQuery selectQuery :
  [])


createTableWithQuery :: Connection -> Text -> Query -> IO ()
createTableWithQuery connection aTableName theQuery = do
  result <- try $ execute_ connection theQuery

  case result :: Either SQLError () of
    Left errorMessage ->
      if isSuffixOf "already exists" (sqlErrorDetails errorMessage)
      then return ()
      else P.print errorMessage
    Right _ ->
      putText $ "🆕 Create table \"" <> aTableName <> "\""
