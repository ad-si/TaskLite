{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module SqlUtils where

import Protolude as P

import Data.Text as T
import Database.SQLite.Simple as Sql


newtype SqlQuery = SqlQuery Text
  deriving Show


getValue :: Show a => a -> Text
getValue value =
  "'" <> show value <> "'"


getTable :: Text -> [Text] -> Query
getTable tableName columns = Query $ T.unlines (
  "create table `" <> tableName <> "` (" :
  (T.intercalate ",\n" columns) :
  ");" :
  [])


getColumns :: Text -> [Text] -> SqlQuery
getColumns tableName columns  = SqlQuery $ unlines $ (
  "select" :
  "  " <> T.intercalate ",\n  " columns <> "\n" :
  "from `" <> tableName <> "`;" :
  [])


getSelect :: [Text] -> Text -> Text -> Query
getSelect selectLines fromStatement groupByColumn = Query $ T.unlines (
  "select" :
  (T.intercalate ",\n" selectLines) :
  "from" :
  fromStatement :
  "group by " <> groupByColumn <> ";":
  [])


getView :: Text -> Query -> Query
getView viewName selectQuery = Query $ T.unlines (
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


getCase :: Maybe Text -> [(Text, Float)] -> Text
getCase fieldNameMaybe valueMap =
  "case "
  <> case fieldNameMaybe of
      Nothing -> ""
      Just fName -> "`" <> fName <> "`"
  <> (P.fold $ fmap
        (\(key, val) -> "when " <> key <> " then " <> show val <> " ")
        valueMap)
  <> " end "


createTriggerAfterUpdate :: Text -> Text -> Text -> Text -> Query
createTriggerAfterUpdate name tableName whenBlock body = Query $ "\
    \create trigger if not exists `" <> name <> "_after_update`\n\
    \after update on `" <> tableName <> "`\n\
    \when " <> whenBlock <> "\n\
    \begin\n\
    \  " <> body <> ";\n\
    \end;\n\
    \"
