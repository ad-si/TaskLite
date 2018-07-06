{-|
Utils to simplify creation of SQL queries
-}

module SqlUtils where

import Protolude as P

import Data.Text as T
import Database.SQLite.Simple as Sql hiding (columnName)
import Data.Text.Prettyprint.Doc (Doc, pretty)
import Language.SQL.SimpleSQL.Syntax


newtype SqlQuery = SqlQuery Text
  deriving Show


id :: Name -> ValueExpr
id columnName =
  Iden [ columnName ]


ids :: [Name] -> ValueExpr
ids segments =
  Iden $ segments


tableCol :: Name -> Name -> ValueExpr
tableCol table column =
  Iden [table, column]


col :: Name -> ValueExpr
col column =
  Iden [column]


count :: ValueExpr -> ValueExpr
count column =
  App
    [ Name "count" ]
    [ column ]


ifNull :: Name -> Text -> ValueExpr
ifNull ifValue thenValue =
  App
    [ Name "ifnull" ]
    [ Iden [ifValue]
    , NumLit $ T.unpack thenValue
    ]


dot :: Name -> Name -> ValueExpr
dot item subItem =
  ids [item, subItem]


is :: ValueExpr -> ValueExpr -> ValueExpr
is exprA exprB =
  BinOp
    exprA
    [ Name "is" ]
    exprB


isNotNull :: Name -> ValueExpr
isNotNull columnName =
  PostfixOp
    [ Name "is not null" ]
    ( Iden [ columnName ] )


as :: ValueExpr -> Name -> (ValueExpr, Maybe Name)
as column aliasName@(Name theAlias) =
  ( column
  , if theAlias == ""
    then Nothing
    else Just aliasName
  )
as column otherAlias = (column, Just otherAlias)


groupBy :: ValueExpr -> GroupingExpr
groupBy = SimpleGroup


orderByAsc :: ValueExpr -> SortSpec
orderByAsc column =
  SortSpec column Asc NullsOrderDefault


orderByDesc :: ValueExpr -> SortSpec
orderByDesc column =
  SortSpec column Desc NullsOrderDefault


leftJoinOn :: Name -> Name -> ValueExpr -> TableRef
leftJoinOn tableA tableB joinOnExpr =
  TRJoin
    ( TRSimple [ tableA ] )
    False
    JLeft
    ( TRSimple [ tableB ] )
    ( Just ( JoinOn joinOnExpr ) )


leftTRJoinOn :: TableRef -> TableRef -> ValueExpr -> TableRef
leftTRJoinOn tableA tableB joinOnExpr =
  TRJoin
    tableA
    False
    JLeft
    tableB
    ( Just ( JoinOn joinOnExpr ) )


castTo :: ValueExpr -> Text -> ValueExpr
castTo valueExpr castType =
  Cast
    valueExpr
    (TypeName [Name $ T.unpack castType])


div :: ValueExpr -> ValueExpr -> ValueExpr
div valueA valueB =
  BinOp valueA [Name "/"] valueB


roundTo :: Integer -> ValueExpr -> ValueExpr
roundTo numOfDigits column  =
  App
    [ Name "round" ]
    [ column
    , NumLit $ show numOfDigits
    ]


alias :: Name -> Alias
alias aliasName =
  Alias aliasName Nothing


fromAs :: Name -> Name -> TableRef
fromAs tableName aliasName =
  TRAlias
    (TRSimple [tableName])
    (alias aliasName)


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


createTableWithQuery :: Connection -> Text -> Query -> IO (Doc ann)
createTableWithQuery connection aTableName theQuery = do
  result <- try $ execute_ connection theQuery

  let
    output = case result :: Either SQLError () of
      Left errorMessage ->
        if isSuffixOf "already exists" (sqlErrorDetails errorMessage)
        then ""
        else T.pack $ (show errorMessage) <> "\n"
      Right _ -> "🆕 Create table \"" <> aTableName <> "\"\n"

  pure $ pretty output


getCase :: Maybe Text -> [(Text, Float)] -> Text
getCase fieldNameMaybe valueMap =
  "case "
  <> case fieldNameMaybe of
      Nothing -> ""
      Just fName -> "`" <> fName <> "`"
  <> (P.fold $ fmap
        (\(key, val) -> "  when " <> key <> " then " <> show val <> "\n")
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
