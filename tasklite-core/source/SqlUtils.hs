{-|
Utils to simplify creation of SQL queries
-}
module SqlUtils where

import Protolude as P (
  Applicative (pure),
  Bool (False),
  Either (..),
  Eq ((==)),
  Float,
  Foldable (fold),
  Functor (fmap),
  IO,
  Integer,
  Maybe (..),
  Semigroup ((<>)),
  Show,
  Text,
  show,
  take,
  try,
  unlines,
  unwords,
  words,
  ($),
 )

import Data.Text as T (
  intercalate,
  isSuffixOf,
  pack,
  unlines,
  unpack,
 )
import Database.SQLite.Simple as Sql (
  Connection,
  Query (..),
  SQLError (sqlErrorDetails),
  execute_,
 )
import Language.SQL.SimpleSQL.Syntax (
  Alias (..),
  Direction (Asc, Desc),
  GroupingExpr (SimpleGroup),
  JoinCondition (JoinOn),
  JoinType (JLeft),
  Name (..),
  NullsOrder (NullsOrderDefault),
  ScalarExpr (App, BinOp, Cast, Iden, NumLit, PostfixOp),
  SortSpec (..),
  TableRef (TRAlias, TRJoin, TRSimple),
  TypeName (TypeName),
 )
import Prettyprinter (Doc, pretty)


id :: Name -> ScalarExpr
id columnName =
  Iden [columnName]


ids :: [Name] -> ScalarExpr
ids = Iden


tableCol :: Name -> Name -> ScalarExpr
tableCol table column =
  Iden [table, column]


col :: Name -> ScalarExpr
col column =
  Iden [column]


count :: ScalarExpr -> ScalarExpr
count column =
  App
    [Name Nothing "count"]
    [column]


ifNull :: Name -> Text -> ScalarExpr
ifNull ifValue thenValue =
  App
    [Name Nothing "ifnull"]
    [ Iden [ifValue]
    , NumLit $ T.unpack thenValue
    ]


dot :: Name -> Name -> ScalarExpr
dot item subItem =
  ids [item, subItem]


is :: ScalarExpr -> ScalarExpr -> ScalarExpr
is exprA =
  BinOp exprA [Name Nothing "is"]


isNotNull :: Name -> ScalarExpr
isNotNull columnName =
  PostfixOp
    [Name Nothing "is not null"]
    (Iden [columnName])


as :: ScalarExpr -> Name -> (ScalarExpr, Maybe Name)
as column aliasName@(Name _ theAlias) =
  ( column
  , if theAlias == ""
      then Nothing
      else Just aliasName
  )


-- as column otherAlias = (column, Just otherAlias)

groupBy :: ScalarExpr -> GroupingExpr
groupBy = SimpleGroup


orderByAsc :: ScalarExpr -> SortSpec
orderByAsc column =
  SortSpec column Asc NullsOrderDefault


orderByDesc :: ScalarExpr -> SortSpec
orderByDesc column =
  SortSpec column Desc NullsOrderDefault


leftJoinOn :: Name -> Name -> ScalarExpr -> TableRef
leftJoinOn tableA tableB joinOnExpr =
  TRJoin
    (TRSimple [tableA])
    False
    JLeft
    (TRSimple [tableB])
    (Just (JoinOn joinOnExpr))


leftTRJoinOn :: TableRef -> TableRef -> ScalarExpr -> TableRef
leftTRJoinOn tableA tableB joinOnExpr =
  TRJoin
    tableA
    False
    JLeft
    tableB
    (Just (JoinOn joinOnExpr))


castTo :: ScalarExpr -> Text -> ScalarExpr
castTo scalarExpr castType =
  Cast
    scalarExpr
    (TypeName [Name Nothing $ T.unpack castType])


add :: ScalarExpr -> ScalarExpr -> ScalarExpr
add valueA =
  BinOp valueA [Name Nothing "+"]


sub :: ScalarExpr -> ScalarExpr -> ScalarExpr
sub valueA =
  BinOp valueA [Name Nothing "-"]


div :: ScalarExpr -> ScalarExpr -> ScalarExpr
div valueA =
  BinOp valueA [Name Nothing "/"]


roundTo :: Integer -> ScalarExpr -> ScalarExpr
roundTo numOfDigits column =
  App
    [Name Nothing "round"]
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
getTable tableName columns =
  Query $
    T.unlines
      [ "create table `" <> tableName <> "` ("
      , T.intercalate ",\n" columns
      , ");"
      ]


getColumns :: Text -> [Text] -> Query
getColumns tableName columns =
  Query $
    unlines
      [ "select"
      , "  " <> T.intercalate ",\n  " columns <> "\n"
      , "from `" <> tableName <> "`;"
      ]


getSelect :: [Text] -> Text -> Text -> Query
getSelect selectLines fromStatement groupByColumn =
  Query $
    T.unlines
      [ "select"
      , T.intercalate ",\n" selectLines
      , "from"
      , fromStatement
      , "group by " <> groupByColumn <> ";"
      ]


getView :: Text -> Query -> Query
getView viewName selectQuery =
  Query $
    T.unlines
      [ "create view `" <> viewName <> "` as"
      , fromQuery selectQuery
      ]


createWithQuery :: Connection -> Query -> IO (Doc ann)
createWithQuery connection theQuery = do
  result <- try $ execute_ connection theQuery

  let
    output = case result :: Either SQLError () of
      Left errorMessage ->
        if "already exists" `T.isSuffixOf` sqlErrorDetails errorMessage
          then ""
          else T.pack $ show errorMessage <> "\n"
      Right _ ->
        "🆕 " <> unwords (P.take 3 $ words $ show theQuery) <> "… \n"

  pure $ pretty output


createTableWithQuery :: Connection -> Text -> Query -> IO (Doc ann)
createTableWithQuery connection aTableName theQuery = do
  result <- try $ execute_ connection theQuery

  let
    output = case result :: Either SQLError () of
      Left errorMessage ->
        if "already exists" `T.isSuffixOf` sqlErrorDetails errorMessage
          then ""
          else T.pack $ show errorMessage <> "\n"
      Right _ -> "🆕 Create table \"" <> aTableName <> "\"\n"

  pure $ pretty output


replaceTableWithQuery :: Connection -> Text -> Query -> IO (Doc ann)
replaceTableWithQuery connection aTableName theQuery = do
  execute_ connection $ Query $ "drop table if exists `" <> aTableName <> "`"
  result <- try $ execute_ connection theQuery

  let
    output = case result :: Either SQLError () of
      Left errorMessage -> T.pack $ show errorMessage <> "\n"
      Right _ -> "🆕 Replace table \"" <> aTableName <> "\"\n"

  pure $ pretty output


getCase :: Maybe Text -> [(Text, Float)] -> Text
getCase fieldNameMaybe valueMap =
  "case "
    <> case fieldNameMaybe of
      Nothing -> ""
      Just fName -> "`" <> fName <> "`"
    <> P.fold
      ( fmap
          (\(key, val) -> "  when " <> key <> " then " <> show val <> "\n")
          valueMap
      )
    <> " end "


createTriggerAfterUpdate :: Text -> Text -> Text -> Text -> Query
createTriggerAfterUpdate name tableName whenBlock body =
  Query $
    "\
    \create trigger `"
      <> name
      <> "_after_update`\n\
         \after update on `"
      <> tableName
      <> "`\n\
         \when "
      <> whenBlock
      <> "\n\
         \begin\n\
         \  "
      <> body
      <> ";\n\
         \end;\n\
         \"
