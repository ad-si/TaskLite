{-|
Creates all tables and views
-}

module DbSetup where

import Protolude as P

import Data.Text as T
import Database.SQLite.Simple as Sql
import qualified SqlUtils as S
import Task as Task
import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.Pretty
import Data.Text.Prettyprint.Doc hiding ((<>))
import Config


createTaskTable :: Connection -> IO (Doc ann)
createTaskTable connection = do
  let
    theTableName = tableName conf
    -- TODO: Replace with beam-migrate based table creation
    createTableQuery = S.getTable theTableName (
      "`ulid` text not null primary key" :
      "`body` text not null" :
      ("`state` text check(`state` in (" <> stateOptions
        <> ")) not null default '" <> show stateDefault <> "'") :
      "`due_utc` text" :
      "`closed_utc` text" :
      "`modified_utc` text not null" :
      "`priority_adjustment` float" :
      "`metadata` text" :
      [])

  result <- S.createTableWithQuery
    connection
    theTableName
    createTableQuery

  -- Update modified_utc whenever a task is updated
  -- (and modified_utc itself isn't changed)
  execute_ connection $ S.createTriggerAfterUpdate "set_modified_utc" "tasks"
    "`new`.`modified_utc` is `old`.`modified_utc`"
    "\
      \update `tasks`\n\
      \set `modified_utc` = datetime('now')\n\
      \where `ulid` = `new`.`ulid`\n\
      \"

  execute_ connection $ S.createTriggerAfterUpdate "set_closed_utc" "tasks"
    "`new`.`state` is 'Done' or `new`.`state` is 'Obsolete'"
    "\
      \update `tasks`\n\
      \set `closed_utc` = datetime('now')\n\
      \where `ulid` = `new`.`ulid`\n\
      \"

  pure result


taskViewQuery :: Query
taskViewQuery =
  let
    caseStateSql = S.getCase (Just "state") $ [stateDefault ..]
      & fmap (\tState -> (S.getValue tState, case tState of
          Open     ->  0
          Waiting  -> -3
          Done     ->  0
          Obsolete ->  0
        ))
    caseOverdueSql = S.getCase Nothing
      [ ("`due_utc` is null", 0)
      , ("`due_utc` >= datetime('now', '+1 month')", 0)
      , ("`due_utc` >= datetime('now', '+1 week')", 3)
      , ("`due_utc` >= datetime('now')", 6)
      , ("`due_utc` < datetime('now')", 9)
      ]
    selectQuery = S.getSelect
      (
        "`tasks`.`ulid` as `ulid`" :
        "`tasks`.`body` as `body`" :
        "`tasks`.`state` as `state`" :
        "`tasks`.`due_utc` as `due_utc`" :
        "`tasks`.`closed_utc` as `closed_utc`" :
        "`tasks`.`modified_utc`as `modified_utc`" :
        "group_concat(distinct `task_to_tag`.`tag`) as `tags`" :
        "group_concat(distinct `task_to_note`.`note`) as `notes`" :
        "ifnull(`tasks`.`priority_adjustment`, 0.0)\n\
        \  + " <> caseStateSql <> "\n\
        \  + " <> caseOverdueSql <> "\n\
        \  + case count(`task_to_note`.`note`)\n\
        \      when 0 then 0.0\n\
        \      else 1.0\n\
        \    end\n\
        \  + case count(`task_to_tag`.`tag`)\n\
        \      when 0 then 0.0\n\
        \      else 2.0\n\
        \    end\n\
        \as `priority`" :
        "`tasks`.`metadata`as `metadata`" :
        []
      )
      ("`" <> tableName conf <> "` \n\
        \left join task_to_tag on tasks.ulid = task_to_tag.task_ulid \n\
        \left join task_to_note on tasks.ulid = task_to_note.task_ulid \n\
        \")
      "`tasks`.`ulid`"
  in
    selectQuery


createTaskView :: Connection -> IO (Doc ann)
createTaskView connection = do
  let
    viewName = "tasks_view"

  S.createTableWithQuery
    connection
    viewName
    (S.getView viewName taskViewQuery)


createTagsTable :: Connection -> IO (Doc ann)
createTagsTable connection = do
  let
    theTableName = "task_to_tag"
    createTableQuery = S.getTable theTableName (
      "`ulid` text not null primary key" :
      "`task_ulid` text not null" :
      "`tag` text not null" :
      "foreign key(`task_ulid`) references `" <> tableName conf <> "`(`ulid`)" :
      "constraint `no_duplicate_tags` unique (`task_ulid`, `tag`) " :
      [])

  S.createTableWithQuery
    connection
    theTableName
    createTableQuery


tagsViewQuery :: Query
tagsViewQuery =
  let
    txtToName = Name . T.unpack

    tasks_t         = txtToName "tasks"
    task_to_tag_t   = txtToName "task_to_tag"
    task_to_tag_1_t = txtToName "task_to_tag_1"
    task_to_tag_2_t = txtToName "task_to_tag_2"

    closed_count_c  = txtToName "closed_count"
    tag_c           = txtToName "tag"
    ulid_c          = txtToName "ulid"
    task_ulid_c     = txtToName "task_ulid"
    closed_utc_c    = txtToName "closed_utc"
    open_c          = txtToName "open"
    closed_c        = txtToName "closed"
    progress_c      = txtToName "progress"

    t1Tag = S.tableCol task_to_tag_1_t tag_c
    t2Tag = S.tableCol task_to_tag_2_t tag_c
    closedCount = S.ifNull closed_count_c "0"
    t1TagCount = S.count t1Tag

    subQueryAst = makeSelect
      { qeSelectList = (
          S.col tag_c `S.as` (Name "") :
          S.count (S.tableCol tasks_t ulid_c) `S.as` closed_count_c :
          [])
      , qeFrom = [ S.leftJoinOn tasks_t task_to_tag_t $
            (tasks_t `S.dot` ulid_c)
            `S.is`
            (task_to_tag_t `S.dot` task_ulid_c)
          ]
      , qeWhere = Just $ S.isNotNull closed_utc_c
      , qeGroupBy = [ S.groupBy $ S.col tag_c ]
      }
    selectQueryAst = makeSelect
      { qeSelectList = (
          t1Tag `S.as` (Name "") :
          t1TagCount `S.as` open_c :
          closedCount `S.as` closed_c :
          (S.roundTo 6 (closedCount `S.castTo` "float" `S.div` t1TagCount))
            `S.as` progress_c :
          [])
      , qeFrom = [ S.leftTRJoinOn
          (TRAlias
            (TRSimple [task_to_tag_t])
            (S.alias task_to_tag_1_t))
          (TRAlias
            (TRQueryExpr subQueryAst)
            (S.alias task_to_tag_2_t))
          (t1Tag `S.is` t2Tag)
        ]
      , qeGroupBy = [ S.groupBy t1Tag ]
      , qeOrderBy = [ S.orderByAsc t1Tag ]
      }
    selectQueryText = T.pack $ prettyQueryExpr SQL2011 selectQueryAst
  in
    Query selectQueryText


createTagsView :: Connection -> IO (Doc ann)
createTagsView connection = do
  let viewName = "tags"

  S.createTableWithQuery
    connection
    viewName
    (S.getView viewName tagsViewQuery)


createNotesTable :: Connection -> IO (Doc ann)
createNotesTable connection = do
  let
    theTableName = "task_to_note"
    createTableQuery = S.getTable theTableName (
      "`ulid` text not null primary key" :
      "`task_ulid` text not null" :
      "`note` text not null" :
      "foreign key(`task_ulid`) references `" <> tableName conf <> "`(`ulid`)" :
      [])

  S.createTableWithQuery
    connection
    theTableName
    createTableQuery


createTables :: Connection -> IO (Doc ann)
createTables connection = do
  t1 <- createTaskTable connection
  t2 <- createTagsTable connection
  t3 <- createNotesTable connection

  v1 <- createTaskView connection
  v2 <- createTagsView connection

  pure $
    t1 <> t2 <> t3 <>
    v1 <> v2
