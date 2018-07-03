module Lib where

import Protolude as P

import Data.Hourglass
import Codec.Crockford as Crock
import Data.Text as T
import Data.ULID
import Database.Beam
import Database.Beam.Sqlite
import Database.Beam.Schema.Tables
import Database.SQLite.Simple as Sql
import Numeric
import System.Directory
import System.Process (readProcess)
import qualified Text.Fuzzy as Fuzzy
import Time.System
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Text.Prettyprint.Doc.Util
import Data.Text.Prettyprint.Doc.Render.Terminal
import Unsafe (unsafeHead)
import Utils
import qualified SqlUtils as S
import Task as Task
import FullTask as FullTask
import Note as Note
import TaskToNote as TaskToNote
import Language.SQL.SimpleSQL.Syntax
import Language.SQL.SimpleSQL.Pretty


data Config = Config
  { tableName :: Text
  , idWidth :: Int
  , idStyle :: AnsiStyle
  , priorityStyle :: AnsiStyle
  , dateStyle :: AnsiStyle
  , bodyStyle :: AnsiStyle
  , closedStyle :: AnsiStyle
  , dueStyle :: AnsiStyle
  , tagStyle :: AnsiStyle
  , utcFormat :: TimeFormatString
  , mainDir :: FilePath
  , dbName :: FilePath
  , dateWidth :: Int
  , bodyWidth :: Int
  , prioWidth :: Int
  , headCount :: Int
  , maxWidth :: Int
  , progressBarWidth :: Int
  }


conf :: Config
conf = Config
  { tableName = "tasks"
  , idWidth = 4
  , idStyle = color Green
  , priorityStyle = color Magenta
  , dateStyle = color Yellow
  , bodyStyle = color White
  , closedStyle = color Black
  , dueStyle = color Red
  , tagStyle = color Blue
  , utcFormat = toFormat ("YYYY-MM-DD H:MI:S" :: [Char])
  , mainDir = "tasklite"
  , dbName = "main.db"
  , dateWidth = 10
  , bodyWidth = 10
  , prioWidth = 4
  , headCount = 20
  , maxWidth = 120
  , progressBarWidth = 24
  }


getMainDir :: FilePath -> FilePath
getMainDir = (<> "/" <> (mainDir conf) )


noTasksWarning :: Text
noTasksWarning = "No tasks available"


newtype NumRows = NumRows Integer
  deriving (Eq, Ord, Read, Show)

instance FromRow NumRows where
  fromRow = NumRows <$> field


-- | Record for storing entries of the `task_to_tag` table
data TaskToTagT f = TaskToTag
  { _ttUlid :: Columnar f Text -- Ulid
  , _ttTaskUlid :: PrimaryKey TaskT f
  , _ttTag :: Columnar f Text
  } deriving Generic

type TaskToTag = TaskToTagT Identity
type TaskToTagId = PrimaryKey TaskToTagT Identity

-- FIXME: Probably doesn't work because of `PrimaryKey TaskT f`
-- deriving instance Show TaskToTag
-- deriving instance Eq TaskToTag

instance Beamable TaskToTagT

instance Table TaskToTagT where
  data PrimaryKey TaskToTagT f = TaskToTagId (Columnar f Text)
    deriving Generic
  primaryKey = TaskToTagId . _ttUlid
instance Beamable (PrimaryKey TaskToTagT)


-- | Record for storing entries of the `tasks_view` table
-- TODO: Use Beam instead of SQLite.Simple
data TaskView f = TaskView
  { _tvTask :: TaskT f
  , _tvTag :: TaskToTagT f
  } deriving Generic
instance Beamable TaskView


data TaskLiteDb f = TaskLiteDb
  { _tldbTasks :: f (TableEntity TaskT)
  , _tldbTaskToTag :: f (TableEntity TaskToTagT)
  , _tldbTaskToNote :: f (TableEntity TaskToNoteT)
  , _tldbTasksView :: f (ViewEntity TaskView)
  } deriving Generic

instance Database be TaskLiteDb


taskLiteDb :: DatabaseSettings be TaskLiteDb
taskLiteDb = defaultDbSettings `withDbModification`
  dbModification
    { _tldbTaskToTag = modifyTable identity $
        tableModification
          { _ttTaskUlid = TaskUlid (fieldNamed "task_ulid") }
    , _tldbTaskToNote = modifyTable identity $
        tableModification
          { TaskToNote.task_ulid = TaskUlid (fieldNamed "task_ulid") }
    }


createTaskTable :: Connection -> IO ()
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

  S.createTableWithQuery
    connection
    theTableName
    createTableQuery

  -- | Update `modified_utc` whenever a task is updated
  -- | (and `modified_utc` itselft isn't changed)
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


createTaskView :: Connection -> IO ()
createTaskView connection = do
  let
    viewName = "tasks_view"

  S.createTableWithQuery
    connection
    viewName
    (S.getView viewName taskViewQuery)


createTagsTable :: Connection -> IO ()
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
    tags_t          = txtToName "tags"
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


createTagsView :: Connection -> IO ()
createTagsView connection = do
  let viewName = "tags"

  S.createTableWithQuery
    connection
    viewName
    (S.getView viewName tagsViewQuery)


createNotesTable :: Connection -> IO ()
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


getDbPath :: IO FilePath
getDbPath = do
  homeDir <- getHomeDirectory
  pure $ (getMainDir homeDir) <> "/" <> (dbName conf)


setupConnection :: IO Connection
setupConnection = do
  homeDir <- getHomeDirectory
  createDirectoryIfMissing True $ getMainDir homeDir
  connection <- open $ (getMainDir homeDir) <> "/" <> (dbName conf)

  createTaskTable connection
  createTagsTable connection
  createNotesTable connection

  createTaskView connection
  createTagsView connection

  return connection


execWithConn :: (Connection -> IO a) -> IO a
execWithConn func = do
  homeDir <- getHomeDirectory
  createDirectoryIfMissing True $ getMainDir homeDir
  withConnection
    ((getMainDir homeDir) <> "/" <> (dbName conf))
    (\connection -> do
        createTaskTable connection
        func connection
    )


insertTask :: Connection -> Task -> IO ()
insertTask connection task = do
  runBeamSqlite connection $ runInsert $
    insert (_tldbTasks taskLiteDb) $
    insertValues [task]


insertTags :: Connection -> TaskUlid -> [Text] -> IO ()
insertTags connection primKey tags = do
  taskToTags <- forM tags $ \tag -> do
    tagUlid <- fmap (toLower . show) getULID
    pure $ TaskToTag tagUlid primKey tag

  runBeamSqlite connection $ runInsert $
    insert (_tldbTaskToTag taskLiteDb) $
    insertValues taskToTags


insertNotes :: Connection -> TaskUlid -> [Note] -> IO ()
insertNotes connection primKey notes = do
  taskToNotes <- forM notes $ \theNote -> do
    pure $ TaskToNote (Note.ulid theNote) primKey (Note.body theNote)

  runBeamSqlite connection $ runInsert $
    insert (_tldbTaskToNote taskLiteDb) $
    insertValues taskToNotes


formatElapsed :: IO Elapsed -> IO Text
formatElapsed =
  fmap (pack . (timePrint $ utcFormat conf))


formatUlid :: IO ULID -> IO Text
formatUlid =
  fmap (toLower . show)


parseTaskBody :: [Text] -> (Text, [Text])
parseTaskBody bodyWords =
  let
    -- Handle case when word is actually a text
    bodyWordsReversed = bodyWords & T.unwords & T.words & P.reverse
    body = bodyWordsReversed
      & P.dropWhile ("+" `T.isPrefixOf`)
      & P.reverse
      & unwords
    tags = bodyWordsReversed
      & P.takeWhile ("+" `T.isPrefixOf`)
      <&> T.replace "+" ""
      & P.reverse
  in
    (body, tags)


addTask :: Connection -> [Text] -> IO (Doc AnsiStyle)
addTask connection bodyWords = do
  ulid <- formatUlid getULID
  modified_utc <- formatElapsed timeCurrent
  let
    (body, tags) = parseTaskBody bodyWords
    task = Task
      { state = Open
      , due_utc = Nothing
      , closed_utc = Nothing
      , priority_adjustment = Nothing
      , metadata = Nothing
      , ..
      }

  insertTask connection task
  insertTags connection (primaryKey task) tags
  pure $
    "🆕 Added task" <+> (dquotes $ pretty $ Task.body task)
    <+> "with ulid" <+> (dquotes $ pretty $ Task.ulid task)
    <+> hardline


logTask :: Connection -> [Text] -> IO (Doc AnsiStyle)
logTask connection bodyWords = do
  ulid <- formatUlid getULID
  -- TODO: Set via a SQL trigger
  modified_utc <- formatElapsed timeCurrent
  let
    (body, tags) = parseTaskBody bodyWords
    task = Task
      { state = Done
      , due_utc = Nothing
      , closed_utc = Just modified_utc
      , priority_adjustment = Nothing
      , metadata = Nothing
      , ..
      }

  insertTask connection task
  insertTags connection (primaryKey task) tags
  pure $
    "📝 Logged task" <+> (dquotes $ pretty $ Task.body task)
    <+> "with ulid" <+> (dquotes $ pretty $ Task.ulid task)
    <+> hardline


execWithId ::
  Connection -> Text -> (TaskUlid -> IO (Doc AnsiStyle)) -> IO (Doc AnsiStyle)
execWithId connection idSubstr callback = do
  tasks <- (query connection
      (Query $ "select * from " <> tableName conf <> " where `ulid` like ?")
      ["%"  <> idSubstr :: Text]
    ) :: IO [Task]

  let
    numOfTasks = P.length tasks
    ulidLength = 26
    prefix = if (T.length idSubstr) == ulidLength
      then ""
      else "…"
    quote = dquotes . pretty

  if
    | numOfTasks == 0 -> pure $
        "⚠️  Task" <+> (quote $ prefix <> idSubstr) <+> "does not exist"
    | numOfTasks == 1 ->
        callback $ primaryKey $ unsafeHead tasks
    | numOfTasks > 1 -> pure $
        "⚠️  Id slice" <+> (quote idSubstr) <+> "is not unique."
        <+> "Please use a longer slice!"


setStateAndClosed :: Connection -> TaskUlid -> TaskState -> IO ()
setStateAndClosed connection taskUlid theTaskState = do
  runBeamSqlite connection $ runUpdate $
    update (_tldbTasks taskLiteDb)
      (\task -> [ (Task.state task) <-. val_  theTaskState])
      (\task -> primaryKey task ==. val_ taskUlid &&.
                (Task.state task) /=. val_ theTaskState)


doTask :: Connection -> Text -> IO (Doc AnsiStyle)
doTask connection idSubstr = do
  execWithId connection idSubstr $ \taskUlid@(TaskUlid idText) -> do
    setStateAndClosed connection taskUlid Done

    numOfChanges <- changes connection

    pure $ pretty $ if numOfChanges == 0
      then "⚠️  Task \"…" <> idText <> "\" is already done"
      else "✅ Finished task \"…" <> idText <> "\""


endTask :: Text -> IO (Doc AnsiStyle)
endTask idSubstr = do
  dbPath <- getDbPath
  withConnection dbPath $ \connection -> do
    execWithId connection idSubstr $ \taskUlid@(TaskUlid idText) -> do
      setStateAndClosed connection taskUlid Obsolete

      numOfChanges <- changes connection

      pure $ pretty $ if numOfChanges == 0
        then "⚠️  Task \"…" <> idText <> "\" is already marked as obsolete"
        else "⏹  Marked task \"…" <> idText <> "\" as obsolete"


deleteTask :: Connection -> Text -> IO (Doc AnsiStyle)
deleteTask connection idSubstr = do
  -- TODO: Delete corresponding tags and notes
  execWithId connection idSubstr $ \(TaskUlid idText) -> do
    execute connection
      (Query $ "delete from `" <> tableName conf <> "` where `ulid` == ?")
      [idText :: Text]

    numOfChanges <- changes connection

    pure $ pretty $ if numOfChanges == 0
      then "⚠️ An error occured while deleting task \"…" <> idText <> "\""
      else "❌ Deleted task \"…" <> idText <> "\""


adjustPriority :: Float -> [IdText] -> IO (Doc AnsiStyle)
adjustPriority adjustment ids  = do
  dbPath <- getDbPath
  withConnection dbPath $ \connection -> do
    docs <- forM ids $ \idSubstr -> do
      doc <- execWithId connection idSubstr $ \(TaskUlid idText) -> do
        -- TODO: Figure out why this doesn't work
        -- runBeamSqlite connection $ runUpdate $
        --   update (_tldbTasks taskLiteDb)
        --     (\task -> [(Task.priority_adjustment task) <-.
        --       fmap (+ adjustment) (current_ (Task.priority_adjustment task))
        --     ])
        --     (\task -> primaryKey task ==. val_ taskUlid)

        execute connection
          (Query $ "update `tasks` \
            \set `priority_adjustment` = ifnull(`priority_adjustment`, 0) + ? \
            \where `ulid` == ?")
          (adjustment, idText :: Text)

        numOfChanges <- changes connection

        pure $ pretty $ if numOfChanges == 0
          then
            "⚠️ An error occured \
            \while adjusting the priority of task \"" <> idText <> "\""
          else (
            (if adjustment > 0 then "⬆️  Increased" else "⬇️  Decreased")
            <> " priority of task \""
            <> idText <> "\" by " <> (show $ abs adjustment)
          )

      pure doc
    pure $ vsep docs


infoTask :: Text -> IO (Doc AnsiStyle)
infoTask idSubstr = do
  dbPath <- getDbPath
  withConnection dbPath $ \connection -> do
    execWithId connection idSubstr $ \(TaskUlid idText) -> do
      tasks <- query connection
        (Query $ "select * from `tasks_view` where `ulid` == ?")
        [idText :: Text]

      pure $ case P.head (tasks :: [FullTask]) of
        Nothing -> pretty
          ("This case should already be handled by `execWithId`" :: Text)
        Just task -> pretty task


nextTask :: Connection -> IO (Doc AnsiStyle)
nextTask connection = do
  let
    -- TODO: Add "state is 'Waiting' and `wait_utc` < datetime('now')"
    selectQuery = "select * from `tasks_view` where state is 'Open'"
    orderByAndLimit = "order by `priority` desc limit 1"
  tasks <- query_ connection $ Query $ selectQuery <> orderByAndLimit

  pure $ case P.head (tasks :: [FullTask]) of
    Nothing -> pretty noTasksWarning
    Just task -> pretty task


findTask :: Text -> IO (Doc AnsiStyle)
findTask pattern = do
  connection <- setupConnection
  tasks <- query_ connection $ Query $
    "select ulid, body, tags, notes, metadata from tasks_view"

  let
    scoreWidth = 5
    numOfResults = 8
    results = Fuzzy.filter
      pattern
      (tasks :: [(Text, Text, Maybe [Text], Maybe [Text], Maybe Text)])
      "\x1b[4m\x1b[32m" -- Set underline and color to green
      "\x1b[0m"
      (\(ulid, body, tags, notes, metadata) -> unwords
        [ ulid
        , "\n"
        , body
        , fromMaybe "" (unwords <$> tags)
        , fromMaybe "" (unwords <$> notes)
        , T.replace "\",\"" "\", \"" $ fromMaybe "" metadata
        ])
      False -- Case insensitive
    moreResults = (P.length results) - numOfResults
    header =
      (annotate (underlined <> color Red) $ fill scoreWidth "Score") <++>
      (annotate (underlined) $ fill 10 "Task") <>
      hardline
    body =
      results
        & P.take numOfResults
        <&> (\result ->
              annotate (color Red)
                (fill scoreWidth $ pretty $ Fuzzy.score result)
              <++> (align (reflow $ Fuzzy.rendered result)))
        & vsep
    footer =
      if moreResults > 0
      then hardline
        <> hardline
        <> annotate (color Red)
            ("There are " <> pretty moreResults <> " more results available")
        <> hardline
      else hardline

  pure $ header <> body <> footer


-- TODO: Use Continuation monad to avoid callback hell
-- withConnectCont :: Text -> ContT a IO Connection
-- withConnectCont dbPath =
--     ContT $ withConnection dbPath


addTag :: Connection -> Text -> [IdText] -> IO (Doc AnsiStyle)
addTag connection tag ids = do
  docs <- forM ids $ \idSubstr -> do
    doc <- execWithId connection idSubstr $ \taskUlid@(TaskUlid idText) -> do
      now <- fmap (pack . (timePrint $ utcFormat conf)) timeCurrent
      ulid <- fmap (toLower . show) getULID

      let taskToTag = TaskToTag ulid taskUlid tag

      runBeamSqlite connection $ runInsert $
        insert (_tldbTaskToTag taskLiteDb) $
        insertValues [taskToTag]

      runBeamSqlite connection $ runUpdate $
        update (_tldbTasks taskLiteDb)
          (\task -> [(Task.modified_utc task) <-. val_ now])
          (\task -> primaryKey task ==. val_ taskUlid)

      pure $ "🏷  Added tag" <+> (dquotes $ pretty tag)
        <+> "to task" <+> (dquotes $ pretty idText)
        <+> hardline
    pure doc
  pure $ vsep docs


setDueUtc :: Connection -> DateTime -> [IdText] -> IO (Doc AnsiStyle)
setDueUtc connection datetime ids = do
  let
    utcText :: Text
    utcText = pack $ timePrint (utcFormat conf) datetime

  docs <- forM ids $ \idSubstr -> do
    doc <- execWithId connection idSubstr $ \taskUlid@(TaskUlid idText) -> do
      runBeamSqlite connection $ runUpdate $
        update (_tldbTasks taskLiteDb)
          (\task -> [(Task.due_utc task) <-. (val_ $ Just utcText)])
          (\task -> primaryKey task ==. val_ taskUlid)

      pure $ "📅 Set due UTC to" <+> (dquotes $ pretty utcText)
        <+> "of task" <+> (dquotes $ pretty idText)
    pure doc
  pure $ vsep docs


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


showAtPrecision :: Double -> Text
showAtPrecision number =
  let tuple = breakOn "." (show number)
  in fst tuple <> (T.replace ".0" "  " . T.take 2 . snd) tuple


formatTaskLine :: Int -> FullTask -> Doc AnsiStyle
formatTaskLine taskUlidWidth task =
  let
    id = pretty $ T.takeEnd taskUlidWidth $ FullTask.ulid task
    createdUtc = fmap
      (pack . timePrint ISO8601_Date)
      (ulidToDateTime $ FullTask.ulid task)
    body = FullTask.body task
    tags = fromMaybe [] $ FullTask.tags task
    formatTag = annotate (tagStyle conf)
      . (annotate (color Black) "+" <>)
      . pretty
    closedUtcMaybe = (FullTask.closed_utc task)
      >>= parseUtc
      <&> timePrint (utcFormat conf)
    dueUtcMaybe = (FullTask.due_utc task)
      >>= parseUtc
      <&> timePrint (utcFormat conf)
    multilineIndent = 2
    hangWidth = taskUlidWidth + 2
      + (dateWidth conf) + 2
      + (prioWidth conf) + 2
      + multilineIndent
    taskLine = createdUtc <$$> \taskDate -> hang hangWidth $
           annotate (idStyle conf) id
      <++> annotate (priorityStyle conf) (pretty $ justifyRight 4 ' '
            $ showAtPrecision $ realToFrac
            $ fromMaybe 0 (FullTask.priority task))
      <++> annotate (dateStyle conf) (pretty taskDate)
      <++> annotate (bodyStyle conf) (reflow body)
      <++> annotate (dueStyle conf) (pretty dueUtcMaybe)
      <++> annotate (closedStyle conf) (pretty closedUtcMaybe)
      <++> hsep (tags <$$> formatTag)
  in
    fromMaybe
      ("Id" <+> (dquotes $ pretty $ FullTask.ulid task) <+>
        "is an invalid ulid and could not be converted to a datetime")
      taskLine


getIdLength :: Float -> Int
getIdLength numOfItems =
  -- TODO: Calculate idLength by total number of tasks, not just of the viewed
  let
    targetCollisionChance = 0.01  -- Targeted likelihood of id collisions
    sizeOfAlphabet = 32  -- Crockford's base 32 alphabet
  in
    (ceiling $ log
      (numOfItems / targetCollisionChance) / log sizeOfAlphabet) + 1


countTasks :: Filter TaskState -> IO (Doc AnsiStyle)
countTasks taskStateFilter = do
  execWithConn $ \connection -> do
    [NumRows taskCount] <- case taskStateFilter of
      NoFilter -> query_ connection $ Query $
        "select count(*) from `" <> tableName conf <> "`"
      Utils.Only taskState -> query connection
        (Query $ "select count(*) from `" <> tableName conf
          <> "` where `state` == ?")
        [(show taskState) :: Text]

    pure $ pretty taskCount


-- TODO: Print number of remaining tasks and how to display them at the bottom
headTasks :: Connection -> IO (Doc AnsiStyle)
headTasks connection = do
  tasks <- query_ connection $ Query $
    -- TODO: Add `wait_utc` < datetime('now')"
    "select * from tasks_view \
    \where closed_utc is null \
    \order by `priority` desc limit " <> show (headCount conf)
  pure $ formatTasks tasks


newTasks :: Connection -> IO (Doc AnsiStyle)
newTasks connection = do
  tasks <- query_ connection $ Query $
    -- TODO: Add `wait_utc` < datetime('now')"
    "select * from `tasks_view` \
    \where closed_utc is null \
    \order by `ulid` desc limit " <> show (headCount conf)
  pure $ formatTasks tasks


openTasks :: Connection -> IO (Doc AnsiStyle)
openTasks connection = do
  tasks <- query_ connection $ Query $
    "select * from `tasks_view` \
    \where closed_utc is null \
    \order by `ulid` desc"
  pure $ formatTasks tasks


overdueTasks :: Connection -> IO (Doc AnsiStyle)
overdueTasks connection = do
  tasks <- query_ connection $ Query $
    "select * from `tasks_view` \
    \where closed_utc is null and due_utc < datetime('now') \
    \order by `priority` desc"
  pure $ formatTasks tasks


doneTasks :: Connection -> IO (Doc AnsiStyle)
doneTasks connection = do
  tasks <- query_ connection $ Query $
    "select * from tasks_view \
    \where closed_utc is not null and state is 'Done' \
    \order by ulid desc limit " <> show (headCount conf)
  pure $ formatTasks tasks


obsoleteTasks :: Connection -> IO (Doc AnsiStyle)
obsoleteTasks connection = do
  tasks <- query_ connection $ Query $
    "select * from tasks_view \
    \where closed_utc is not null and state is 'Obsolete' \
    \order by ulid desc limit " <> show (headCount conf)
  pure $ formatTasks tasks


listWaiting :: Connection -> IO (Doc AnsiStyle)
listWaiting connection = do
  tasks <- query_ connection
    "select * from tasks_view \
    \where state == 'Waiting' \
    \order by priority desc"

  pure $ formatTasks tasks


listAll :: Connection -> IO (Doc AnsiStyle)
listAll connection = do
  tasks <-  query_ connection
    "select * from tasks_view order by priority desc"
  pure $ formatTasks tasks


listNoTag :: Connection -> IO (Doc AnsiStyle)
listNoTag connection = do
  tasks <-  query_ connection
    "select * from tasks_view \
    \where closed_utc is null and tags is null \
    \order by priority desc"
  pure $ formatTasks tasks


queryTasks :: Text -> IO (Doc AnsiStyle)
queryTasks sqlQuery = do
  connection <- setupConnection
  tasks <- query_ connection $ Query $
    "select * from `tasks_view` where " <> sqlQuery
  pure $ formatTasks tasks


runSql :: Text -> IO (Doc AnsiStyle)
runSql sqlQuery = do
  homeDir <- getHomeDirectory
  result <- readProcess "sqlite3"
    [ (getMainDir homeDir) <> "/" <> (dbName conf)
    , ".headers on"
    , ".mode csv"
    , ".separator , '\n'"
    , T.unpack sqlQuery
    ]
    []
  pure $ pretty result


formatTasks :: [FullTask] -> Doc AnsiStyle
formatTasks tasks =
  if P.length tasks == 0
  then pretty noTasksWarning
  else
    let
      strong = bold <> underlined
      taskUlidWidth = getIdLength $ fromIntegral $ P.length tasks
      docHeader =
             (annotate (idStyle conf <> strong) $
                fill taskUlidWidth "Id")
        <++> (annotate (priorityStyle conf <> strong) $
                fill (prioWidth conf) "Prio")
        <++> (annotate (dateStyle conf <> strong) $
                fill (dateWidth conf) "Opened UTC")
        <++> (annotate (bodyStyle conf <> strong) $
                fill (bodyWidth conf) "Body")
        <++> line
    in
      docHeader <>
      (vsep $ fmap (formatTaskLine taskUlidWidth) tasks) <>
      line


getProgressBar :: Integer -> Double -> Doc AnsiStyle
getProgressBar maxWidthInChars progress =
  let
    width = floor (progress * (fromInteger maxWidthInChars))
    remainingWidth = fromIntegral $ maxWidthInChars - width
  in
    (annotate (bgColorDull Green <> colorDull Green) $ pretty $
      P.take (fromIntegral width) $ P.repeat '#') <>
    -- (annotate (bgColorDull Green) $ fill (fromIntegral width) "" <>
    (annotate (bgColorDull Black) $ fill remainingWidth "")


formatTagLine :: Int -> (Text, Integer, Integer, Double) -> Doc AnsiStyle
formatTagLine maxTagLength (tag, open_count, closed_count, progress) =
  let
    barWidth = toInteger $ progressBarWidth conf
    progressPercentage =
      if progress == 0
      then "     "
      else
        (pretty $ justifyRight 3 ' ' $ T.pack $
          showFFloat (Just 0) (progress * 100) "")
        <+> "%"
  in
    (fill maxTagLength $ pretty tag)
    <++> (pretty $ justifyRight (T.length "open") ' ' $ show open_count)
    <++> (pretty $ justifyRight (T.length "closed") ' ' $ show closed_count)
    <++> progressPercentage <+> (getProgressBar barWidth progress)



listTags :: Connection -> IO (Doc AnsiStyle)
listTags connection = do
  tags <- query_ connection $ Query "select * from tags"

  let
    percWidth = 6  -- Width of e.g. 100 %
    progressWith = (progressBarWidth conf) + percWidth
    firstOf4 = \(a, b, c, d) -> a
    maxTagLength = tags
      <&> (T.length . firstOf4)
      & P.maximum

  pure $
         (annotate (bold <> underlined) $ fill maxTagLength "Tag")
    <++> (annotate (bold <> underlined) $ "Open")
    <++> (annotate (bold <> underlined) $ "Closed")
    <++> (annotate (bold <> underlined) $ fill progressWith "Progress")
    <> line
    <> (vsep $ fmap (formatTagLine maxTagLength) tags)
    <> hardline

