{-|
Migrations of SQLite database for new versions
-}

module Migrations where

import Protolude as P

import Database.SQLite.Simple
import Prettyprinter hiding ((<>))
import Config


newtype UserVersion = UserVersion Int
  deriving (Eq, Ord, Read, Show)

instance FromRow UserVersion where
  fromRow = UserVersion <$> field


-- | List of queries for one migration
type QuerySet = [Query]


data MigrateDirection = MigrateUp | MigrateDown
data Migration = Migration
  { id :: UserVersion
  , querySet :: QuerySet
  } deriving (Show)


createSetModifiedUtcTrigger :: Query
createSetModifiedUtcTrigger =
  "create trigger set_modified_utc_after_update\n\
        \after update on tasks\n\
        \when new.modified_utc is old.modified_utc\n\
        \begin\n\
        \  update tasks\n\
        \  set modified_utc = datetime('now')\n\
        \  where ulid = new.ulid;\n\
        \end"


createSetClosedUtcTrigger :: Query
createSetClosedUtcTrigger =
  "create trigger set_closed_utc_after_update\n\
        \after update on tasks\n\
        \when old.state is not new.state and (\n\
        \    new.state is 'Done'\n\
        \    or new.state is 'Obsolete'\n\
        \    or new.state is 'Deletable'\n\
        \  )\n\
        \begin\n\
        \  update tasks\n\
        \  set closed_utc = datetime('now')\n\
        \  where ulid = new.ulid;\n\
        \end"


_0_ :: MigrateDirection -> Migration
_0_ =
  let
    base = Migration
      { id = UserVersion 0
      , querySet = []
      }
  in \case
    MigrateUp -> base { Migrations.querySet =
      [ "create table tasks (\n\
        \  ulid text not null primary key,\n\
        \  body text not null,\n\
        \  state text check(state in ('Done','Obsolete','Deletable'))\n\
        \    not null default 'Done',\n\
        \  due_utc text,\n\
        \  closed_utc text,\n\
        \  modified_utc text not null,\n\
        \  priority_adjustment float,\n\
        \  metadata text\n\
        \)"

      , createSetModifiedUtcTrigger

      , createSetClosedUtcTrigger

      , "create table task_to_note (\n\
        \  ulid text not null primary key,\n\
        \  task_ulid text not null,\n\
        \  note text not null,\n\
        \  foreign key(task_ulid) references tasks(ulid)\n\
        \)"

      , "create table task_to_tag (\n\
        \  ulid text not null primary key,\n\
        \  task_ulid text not null,\n\
        \  tag text not null,\n\
        \  foreign key(task_ulid) references tasks(ulid),\n\
        \  constraint no_duplicate_tags unique (task_ulid, tag)\n\
        \)"
      ]}

    MigrateDown -> base { Migrations.querySet = [] }


-- | Add field "user"
_1_ :: MigrateDirection -> Migration
_1_ =
  let
    base = Migration
      { id = UserVersion 1
      , querySet = []
      }
  in \case
    MigrateUp -> base { Migrations.querySet =
          ["alter table tasks add column user text"]
      }

    -- TODO: Fix the invalid create table statement
    MigrateDown -> base { Migrations.querySet =
          [ "create table tasks_temp"
          , "insert into tasks_temp \
              \select ulid, body, state, due_utc, closed_utc, \
              \  modified_utc, priority_adjustment, metadata from tasks"
          , "drop table tasks"
          , "alter table tasks_temp rename to tasks"
        ]
      }


-- | Make state optional and add state "Deleted", add field "sleep_utc"
_2_ :: MigrateDirection -> Migration
_2_ =
  let
    base = Migration
      { id = UserVersion 2
      , querySet = []
      }
    createTempTableQueryUp = Query "\
      \create table tasks_temp ( \n\
      \  ulid text not null primary key, \n\
      \  body text not null, \n\
      \  state text check(state in (NULL, 'Done', 'Obsolete', 'Deleted')), \n\
      \  due_utc text, \n\
      \  sleep_utc text, \n\
      \  closed_utc text, \n\
      \  modified_utc text not null, \n\
      \  priority_adjustment float, \n\
      \  metadata text, \n\
      \  user text \n\
      \) \n\
      \"

    -- TODO: Finish query
    createTempTableQueryDown = Query "create table tasks_temp"

  in \case
    MigrateUp -> base { Migrations.querySet = (
        createTempTableQueryUp :
        "insert into tasks_temp \
          \select ulid, body, nullif(nullif(state,'Open'),'Waiting') as state, \
          \due_utc, NULL, closed_utc, \
          \modified_utc, priority_adjustment, metadata, user \
          \from tasks" :
        "drop table tasks" :
        "alter table tasks_temp rename to tasks" :
        [])
      }

    MigrateDown -> base { Migrations.querySet = (
        createTempTableQueryDown :
        "insert into tasks_temp \
          \select ulid, body, state, due_utc, closed_utc, \
          \  modified_utc, priority_adjustment, metadata, user from tasks" :
        "drop table tasks" :
        "alter table tasks_temp rename to tasks" :
        [])
      }


-- | Add fields awake_utc, ready_utc, waiting_utc, review_utc, closed_utc,
-- | group_ulid, repetition_duration, recurrence_duration,
_3_ :: MigrateDirection -> Migration
_3_ =
  let
    base = Migration
      { id = UserVersion 3
      , querySet = []
      }
    createTempTableQueryUp = Query "\
      \create table tasks_temp ( \n\
      \  ulid text not null primary key, \n\
      \  body text not null, \n\
      \  modified_utc text not null, \n\
      \  awake_utc text, \n\
      \  ready_utc text, \n\
      \  waiting_utc text, \n\
      \  review_utc text, \n\
      \  due_utc text, \n\
      \  closed_utc text, \n\
      \  state text check(state in (NULL, 'Done', 'Obsolete', 'Deletable')), \n\
      \  group_ulid text, \n\
      \  repetition_duration text, \n\
      \  recurrence_duration text, \n\
      \  priority_adjustment float, \n\
      \  user text, \n\
      \  metadata text \n\
      \) \n\
      \"

    -- TODO: Finish query
    createTempTableQueryDown = Query "create table tasks_temp"

  in \case
    MigrateUp -> base { Migrations.querySet = (
        createTempTableQueryUp :
        "insert into tasks_temp \
          \select ulid, body, modified_utc, sleep_utc, NULL, NULL, NULL, \
          \due_utc, closed_utc, state, NULL, NULL, NULL, \
          \priority_adjustment, user, metadata \
          \from tasks" :
        "drop table tasks" :
        "alter table tasks_temp rename to tasks" :
        createSetModifiedUtcTrigger :
        createSetClosedUtcTrigger :
        [])
      }

    MigrateDown -> base { Migrations.querySet = (
        createTempTableQueryDown :
        "insert into tasks_temp \
          \select ulid, body, state, due_utc, closed_utc, \
          \  modified_utc, priority_adjustment, metadata, user from tasks" :
        "drop table tasks" :
        "alter table tasks_temp rename to tasks" :
        [])
      }


_4_ :: MigrateDirection -> Migration
_4_ =
  let
    base = Migration
      { id = UserVersion 4
      , querySet = []
      }
  in \case
    MigrateUp -> base { Migrations.querySet =
      [ "create view tasks_view as\n\
        \select\n\
        \  tasks.ulid as ulid,\n\
        \  tasks.body as body,\n\
        \  tasks.modified_utc as modified_utc,\n\
        \  tasks.awake_utc as awake_utc,\n\
        \  tasks.ready_utc as ready_utc,\n\
        \  tasks.waiting_utc as waiting_utc,\n\
        \  tasks.review_utc as review_utc,\n\
        \  tasks.due_utc as due_utc,\n\
        \  tasks.closed_utc as closed_utc,\n\
        \  tasks.state as state,\n\
        \  tasks.group_ulid as group_ulid,\n\
        \  tasks.repetition_duration as repetition_duration,\n\
        \  tasks.recurrence_duration as recurrence_duration,\n\
        \  group_concat(distinct task_to_tag.tag) as tags,\n\
        \  group_concat(distinct task_to_note.note) as notes,\n\
        \  ifnull(tasks.priority_adjustment, 0.0)\n\
        \    + case\n\
        \        when awake_utc is null then 0.0\n\
        \        when awake_utc >= datetime('now') then -5.0\n\
        \        when awake_utc >= datetime('now', '-1 days') then 1.0\n\
        \        when awake_utc >= datetime('now', '-2 days') then 2.0\n\
        \        when awake_utc >= datetime('now', '-5 days') then 5.0\n\
        \        when awake_utc <  datetime('now', '-5 days') then 9.0\n\
        \      end \n\
        \    + case\n\
        \        when waiting_utc is null then 0.0\n\
        \        when waiting_utc >= datetime('now') then 0.0\n\
        \        when waiting_utc <  datetime('now') then -10.0\n\
        \      end \n\
        \    + case when review_utc is null then 0.0\n\
        \        when review_utc >= datetime('now') then 0.0\n\
        \        when review_utc <  datetime('now') then 20.0\n\
        \      end \n\
        \    + case\n\
        \        when due_utc is null then 0.0\n\
        \        when due_utc >= datetime('now', '+24 days') then 0.0\n\
        \        when due_utc >= datetime('now',  '+6 days') then 3.0\n\
        \        when due_utc >= datetime('now') then 6.0\n\
        \        when due_utc >= datetime('now',  '-6 days') then 9.0\n\
        \        when due_utc >= datetime('now', '-24 days') then 12.0\n\
        \        when due_utc <  datetime('now', '-24 days') then 15.0\n\
        \      end\n\
        \    + case\n\
        \        when state is null then 0.0\n\
        \        when state == 'Done' then 0.0\n\
        \        when state == 'Obsolete' then -1.0\n\
        \        when state == 'Deletable' then -10.0\n\
        \      end \n\
        \    + case count(task_to_note.note)\n\
        \        when 0 then 0.0\n\
        \        else 1.0\n\
        \      end\n\
        \    + case count(task_to_tag.tag)\n\
        \        when 0 then 0.0\n\
        \        else 2.0\n\
        \      end\n\
        \    as priority,\n\
        \  tasks.user as user,\n\
        \  tasks.metadata as metadata\n\
        \from\n\
        \  tasks\n\
        \  left join task_to_tag on tasks.ulid = task_to_tag.task_ulid\n\
        \  left join task_to_note on tasks.ulid = task_to_note.task_ulid\n\
        \group by tasks.ulid"

      , "create view tags as\n\
        \select\n\
        \  task_to_tag_1.tag,\n\
        \  (count(task_to_tag_1.tag) - ifnull(closed_count, 0)) as open,\n\
        \  ifnull(closed_count, 0) as closed,\n\
        \  round(cast(ifnull(closed_count, 0) as float) / \
        \    count(task_to_tag_1.tag), 6) as progress\n\
        \from\n\
        \  task_to_tag as task_to_tag_1\n\
        \  left join (\n\
        \      select tag, count(tasks.ulid) as closed_count\n\
        \      from tasks\n\
        \      left join task_to_tag\n\
        \      on tasks.ulid is task_to_tag.task_ulid\n\
        \      where closed_utc is not null\n\
        \      group by tag\n\
        \    ) as task_to_tag_2\n\
        \  on task_to_tag_1.tag is task_to_tag_2.tag\n\
        \group by task_to_tag_1.tag\n\
        \order by task_to_tag_1.tag asc"
      ] }
    MigrateDown -> base { Migrations.querySet = [] }


hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs) =
  x `elem` xs || hasDuplicates xs


wrapQuery :: UserVersion -> QuerySet -> QuerySet
wrapQuery (UserVersion userVersion) querySet =
  [ "pragma foreign_keys = OFF" ] <>
  querySet <>
  [ "pragma foreign_key_check"
  , "pragma user_version = " <> (Query $ show userVersion)
  ]


wrapMigration :: Migration -> Migration
wrapMigration migration =
  migration { querySet =
    wrapQuery (Migrations.id migration) (Migrations.querySet migration)
  }


lintQuery :: Query -> Either Text Query
lintQuery = Right
  -- TODO: Reactivate after
  --   https://github.com/JakeWheat/simple-sql-parser/issues/20 is fixed
  -- let
  --   queryStr = T.unpack $ fromQuery sqlQuery
  --   result = parseStatements ansi2011 "migration" Nothing queryStr
  -- in case result of
  --   Left error -> Left (show error)
  --   Right _ -> Right sqlQuery


lintMigration :: Migration -> Either Text Migration
lintMigration migration =
  either
    Left
    (\_ -> Right migration)
    (mapM lintQuery (Migrations.querySet migration))


runMigration :: Connection -> [Query] -> IO (Either SQLError [()])
runMigration connection querySet = do
  withTransaction connection $ do
    try $ mapM (execute_ connection) querySet
    -- Following doesn't work due to
    -- https://github.com/nurpax/sqlite-simple/issues/44
    -- try $ execute_ connection $ P.fold $ querySet <&> (<> ";\n")


runMigrations :: Config -> Connection -> IO (Doc ann)
runMigrations _ connection = do
  currentVersionList <- (query_ connection
    "pragma user_version" :: IO [UserVersion])

  let
    migrations = (
        _0_ :
        _1_ :
        _2_ :
        _3_ :
        _4_ :
      [])

    migrationsUp = fmap ($ MigrateUp) migrations
    (UserVersion userVersionMax) = migrationsUp
      <&> Migrations.id
      & P.maximum

    migrationsUpLinted :: Either Text [Migration]
    migrationsUpLinted = do
      currentVersion <- maybeToEither
        "'pragma user_verison' does not return current version"
        (P.head currentVersionList)

      -- Check if duplicate user versions are defined
      _ <- if migrationsUp <&> Migrations.id & hasDuplicates
          then Left "Your migrations contain duplicate user versions"
          else Right []

      -- Get new migrations, lint and wrap them
      migrationsUp
        & P.filter (\m ->
              (Migrations.id m) > currentVersion
              || ((Migrations.id m) == UserVersion 0)
                    && (currentVersion == UserVersion 0))
        <&> lintMigration
        <&> fmap wrapMigration
        & sequence

  case migrationsUpLinted of
    Left error -> pure $ pretty error
    Right [] -> pure ""
    Right migsUpLinted -> do
      result <- migsUpLinted
        <&> Migrations.querySet
        <&> runMigration connection
        & sequence

      case sequence result of
        Left error -> pure $ pretty (show error :: Text)
        Right _ -> do
          execute_ connection $
            Query $ "pragma user_version = " <> (show userVersionMax)
          pure $ (
            "Migration succeeded. New user-version: "
            <> (pretty userVersionMax))
            <> hardline
