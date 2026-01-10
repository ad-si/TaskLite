{-# LANGUAGE QuasiQuotes #-}

{-|
Migrations of SQLite database for new versions
-}
module Migrations where

import Protolude (
  Applicative (pure),
  Bool (False),
  Either (..),
  Eq ((==)),
  Foldable (elem),
  Functor (fmap),
  IO,
  Int,
  Ord ((>)),
  Read,
  Semigroup ((<>)),
  Show,
  Text,
  Traversable (mapM, sequence),
  maybeToEither,
  show,
  try,
  ($),
  (&),
  (&&),
  (<$>),
  (<&>),
  (||),
 )
import Protolude qualified as P

import Config (Config)
import Database.SQLite.Simple (
  Connection,
  FromRow (..),
  Query (Query),
  SQLError,
  execute_,
  field,
  query_,
  withTransaction,
 )
import Database.SQLite.Simple.QQ (sql)
import Prettyprinter (Doc, Pretty (pretty), hardline)


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
  }
  deriving (Show)


createSetModifiedUtcTrigger :: Query
createSetModifiedUtcTrigger =
  [sql|
    CREATE TRIGGER set_modified_utc_after_update
    AFTER UPDATE ON tasks
    WHEN new.modified_utc IS old.modified_utc  -- Must be `IS` to handle `NULL`
    BEGIN
      UPDATE tasks
      SET modified_utc = datetime('now')
      WHERE ulid == new.ulid;
    END
  |]


createSetClosedUtcTrigger :: Query
createSetClosedUtcTrigger =
  [sql|
    CREATE TRIGGER set_closed_utc_after_update
    AFTER UPDATE ON tasks
    WHEN
      old.state IS NOT new.state  -- Must be `IS NOT` to handle `NULL`
      AND (
        new.state == 'Done' OR
        new.state == 'Obsolete' OR
        new.state == 'Deletable'
      )
    BEGIN
      UPDATE tasks
      SET closed_utc = datetime('now')
      WHERE ulid == new.ulid;
    END
  |]


_0_ :: MigrateDirection -> Migration
_0_ =
  let
    base =
      Migration
        { id = UserVersion 0
        , querySet = []
        }
  in
    \case
      MigrateUp ->
        base
          { Migrations.querySet =
              [ [sql|
                  CREATE TABLE tasks (
                    ulid TEXT NOT NULL PRIMARY KEY,
                    body TEXT NOT NULL,
                    state TEXT check(state IN ('Done','Obsolete','Deletable'))
                      NOT NULL DEFAULT 'Done',
                    due_utc TEXT,
                    closed_utc TEXT,
                    modified_utc TEXT NOT NULL,
                    priority_adjustment REAL,
                    metadata TEXT
                  )
                |]
              , createSetModifiedUtcTrigger
              , createSetClosedUtcTrigger
              , [sql|
                  CREATE TABLE task_to_note (
                    ulid TEXT NOT NULL PRIMARY KEY,
                    task_ulid TEXT NOT NULL,
                    note TEXT NOT NULL,
                    FOREIGN KEY(task_ulid) REFERENCES tasks(ulid)
                  )
                |]
              , [sql|
                  CREATE TABLE task_to_tag (
                    ulid TEXT NOT NULL PRIMARY KEY,
                    task_ulid TEXT NOT NULL,
                    tag TEXT NOT NULL,
                    FOREIGN KEY(task_ulid) REFERENCES tasks(ulid),
                    CONSTRAINT no_duplicate_tags UNIQUE (task_ulid, tag)
                  )
                |]
              ]
          }
      MigrateDown -> base{Migrations.querySet = []}


-- | Add field "user"
_1_ :: MigrateDirection -> Migration
_1_ =
  let
    base =
      Migration
        { id = UserVersion 1
        , querySet = []
        }
  in
    \case
      MigrateUp ->
        base
          { Migrations.querySet =
              [ "ALTER TABLE tasks\n\
                \ADD COLUMN user TEXT"
              ]
          }
      MigrateDown ->
        base
          { Migrations.querySet =
              [ [sql|
                  CREATE TABLE tasks_temp (
                    ulid TEXT NOT NULL PRIMARY KEY,
                    body TEXT NOT NULL,
                    state TEXT check(state IN ('Done','Obsolete','Deletable'))
                      NOT NULL DEFAULT 'Done',
                    due_utc TEXT,
                    closed_utc TEXT,
                    modified_utc TEXT NOT NULL,
                    priority_adjustment REAL,
                    metadata TEXT
                  )
                |]
              , [sql|
                  INSERT INTO tasks_temp
                  SELECT
                    ulid,
                    body,
                    state,
                    due_utc,
                    closed_utc,
                    modified_utc,
                    priority_adjustment,
                    metadata
                  FROM tasks
                |]
              , "DROP TABLE tasks"
              , "ALTER TABLE tasks_temp RENAME TO tasks"
              , createSetModifiedUtcTrigger
              , createSetClosedUtcTrigger
              ]
          }


-- | Make state optional and add state "Deleted", add field "sleep_utc"
_2_ :: MigrateDirection -> Migration
_2_ =
  let
    base =
      Migration
        { id = UserVersion 2
        , querySet = []
        }
    createTempTableQueryUp =
      [sql|
        CREATE TABLE tasks_temp (
          ulid TEXT NOT NULL PRIMARY KEY,
          body TEXT NOT NULL,
          state TEXT check(state IN (NULL, 'Done', 'Obsolete', 'Deleted')),
          due_utc TEXT,
          sleep_utc TEXT,
          closed_utc TEXT,
          modified_utc TEXT NOT NULL,
          priority_adjustment REAL,
          metadata TEXT,
          user TEXT
        )
      |]

    createTempTableQueryDown =
      [sql|
        CREATE TABLE tasks_temp (
          ulid TEXT NOT NULL PRIMARY KEY,
          body TEXT NOT NULL,
          state TEXT check(state IN ('Done', 'Obsolete', 'Deletable'))
            NOT NULL DEFAULT 'Done',
          due_utc TEXT,
          closed_utc TEXT,
          modified_utc TEXT NOT NULL,
          priority_adjustment REAL,
          metadata TEXT,
          user TEXT
        )
      |]
  in
    \case
      MigrateUp ->
        base
          { Migrations.querySet =
              [ createTempTableQueryUp
              , [sql|
                  INSERT INTO tasks_temp
                  SELECT
                    ulid,
                    body,
                    nullif(nullif(state, 'Open'), 'Waiting') AS state,
                    due_utc,
                    NULL,
                    closed_utc,
                    modified_utc,
                    priority_adjustment,
                    metadata,
                    user
                  FROM tasks
                |]
              , "DROP TABLE tasks"
              , "ALTER TABLE tasks_temp\n\
                \RENAME TO tasks"
              ]
          }
      MigrateDown ->
        base
          { Migrations.querySet =
              [ createTempTableQueryDown
              , [sql|
                  INSERT INTO tasks_temp
                  SELECT
                    ulid,
                    body,
                    CASE
                      WHEN state = 'Deleted' THEN 'Deletable'
                      WHEN state IS NULL THEN 'Done'
                      ELSE state
                    END AS state,
                    due_utc,
                    closed_utc,
                    modified_utc,
                    priority_adjustment,
                    metadata,
                    user
                  FROM tasks
                |]
              , "DROP TABLE tasks"
              , "ALTER TABLE tasks_temp RENAME TO tasks"
              , createSetModifiedUtcTrigger
              , createSetClosedUtcTrigger
              ]
          }


{-| Add fields awake_utc, ready_utc, waiting_utc, review_utc, closed_utc,
group_ulid, repetition_duration, recurrence_duration,
-}
_3_ :: MigrateDirection -> Migration
_3_ =
  let
    base =
      Migration
        { id = UserVersion 3
        , querySet = []
        }
    createTempTableQueryUp =
      [sql|
        CREATE TABLE tasks_temp (
          ulid TEXT NOT NULL PRIMARY KEY,
          body TEXT NOT NULL,
          modified_utc TEXT NOT NULL,
          awake_utc TEXT,
          ready_utc TEXT,
          waiting_utc TEXT,
          review_utc TEXT,
          due_utc TEXT,
          closed_utc TEXT,
          state TEXT check(state IN (NULL, 'Done', 'Obsolete', 'Deletable')),
          group_ulid TEXT,
          repetition_duration TEXT,
          recurrence_duration TEXT,
          priority_adjustment REAL,
          user TEXT,
          metadata TEXT
        )
      |]

    createTempTableQueryDown =
      [sql|
        CREATE TABLE tasks_temp (
          ulid TEXT NOT NULL PRIMARY KEY,
          body TEXT NOT NULL,
          state TEXT check(state IN (NULL, 'Done', 'Obsolete', 'Deleted')),
          due_utc TEXT,
          sleep_utc TEXT,
          closed_utc TEXT,
          modified_utc TEXT NOT NULL,
          priority_adjustment REAL,
          metadata TEXT,
          user TEXT
        )
      |]
  in
    \case
      MigrateUp ->
        base
          { Migrations.querySet =
              [ createTempTableQueryUp
              , [sql|
                  INSERT INTO tasks_temp
                  SELECT
                    ulid,
                    body,
                    modified_utc,
                    sleep_utc,
                    NULL,
                    NULL,
                    NULL,
                    due_utc,
                    closed_utc,
                    state,
                    NULL,
                    NULL,
                    NULL,
                    priority_adjustment,
                    user,
                    metadata
                  FROM tasks
                |]
              , "DROP TABLE tasks"
              , "ALTER TABLE tasks_temp RENAME TO tasks"
              , createSetModifiedUtcTrigger
              , createSetClosedUtcTrigger
              ]
          }
      MigrateDown ->
        base
          { Migrations.querySet =
              [ createTempTableQueryDown
              , [sql|
                  INSERT INTO tasks_temp
                  SELECT
                    ulid,
                    body,
                    CASE
                      WHEN state = 'Deletable' THEN 'Deleted'
                      ELSE state
                    END AS state,
                    due_utc,
                    awake_utc AS sleep_utc,
                    closed_utc,
                    modified_utc,
                    priority_adjustment,
                    metadata,
                    user
                  FROM tasks
                |]
              , "DROP TABLE tasks"
              , "ALTER TABLE tasks_temp RENAME TO tasks"
              , createSetModifiedUtcTrigger
              , createSetClosedUtcTrigger
              ]
          }


-- | View definition for tasks_view in migration 4 (using group_concat)
tasksViewQuery_v4 :: Query
tasksViewQuery_v4 =
  [sql|
    CREATE VIEW tasks_view AS
    SELECT
      tasks.ulid AS ulid,
      tasks.body AS body,
      tasks.modified_utc AS modified_utc,
      tasks.awake_utc AS awake_utc,
      tasks.ready_utc AS ready_utc,
      tasks.waiting_utc AS waiting_utc,
      tasks.review_utc AS review_utc,
      tasks.due_utc AS due_utc,
      tasks.closed_utc AS closed_utc,
      tasks.state AS state,
      tasks.group_ulid AS group_ulid,
      tasks.repetition_duration AS repetition_duration,
      tasks.recurrence_duration AS recurrence_duration,
      group_concat(DISTINCT task_to_tag.tag) AS tags,
      group_concat(DISTINCT task_to_note.note) AS notes,
      ifnull(tasks.priority_adjustment, 0.0)
        + CASE
            WHEN awake_utc IS NULL THEN 0.0
            WHEN awake_utc >= datetime('now') THEN -5.0
            WHEN awake_utc >= datetime('now', '-1 days') THEN 1.0
            WHEN awake_utc >= datetime('now', '-2 days') THEN 2.0
            WHEN awake_utc >= datetime('now', '-5 days') THEN 5.0
            WHEN awake_utc < datetime('now', '-5 days') THEN 9.0
          END
        + CASE
            WHEN waiting_utc IS NULL THEN 0.0
            WHEN waiting_utc >= datetime('now') THEN 0.0
            WHEN waiting_utc < datetime('now') THEN -10.0
          END
        + CASE
            WHEN review_utc IS NULL THEN 0.0
            WHEN review_utc >= datetime('now') THEN 0.0
            WHEN review_utc < datetime('now') THEN 20.0
          END
        + CASE
            WHEN due_utc IS NULL THEN 0.0
            WHEN due_utc >= datetime('now', '+24 days') THEN 0.0
            WHEN due_utc >= datetime('now', '+6 days') THEN 3.0
            WHEN due_utc >= datetime('now') THEN 6.0
            WHEN due_utc >= datetime('now', '-6 days') THEN 9.0
            WHEN due_utc >= datetime('now', '-24 days') THEN 12.0
            WHEN due_utc < datetime('now', '-24 days') THEN 15.0
          END
        + CASE
            WHEN state IS NULL THEN 0.0
            WHEN state == 'Done' THEN 0.0
            WHEN state == 'Obsolete' THEN -1.0
            WHEN state == 'Deletable' THEN -10.0
          END
        + CASE count(task_to_note.note)
            WHEN 0 THEN 0.0
            ELSE 1.0
          END
        + CASE count(task_to_tag.tag)
            WHEN 0 THEN 0.0
            ELSE 2.0
          END
        AS priority,
      tasks.user AS user,
      tasks.metadata AS metadata
    FROM
      tasks
      LEFT JOIN task_to_tag ON tasks.ulid == task_to_tag.task_ulid
      LEFT JOIN task_to_note ON tasks.ulid == task_to_note.task_ulid
    GROUP BY tasks.ulid
  |]


_4_ :: MigrateDirection -> Migration
_4_ =
  let
    base =
      Migration
        { id = UserVersion 4
        , querySet = []
        }
  in
    \case
      MigrateUp ->
        base
          { Migrations.querySet =
              [ tasksViewQuery_v4
              , [sql|
                  CREATE VIEW tags AS
                  SELECT
                    task_to_tag_1.tag,
                    (count(task_to_tag_1.tag) - ifnull(closed_count, 0))
                      AS "open",
                    ifnull(closed_count, 0) AS closed,
                    round(
                      cast(ifnull(closed_count, 0) AS REAL) /
                        count(task_to_tag_1.tag),
                      6
                    ) AS progress
                  FROM
                    task_to_tag AS task_to_tag_1
                    LEFT JOIN (
                      SELECT tag, count(tasks.ulid) AS closed_count
                      FROM tasks
                      LEFT JOIN task_to_tag
                      ON tasks.ulid IS task_to_tag.task_ulid
                      WHERE closed_utc IS NOT NULL
                      GROUP BY tag
                    ) AS task_to_tag_2
                    ON task_to_tag_1.tag IS task_to_tag_2.tag
                  GROUP BY task_to_tag_1.tag
                  ORDER BY task_to_tag_1.tag ASC
                |]
              ]
          }
      MigrateDown ->
        base
          { Migrations.querySet =
              [ "DROP VIEW IF EXISTS tags"
              , "DROP VIEW IF EXISTS tasks_view"
              ]
          }


_5_ :: MigrateDirection -> Migration
_5_ =
  let
    base =
      Migration
        { id = UserVersion 5
        , querySet = []
        }
  in
    \case
      MigrateUp ->
        base
          { Migrations.querySet =
              [ "DROP VIEW IF EXISTS tasks_view"
              , [sql|
                  CREATE VIEW tasks_view AS
                  SELECT
                    tasks.ulid AS ulid,
                    tasks.body AS body,
                    tasks.modified_utc AS modified_utc,
                    tasks.awake_utc AS awake_utc,
                    tasks.ready_utc AS ready_utc,
                    tasks.waiting_utc AS waiting_utc,
                    tasks.review_utc AS review_utc,
                    tasks.due_utc AS due_utc,
                    tasks.closed_utc AS closed_utc,
                    tasks.state AS state,
                    tasks.group_ulid AS group_ulid,
                    tasks.repetition_duration AS repetition_duration,
                    tasks.recurrence_duration AS recurrence_duration,
                    nullif(
                      (
                        SELECT json_group_array(task_to_tag.tag)
                        FROM task_to_tag
                        WHERE task_to_tag.task_ulid == tasks.ulid
                      ),
                      '[]'
                    ) AS tags,
                    nullif(
                      (
                        SELECT json_group_array(
                          json_object('ulid', task_to_note.ulid, 'body', task_to_note.note)
                        )
                        FROM task_to_note
                        WHERE task_to_note.task_ulid == tasks.ulid
                      ),
                      '[]'
                    ) AS notes,
                    ifnull(tasks.priority_adjustment, 0.0)
                      + CASE
                          WHEN awake_utc IS NULL THEN 0.0
                          WHEN awake_utc >= datetime('now') THEN -5.0
                          WHEN awake_utc >= datetime('now', '-1 days') THEN 1.0
                          WHEN awake_utc >= datetime('now', '-2 days') THEN 2.0
                          WHEN awake_utc >= datetime('now', '-5 days') THEN 5.0
                          WHEN awake_utc < datetime('now', '-5 days') THEN 9.0
                        END
                      + CASE
                          WHEN waiting_utc IS NULL THEN 0.0
                          WHEN waiting_utc >= datetime('now') THEN 0.0
                          WHEN waiting_utc < datetime('now') THEN -10.0
                        END
                      + CASE
                          WHEN review_utc IS NULL THEN 0.0
                          WHEN review_utc >= datetime('now') THEN 0.0
                          WHEN review_utc < datetime('now') THEN 20.0
                        END
                      + CASE
                          WHEN due_utc IS NULL THEN 0.0
                          WHEN due_utc >= datetime('now', '+24 days') THEN 0.0
                          WHEN due_utc >= datetime('now', '+6 days') THEN 3.0
                          WHEN due_utc >= datetime('now') THEN 6.0
                          WHEN due_utc >= datetime('now', '-6 days') THEN 9.0
                          WHEN due_utc >= datetime('now', '-24 days') THEN 12.0
                          WHEN due_utc < datetime('now', '-24 days') THEN 15.0
                        END
                      + CASE
                          WHEN state IS NULL THEN 0.0
                          WHEN state == 'Done' THEN 0.0
                          WHEN state == 'Obsolete' THEN -1.0
                          WHEN state == 'Deletable' THEN -10.0
                        END
                      + CASE
                          WHEN (
                            SELECT count(task_to_note.note)
                            FROM task_to_note
                            WHERE task_to_note.task_ulid == tasks.ulid
                          ) == 0 THEN 0.0
                          ELSE 1.0
                        END
                      + CASE
                          WHEN (
                            SELECT count(task_to_tag.tag)
                            FROM task_to_tag
                            WHERE task_to_tag.task_ulid == tasks.ulid
                          ) == 0 THEN 0.0
                          ELSE 2.0
                        END
                      AS priority,
                    tasks.user AS user,
                    tasks.metadata AS metadata
                  FROM
                    tasks
                |]
              ]
          }
      MigrateDown ->
        base
          { Migrations.querySet =
              [ "DROP VIEW IF EXISTS tasks_view"
              , tasksViewQuery_v4
              ]
          }


-- | Migration 6: Add SQL views for list commands
_6_ :: MigrateDirection -> Migration
_6_ =
  let
    base =
      Migration
        { id = UserVersion 6
        , querySet = []
        }
  in
    \case
      MigrateUp ->
        base
          { Migrations.querySet =
              [ -- tasks_open: All open tasks by priority
                [sql|
                  CREATE VIEW tasks_open AS
                  SELECT *
                  FROM tasks_view
                  WHERE closed_utc IS NULL
                  ORDER BY
                    priority DESC,
                    due_utc ASC,
                    ulid DESC
                |]
              , -- tasks_overdue: Open tasks past their due date
                [sql|
                  CREATE VIEW tasks_overdue AS
                  SELECT *
                  FROM tasks_view
                  WHERE
                    closed_utc IS NULL
                    AND due_utc < datetime('now')
                  ORDER BY
                    priority DESC,
                    due_utc ASC,
                    ulid DESC
                |]
              , -- tasks_done: Completed tasks
                [sql|
                  CREATE VIEW tasks_done AS
                  SELECT *
                  FROM tasks_view
                  WHERE
                    closed_utc IS NOT NULL
                    AND state == 'Done'
                  ORDER BY closed_utc DESC
                |]
              , -- tasks_obsolete: Obsolete tasks
                [sql|
                  CREATE VIEW tasks_obsolete AS
                  SELECT *
                  FROM tasks_view
                  WHERE
                    closed_utc IS NOT NULL
                    AND state == 'Obsolete'
                  ORDER BY ulid DESC
                |]
              , -- tasks_deletable: Tasks marked for deletion
                [sql|
                  CREATE VIEW tasks_deletable AS
                  SELECT *
                  FROM tasks_view
                  WHERE
                    closed_utc IS NOT NULL
                    AND state == 'Deletable'
                  ORDER BY ulid DESC
                |]
              , -- tasks_waiting: Tasks waiting for external input
                [sql|
                  CREATE VIEW tasks_waiting AS
                  SELECT *
                  FROM tasks_view
                  WHERE
                    closed_utc IS NULL
                    AND waiting_utc IS NOT NULL
                    AND (review_utc > datetime('now') OR review_utc IS NULL)
                  ORDER BY waiting_utc DESC
                |]
              , -- tasks_ready: Tasks ready to be worked on
                [sql|
                  CREATE VIEW tasks_ready AS
                  SELECT *
                  FROM tasks_view
                  WHERE
                    closed_utc IS NULL
                    AND (
                      review_utc <= datetime('now')
                      OR ready_utc <= datetime('now')
                      OR (
                        ready_utc IS NULL
                        AND (awake_utc IS NULL OR awake_utc <= datetime('now'))
                        AND (waiting_utc IS NULL OR waiting_utc > datetime('now'))
                        AND (review_utc IS NULL OR review_utc > datetime('now'))
                      )
                    )
                  ORDER BY
                    priority DESC,
                    due_utc ASC,
                    ulid DESC
                |]
              , -- tasks_repeating: Tasks with repetition duration set
                [sql|
                  CREATE VIEW tasks_repeating AS
                  SELECT *
                  FROM tasks_view
                  WHERE repetition_duration IS NOT NULL
                  ORDER BY repetition_duration DESC
                |]
              , -- tasks_recurring: Tasks with recurrence duration set
                [sql|
                  CREATE VIEW tasks_recurring AS
                  SELECT *
                  FROM tasks_view
                  WHERE recurrence_duration IS NOT NULL
                  ORDER BY recurrence_duration DESC
                |]
              , -- tasks_new: All tasks by newest first (ULID descending)
                [sql|
                  CREATE VIEW tasks_new AS
                  SELECT *
                  FROM tasks_view
                  ORDER BY ulid DESC
                |]
              , -- tasks_old: Oldest open tasks first
                [sql|
                  CREATE VIEW tasks_old AS
                  SELECT *
                  FROM tasks_view
                  WHERE closed_utc IS NULL
                  ORDER BY ulid ASC
                |]
              , -- tasks_all: All tasks by creation order
                [sql|
                  CREATE VIEW tasks_all AS
                  SELECT *
                  FROM tasks_view
                  ORDER BY ulid ASC
                |]
              , -- tasks_notag: Open tasks without any tags
                [sql|
                  CREATE VIEW tasks_notag AS
                  SELECT *
                  FROM tasks_view
                  WHERE
                    closed_utc IS NULL
                    AND tags IS NULL
                  ORDER BY
                    priority DESC,
                    due_utc ASC,
                    ulid DESC
                |]
              , -- tasks_modified: All tasks by modification time
                [sql|
                  CREATE VIEW tasks_modified AS
                  SELECT *
                  FROM tasks_view
                  ORDER BY modified_utc DESC
                |]
              ]
          }
      MigrateDown ->
        base
          { Migrations.querySet =
              [ "DROP VIEW IF EXISTS tasks_open"
              , "DROP VIEW IF EXISTS tasks_overdue"
              , "DROP VIEW IF EXISTS tasks_done"
              , "DROP VIEW IF EXISTS tasks_obsolete"
              , "DROP VIEW IF EXISTS tasks_deletable"
              , "DROP VIEW IF EXISTS tasks_waiting"
              , "DROP VIEW IF EXISTS tasks_ready"
              , "DROP VIEW IF EXISTS tasks_repeating"
              , "DROP VIEW IF EXISTS tasks_recurring"
              , "DROP VIEW IF EXISTS tasks_new"
              , "DROP VIEW IF EXISTS tasks_old"
              , "DROP VIEW IF EXISTS tasks_all"
              , "DROP VIEW IF EXISTS tasks_notag"
              , "DROP VIEW IF EXISTS tasks_modified"
              ]
          }


hasDuplicates :: (Eq a) => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x : xs) =
  x `elem` xs || hasDuplicates xs


wrapQuery :: UserVersion -> QuerySet -> QuerySet
wrapQuery (UserVersion userVersion) querySet =
  ["PRAGMA foreign_keys = OFF"]
    <> querySet
    <> [ "PRAGMA foreign_key_check"
       , "PRAGMA user_version = " <> Query (show userVersion)
       ]


wrapMigration :: Migration -> Migration
wrapMigration migration =
  migration
    { querySet =
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
  migration
    & Migrations.querySet
    & mapM lintQuery
      <&> P.const migration


runMigration :: Connection -> [Query] -> IO (Either SQLError [()])
runMigration connection querySet = do
  withTransaction connection $ do
    try $ mapM (execute_ connection) querySet


-- Following doesn't work due to
-- https://github.com/nurpax/sqlite-simple/issues/44
-- try $ execute_ connection $ P.fold $ querySet <&> (<> ";\n")

runMigrations :: Config -> Connection -> IO (Doc ann)
runMigrations _ connection = do
  currentVersionList <-
    query_
      connection
      "PRAGMA user_version" ::
      IO [UserVersion]

  let
    migrations = [_0_, _1_, _2_, _3_, _4_, _5_, _6_]

    migrationsUp = fmap ($ MigrateUp) migrations
    (UserVersion userVersionMax) =
      migrationsUp
        <&> Migrations.id
        & P.maximum

    migrationsUpLinted :: Either Text [Migration]
    migrationsUpLinted = do
      currentVersion <-
        maybeToEither
          "`PRAGMA user_version` does not return current version"
          (P.head currentVersionList)

      -- Check if duplicate user versions are defined
      _ <-
        if migrationsUp <&> Migrations.id & hasDuplicates
          then Left "Your migrations contain duplicate user versions"
          else Right []

      -- Get new migrations, lint and wrap them
      migrationsUp
        & P.filter
          ( \m ->
              Migrations.id m > currentVersion
                || (Migrations.id m == UserVersion 0)
                  && (currentVersion == UserVersion 0)
          )
          <&> lintMigration
        & mapM (fmap wrapMigration)

  case migrationsUpLinted of
    Left error -> pure $ pretty error
    Right [] -> pure ""
    Right migsUpLinted -> do
      result <-
        migsUpLinted
          <&> Migrations.querySet
          & mapM (runMigration connection)

      case sequence result of
        Left error -> pure $ pretty (show error :: Text)
        Right _ -> do
          execute_ connection $
            Query $
              "PRAGMA user_version = " <> show userVersionMax
          pure $
            ( "Migration succeeded. New user-version: "
                <> pretty userVersionMax
            )
              <> hardline
