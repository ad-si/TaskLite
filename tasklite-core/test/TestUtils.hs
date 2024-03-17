module TestUtils where

import Protolude (
  IO,
  ($),
 )

import Database.SQLite.Simple qualified as Sql

import Config (Config (..))
import Migrations (runMigrations)


withMemoryDb :: Config -> (Sql.Connection -> IO a) -> IO a
withMemoryDb conf action =
  Sql.withConnection ":memory:" $ \memConn -> do
    _ <- runMigrations conf memConn
    action memConn
