module MigrationsSpec where

import Protolude (show, ($))

import Data.Text (unpack)
import Database.SQLite.Simple qualified as Sql
import Test.Hspec (
  Spec,
  it,
  shouldStartWith,
 )

import Config (defaultConfig)
import Migrations (runMigrations)


spec :: Spec
spec = do
  it "creates tables on initial run migrates tables to latest version" $ do
    Sql.withConnection ":memory:" $ \memConn -> do
      migrationStatus <- runMigrations defaultConfig memConn
      unpack (show migrationStatus) `shouldStartWith` "Migration succeeded"
