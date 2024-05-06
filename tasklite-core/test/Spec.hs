import Protolude (IO, show, ($))

import Data.Hourglass (DateTime, Time (timeFromElapsedP))
import Data.Text (unpack)
import Database.SQLite.Simple qualified as Sql
import Test.Hspec (
  SpecWith,
  describe,
  hspec,
  it,
  shouldStartWith,
 )
import Time.System (timeCurrentP)

import Config (Config (..), defaultConfig)
import ImportExportSpec qualified
import LibSpec qualified
import Migrations (runMigrations)
import TypesSpec qualified
import UtilsSpec qualified


testSuite :: Config -> DateTime -> SpecWith ()
testSuite conf now = do
  describe "TaskLite" $ do
    it "creates tables on initial run migrates tables to latest version" $ do
      Sql.withConnection ":memory:" $ \memConn -> do
        migrationStatus <- runMigrations conf memConn
        unpack (show migrationStatus) `shouldStartWith` "Migration succeeded"

  UtilsSpec.spec
  TypesSpec.spec
  LibSpec.spec conf now
  ImportExportSpec.spec conf


main :: IO ()
main = do
  nowElapsed <- timeCurrentP
  let now = timeFromElapsedP nowElapsed :: DateTime
  hspec $ testSuite defaultConfig now
