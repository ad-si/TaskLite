import Protolude (IO, ($))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (hspec)

import CliSpec qualified


main :: IO ()
main = do
  withSystemTempDirectory "tasklite-test" $ \dirPath -> do
    hspec $ CliSpec.spec dirPath
