import Protolude (IO)
import Test.Hspec (hspec)

import CliSpec qualified


main :: IO ()
main = do
  hspec CliSpec.spec
