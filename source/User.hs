module User where

import Protolude hiding (get, put)

import Data.Aeson (ToJSON, Value(..))
import Data.Monoid (mconcat)
import Data.Text as T
import GHC.Generics
import Network.HTTP.Types.Status
import Web.Scotty
import AccessToken


data User = User
  { email :: Text
  , name :: Text
  , avatar_url :: Text
  } deriving (Generic)

instance ToJSON User
