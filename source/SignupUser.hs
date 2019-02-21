module SignupUser where

import Protolude hiding (get, put)

import Data.Aeson (FromJSON, Value(..))
import Data.Monoid (mconcat)
import Data.Text as T
import GHC.Generics
import Network.HTTP.Types.Status
import Web.Scotty
import AccessToken
import User


data SignupUser = SignupUser
  { name :: Text
  , email :: Text
  , password :: Text
  } deriving (Show, Generic)

instance FromJSON SignupUser
