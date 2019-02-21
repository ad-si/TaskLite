module AccessToken where

import Protolude hiding (get, put)

import Data.Aeson (ToJSON, Value(..))
import Data.Monoid (mconcat)
import Data.Text as T
import GHC.Generics
import Web.Scotty


data AccessToken = AccessToken
  { jwt :: Text
  , refresh_token :: Text
  }
  deriving (Show, Generic)

instance ToJSON AccessToken
