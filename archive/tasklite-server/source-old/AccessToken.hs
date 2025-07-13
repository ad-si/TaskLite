module AccessToken where

import Protolude hiding (get, put)

import Data.Aeson (ToJSON)

import Types


data AccessToken = AccessToken
  { jwt :: Text
  , refresh_token :: RefreshToken
  }
  deriving (Show, Generic)


instance ToJSON AccessToken
