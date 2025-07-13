module User where

import Protolude hiding (get, put)

import Data.Aeson (ToJSON)


data User = User
  { email :: Text
  , name :: Text
  , avatar_url :: Text
  }
  deriving (Show, Generic)


instance ToJSON User
