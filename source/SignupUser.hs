{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module SignupUser where

import Protolude hiding (get, put)

import Data.Aeson (FromJSON, ToJSON, Value(..))
import Data.Monoid (mconcat)
import Data.SafeCopy
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

instance ToJSON SignupUser
instance FromJSON SignupUser

$(deriveSafeCopy 0 'base ''SignupUser)
