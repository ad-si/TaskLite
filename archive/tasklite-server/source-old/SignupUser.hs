{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module SignupUser where

import Protolude hiding (get, put)

import Data.Aeson (FromJSON, ToJSON)
import Data.SafeCopy

import DbUser
import Helpers


data SignupUser = SignupUser
  { name :: Text
  , email :: Text
  , password :: Text
  }
  deriving (Show, Generic)


instance ToJSON SignupUser
instance FromJSON SignupUser


$(deriveSafeCopy 0 'base ''SignupUser)


toDbUser :: SignupUser -> IO DbUser
toDbUser (SignupUser name email password) =
  credentialsToDbUser name email password
