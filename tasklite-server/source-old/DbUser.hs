{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module DbUser where

import Protolude hiding (get, put)

import Data.Aeson (FromJSON, ToJSON)
import Data.SafeCopy

import Types


data DbUser = DbUser
  { name :: Text
  , email :: Text
  , password_hash :: Text
  , refresh_token :: Maybe RefreshToken
  } deriving (Show, Generic)

instance ToJSON DbUser
instance FromJSON DbUser

$(deriveSafeCopy 0 'base ''DbUser)
