{-# LANGUAGE TypeFamilies #-}

module Helpers where

import Protolude hiding (get, put)

import Crypto.BCrypt
import Data.Text.Encoding as T

import Types
import DbUser


loginToPartialDbUser :: LoginUser -> IO DbUser
loginToPartialDbUser (LoginUser email password) =
  credentialsToDbUser "" email password


credentialsToDbUser :: Text ->  Text -> Text -> IO DbUser
credentialsToDbUser name email password = do
  newHash <- hashPasswordUsingPolicy
            HashingPolicy
              { preferredHashCost = 10
              , preferredHashAlgorithm = "$2b$"
              }
            (T.encodeUtf8 password)

  refresh_token <- getRefreshToken

  pure $ DbUser
    { password_hash = T.decodeUtf8 $ fromMaybe "" newHash
    , refresh_token = Just refresh_token
    , ..
    }
