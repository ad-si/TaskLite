{-# LANGUAGE TypeFamilies #-}

module Helpers where

import Protolude as P hiding (get, put)

import Crypto.JOSE.JWS (CompactJWS)
import Crypto.JWT as Crypto hiding (param)
import Crypto.BCrypt
import Control.Lens
import Data.Aeson as Aeson (Value(..), toJSON, decode)
import Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.HashMap.Lazy (lookup)

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


getAudienceFromJWT :: TL.Text -> Either Text Text
getAudienceFromJWT jwtBS =
  let
    jwtValue :: Either Error (CompactJWS JWSHeader)
    jwtValue = decodeCompact (TL.encodeUtf8 jwtBS)
    -- Misuse `ToJSON` instance due to lack of normal payload accessor
    objectValue = toJSON $ jwtValue ^? _Right
  in
    case objectValue of
      (Object obj) ->
        let
          stringValMaybe = lookup "payload" obj

          strToMaybe = \case
            (Just (String s)) -> Just s
            _                 -> Nothing

          payloadMaybe =
            (strToMaybe stringValMaybe)
            <&> P.encodeUtf8
            >>= preview base64url
            >>= Aeson.decode
        in
          case payloadMaybe of
            Just (Object payloadObj) ->
              let emailMaybe = strToMaybe $ lookup "aud" payloadObj
              in note "Payload does not contain an audience" emailMaybe
            _ -> Left $ "JWT does not contain a payload"

      _ -> Left "JWT payload is not an object"
