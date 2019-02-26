{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies #-}

module Types where

import Protolude as P hiding (get, put)

import Control.Lens.Combinators
import Crypto.JOSE.JWK
import Crypto.JOSE.Types
import Data.Aeson (ToJSON, FromJSON, Value(..), toJSON)
import Data.SafeCopy
import Web.Scotty


newtype Config = Config { ideasPerPage :: Int }

defaultConfig :: Config
defaultConfig = Config
  { ideasPerPage = 10
  }



newtype RefreshToken = RefreshToken
  { refresh_token :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON RefreshToken
instance ToJSON RefreshToken where
  toJSON (RefreshToken tokenText) = String tokenText

instance Parsable RefreshToken where
  parseParam txt = Right $ RefreshToken $ show txt

$(deriveSafeCopy 0 'base ''RefreshToken)


refreshTokenToJwk :: RefreshToken -> JWK
refreshTokenToJwk (RefreshToken refreshToken) =
  let
    bytes = fromMaybe "Can't encode as base64 URL" $
      preview base64url $ P.encodeUtf8 refreshToken
  in
    fromKeyMaterial $ OctKeyMaterial $ OctKeyParameters $ Base64Octets bytes


textToJwt :: RefreshToken -> JWK
textToJwt (RefreshToken refreshToken) =
  let
    bytes = fromMaybe "Can't encode as base64 URL" $
      preview base64url $ P.encodeUtf8 refreshToken
  in
    fromKeyMaterial $ OctKeyMaterial $ OctKeyParameters $ Base64Octets bytes


getRefreshToken :: IO RefreshToken
getRefreshToken = do
  OctKeyMaterial (OctKeyParameters (Base64Octets bytes)) <-
    genKeyMaterial (OctGenParam 64)
  pure $ RefreshToken $ (P.decodeUtf8 . review base64url) bytes


getId :: IO Text
getId = do
  OctKeyMaterial (OctKeyParameters (Base64Octets bytes)) <-
    genKeyMaterial (OctGenParam 16)
  pure $ (P.decodeUtf8 . review base64url) bytes



newtype WebToken = WebToken
  { jwt :: Text
  } deriving (Show, Generic)

instance ToJSON WebToken


data LoginUser = LoginUser
  { email :: Text
  , password :: Text
  } deriving (Show, Generic)

instance FromJSON LoginUser

$(deriveSafeCopy 0 'base ''LoginUser)
