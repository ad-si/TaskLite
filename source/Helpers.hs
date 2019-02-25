{-# LANGUAGE TypeFamilies #-}

module Helpers where

import Protolude as P hiding (get, put)

import Crypto.JOSE.JWS (CompactJWS)
import Crypto.JWT as Crypto hiding (param)
import Crypto.BCrypt
import Control.Lens
import Data.Aeson as Aeson (Value(..), toJSON, decode, object)
import Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.HashMap.Lazy (lookup)

import Data.Acid as Acid
import Data.Time
import Data.Time.Clock.POSIX
import Network.HTTP.Types.Status
import Web.Scotty as Scotty

import Database
import DbUser
import DbIdea
import PostIdea
import Types


toJsonError :: Text -> Value
toJsonError reason =
  object [("reason", String reason)]


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


validateAndAddIdea
  :: AcidState Database
  -> Text
  -> Either JWTError ClaimsSet
  -> Either Text PostIdea
  -> ActionM ()
validateAndAddIdea database emailAddress claimsResult ideaResult =
  case (claimsResult, ideaResult) of
    (Left error, _) -> do
      status status400
      json $ toJsonError $ show error

    (_, Left error) -> do
      status status400
      json $ toJsonError $ show error

    (Right _, Right verifiedIdea) -> do
      newId <- liftIO getId
      now <- liftIO getCurrentTime

      let
        dbIdea = DbIdea
          { id = newId
          , content = PostIdea.content verifiedIdea
          , impact = PostIdea.impact verifiedIdea
          , ease = PostIdea.ease verifiedIdea
          , confidence = PostIdea.confidence verifiedIdea
          , average_score = getAverageScore verifiedIdea
          , created_at = floor $ utcTimeToPOSIXSeconds now
          , created_by = emailAddress

          }

      _ <- liftIO $ update database $ AddIdea dbIdea

      status created201
      json $ DbIdea.toIdea dbIdea


-- TODO: Remove duplications with `validateAndAddIdea`
validateAndReplaceIdea
  :: AcidState Database
  -> Text
  -> Text
  -> Either JWTError ClaimsSet
  -> Either Text PostIdea
  -> ActionM ()
validateAndReplaceIdea database emailAddress id claimsResult ideaResult =
  case (claimsResult, ideaResult) of
    (Left error, _) -> do
      status status400
      json $ toJsonError $ show error

    (_, Left error) -> do
      status status400
      json $ toJsonError $ show error

    (Right _, Right verifiedIdea) -> do
      now <- liftIO getCurrentTime
      let
        dbIdea = DbIdea
          { id = id
          , content = PostIdea.content verifiedIdea
          , impact = PostIdea.impact verifiedIdea
          , ease = PostIdea.ease verifiedIdea
          , confidence = PostIdea.confidence verifiedIdea
          , average_score = getAverageScore verifiedIdea
          , created_at = floor $ utcTimeToPOSIXSeconds now
          , created_by = emailAddress
          }

      _ <- liftIO $ update database $ UpdateIdea id dbIdea

      status created201
      json $ DbIdea.toIdea dbIdea
