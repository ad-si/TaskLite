{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Protolude as P hiding (get, put)

import Crypto.JWT as Crypto hiding (param)
import Control.Lens
import Data.Acid as Acid
import Data.Aeson as Aeson (Value(..), toJSON, object)
import Data.String (fromString)
import Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
-- import Data.ByteString.Lazy.Internal as BL
import Data.Time
import Data.Time.Clock.POSIX
import Network.HTTP.Types.Status
import Web.Scotty as Scotty

import AccessToken
import Database
import DbUser
import DbIdea
import Idea
import PostIdea
import SignupUser
import Types
import Helpers


toJsonError :: Text -> Value
toJsonError reason =
  object [("reason", String reason)]


makeClaims :: DbUser -> IO ClaimsSet
makeClaims user = do
  now <- getCurrentTime
  pure $ emptyClaimsSet
    & (claimIss ?~ fromString "my-idea-pool")
    & (claimAud ?~ Audience [fromString $ T.unpack $ DbUser.email user])
    & (claimIat ?~ NumericDate now)
    & addClaim "id" "123"
    -- TODO: Add expire utc


doJwtSign :: JWK -> ClaimsSet -> IO (Either JWTError SignedJWT)
doJwtSign jwkValue claims = runExceptT $ do
  minimalHeader <- makeJWSHeader jwkValue
  let jwtHeader = minimalHeader & (typ ?~ HeaderParam () "JWT")
  signClaims jwkValue jwtHeader claims


doJwtVerify :: Text -> JWK -> SignedJWT -> IO (Either JWTError ClaimsSet)
doJwtVerify audEmailAddress jwkValue jwt = runExceptT $ do
  let
    config = defaultJWTValidationSettings
      (== (fromString $ T.unpack audEmailAddress))
  verifyClaims config jwkValue jwt


unauthorizedError :: ActionM ()
unauthorizedError = do
  status unauthorized401
  json $ toJsonError "An access token must be provided"


processValidationResults
  :: AcidState Database
  -> Text
  -> Either JWTError ClaimsSet
  -> Either Text PostIdea
  -> ActionM ()
processValidationResults database emailAddress claimsResult ideaResult =
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
      json dbIdea


runIfRegisteredUser
  :: AcidState Database
  -> Maybe TL.Text
  -> (Text -> JWK -> CompactJWS JWSHeader -> ActionM ())
  -> ActionM ()
runIfRegisteredUser database jwtBSMaybe callback =
  case jwtBSMaybe of
    Nothing -> unauthorizedError
    Just jwtBS -> do
      case getAudienceFromJWT jwtBS of
        Left errorMessage -> do
          status status400
          json $ toJsonError errorMessage
        Right emailAddress -> do
          userMaybe <- liftIO $ query database $ GetUserByEmail emailAddress

          case userMaybe of
            Nothing -> do
              status status400
              json $ toJsonError "User does not exist"
            Just user -> do
              let refreshToken = DbUser.refresh_token user

              case refreshToken of
                Nothing ->  do
                  status status400
                  json $ toJsonError "User is not logged in"
                Just refToken -> do
                  let
                    jwkValue = refreshTokenToJwk refToken

                    jwtResult :: Either Error (CompactJWS JWSHeader)
                    jwtResult = decodeCompact $ TL.encodeUtf8 jwtBS

                  case jwtResult of
                    Left error -> do
                      status status400
                      json $ toJsonError $ show error
                    Right jwtValue -> do
                      callback emailAddress jwkValue jwtValue


app :: AcidState Database -> ScottyM ()
app database = do
  -- Refresh JWT
  post   "/access-tokens/refresh" $ do
    refreshToken <- jsonData
    userMaybe <- liftIO $ query database $ GetUserByToken refreshToken

    case userMaybe of
      Nothing -> do
        status notFound404
        json $ toJsonError
          "A user with the provided refresh token does not exist"
      Just _ -> do
        newToken <- liftIO getRefreshToken
        liftIO $ update database $ SetTokenWhere refreshToken newToken
        json $ object [("refresh_token", toJSON newToken)]


  -- User Login
  post   "/access-tokens" $ do
    loginUser <- jsonData
    newToken <- liftIO getRefreshToken
    result <- liftIO $ update database $ LogUserIn
      (Types.email loginUser)
      (Types.password loginUser)
      newToken

    case (result :: Either Text DbUser) of
      Left errorMessage -> do
        status notFound404
        json $ toJsonError errorMessage
      Right dbUser -> do
        (DbUser.refresh_token dbUser)
        <&> (\refreshToken -> do
              let jwkValue = refreshTokenToJwk refreshToken
              claims <- liftIO $ makeClaims dbUser

              signedJwtEither <- liftIO $ doJwtSign jwkValue claims

              case signedJwtEither of
                Left error -> liftIO $ die $ show error
                Right jwt -> do
                  status created201
                  json $ AccessToken
                    { jwt = TL.toStrict $ TL.decodeUtf8 $ encodeCompact jwt
                    , refresh_token = refreshToken
                    }
            )
        & fromMaybe (status internalServerError500)


  -- User Logout
  delete "/access-tokens" $ do
    refreshToken <- jsonData
    userMaybe <- liftIO $ query database $ GetUserByToken refreshToken

    case userMaybe of
      Nothing -> unauthorizedError
      Just _ -> do
        _ <- liftIO $ update database $ LogoutUserByToken refreshToken
        status noContent204
        json $ Object mempty


  -- User Signup
  post   "/users" $ do
    signupUser <- jsonData
    dbUser <- liftIO $ toDbUser signupUser
    _ <- liftIO $ update database $ AddUser dbUser
    let
      refreshToken = fromMaybe (RefreshToken "Not possible")
        $ DbUser.refresh_token dbUser
      jwkValue = refreshTokenToJwk refreshToken

    claims <- liftIO $ makeClaims dbUser
    signedJwtEither <- liftIO $ doJwtSign jwkValue claims

    case signedJwtEither of
      Left error -> liftIO $ die $ show error
      Right jwt -> do
        status created201
        json $ AccessToken
          { jwt = TL.toStrict $ TL.decodeUtf8 $ encodeCompact jwt
          , refresh_token = RefreshToken ""  -- TODO
          }


  -- Get All Registered Users
  get    "/users" $ do
    users <- liftIO $ query database GetUsers
    json users


  -- Get Current User's Info
  get    "/me" $ do
    accessTokenMaybe <- Scotty.header "x-access-token"

    case accessTokenMaybe of
      Nothing -> unauthorizedError
      Just accessToken -> do
        userMaybe <- liftIO $ query database $
          GetUserByToken (RefreshToken $ toStrict accessToken)

        case userMaybe of
          Nothing -> do
            status notFound404
            json $ toJsonError
              "A user with the provided refresh-token does not exist"
          Just user -> json $ object
            [ ("email", String $ DbUser.email user)
            , ("name", String $ DbUser.name user)
            , ("avatar_url", String $ DbUser.email user) -- TODO
            ]


  -- Add Idea
  post   "/ideas" $ do
    jwtBSMaybe <- Scotty.header "x-access-token"
    postIdea <- jsonData

    runIfRegisteredUser database jwtBSMaybe
      (\emailAddress jwkValue jwtValue -> do
        claimsResult <- liftIO $
          doJwtVerify emailAddress jwkValue jwtValue
        let verifiedIdea = verifyIdea postIdea
        processValidationResults
          database
          emailAddress
          claimsResult
          verifiedIdea
      )


  -- Get all Ideas
  get    "/ideas" $ do
    ideas <- liftIO $ query database GetIdeas
    json ideas


  -- Update Idea
  put    "/ideas/:id" $ do
    (idea :: PostIdea) <- jsonData

    json $ Idea
      { id = ""  -- TODO
      , content = PostIdea.content idea
      , impact = PostIdea.impact idea
      , ease = PostIdea.ease idea
      , confidence = PostIdea.confidence idea
      , average_score = 0  -- TODO
      , created_at = 0  -- TODO
      }


  -- Delete an Idea
  delete "/ideas/:id" $ do
    jwtBSMaybe <- Scotty.header "x-access-token"
    id <- param "id"

    runIfRegisteredUser database jwtBSMaybe
      (\emailAddress jwkValue jwtValue -> do
        claimsResult <- liftIO $
          doJwtVerify emailAddress jwkValue jwtValue
        case claimsResult of
          Left errorMessage -> do
            status status400
            json $ toJsonError $ show errorMessage

          Right _ -> do
            deletionResult <- liftIO $ update database $ DeleteIdea id
            case deletionResult of
              Left errorMessage -> do
                status status400
                json $ toJsonError $ show errorMessage
              Right _ -> do
                status noContent204
                json $ Object mempty
      )


main :: IO ()
main = do
  database <- openLocalStateFrom "my-idea-pool-db/" (Database [] [])
  scotty 3000 $ app database
