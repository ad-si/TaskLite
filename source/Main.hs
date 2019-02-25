{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Protolude as P hiding (get, put)

import Crypto.JWT as Crypto hiding (param)
import Control.Lens
import Data.Acid as Acid
import Data.Aeson as Aeson (Value(..), object)
import Data.List.Extra as List (chunksOf)
import Data.String (fromString)
import Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
-- import Data.ByteString.Lazy.Internal as BL
import Data.Time
import Network.HTTP.Types.Status
import Network.Gravatar
import System.Environment (getEnv)
import Web.Scotty as Scotty

import AccessToken
import Database
import DbUser
import DbIdea
import PostIdea
import SignupUser
import Types
import Helpers


makeClaims :: DbUser -> IO ClaimsSet
makeClaims user = do
  now <- getCurrentTime
  pure $ emptyClaimsSet
    & (claimIss ?~ fromString "my-idea-pool")
    & (claimAud ?~ Audience [fromString $ T.unpack $ DbUser.email user])
    & (claimIat ?~ NumericDate now)
    & (claimExp ?~ (NumericDate $ (fromRational 600 {-sec-}) `addUTCTime` now))


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


-- TODO: Use EitherT stack to avoid pyramid of doom
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


-- TODO: Remove duplication with `refreshTokenToAccessToken`
refreshTokenToWebToken :: DbUser -> RefreshToken -> ActionM ()
refreshTokenToWebToken dbUser refreshToken = do
  let jwkValue = refreshTokenToJwk refreshToken
  claims <- liftIO $ makeClaims dbUser
  signedJwtEither <- liftIO $ doJwtSign jwkValue claims

  case signedJwtEither of
    Left error -> liftIO $ die $ show error
    Right jwt -> do
      status created201
      json $ WebToken { jwt = TL.toStrict $ TL.decodeUtf8 $ encodeCompact jwt }


refreshTokenToAccessToken :: DbUser -> RefreshToken -> ActionM ()
refreshTokenToAccessToken dbUser refreshToken = do
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


app :: AcidState Database -> ScottyM ()
app database = do
  defaultHandler (\error -> do
      json $ toJsonError $ toStrict $ error
    )

  -- Refresh JWT
  post   "/access-tokens/refresh" $ do
    refreshToken <- jsonData
    userMaybe <- liftIO $ query database $ GetUserByToken refreshToken

    case userMaybe of
      Nothing -> do
        status notFound404
        json $ toJsonError
          "A user with the provided refresh token does not exist"
      Just dbUser -> do
        refreshTokenToWebToken dbUser refreshToken


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
        <&> refreshTokenToAccessToken dbUser
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
          , refresh_token = refreshToken
          }


  -- Get All Registered Users
  get    "/users" $ do
    users <- liftIO $ query database GetUsers
    json users


  -- Get Current User's Info
  get    "/me" $ do
    jwtBSMaybe <- Scotty.header "x-access-token"

    runIfRegisteredUser database jwtBSMaybe
      (\emailAddress jwkValue jwtValue -> do
        claimsResult <- liftIO $
          doJwtVerify emailAddress jwkValue jwtValue

        case claimsResult of
          Left errorMessage -> do
            status status400
            json $ toJsonError $ show errorMessage

          Right _ -> do
            userMaybe <- liftIO $ query database $ GetUserByEmail emailAddress
            json $ userMaybe <&> (\user -> object
                [ ("email", String $ DbUser.email user)
                , ("name", String $ DbUser.name user)
                , ("avatar_url", String $ T.pack $
                    gravatar (def :: GravatarOptions) $ DbUser.email user)
                ]
              )
      )


  -- Add Idea
  post   "/ideas" $ do
    jwtBSMaybe <- Scotty.header "x-access-token"
    postIdea <- jsonData

    runIfRegisteredUser database jwtBSMaybe
      (\emailAddress jwkValue jwtValue -> do
        claimsResult <- liftIO $
          doJwtVerify emailAddress jwkValue jwtValue
        let verifiedIdea = verifyIdea postIdea
        validateAndAddIdea
          database
          emailAddress
          claimsResult
          verifiedIdea
      )


  -- Get all Ideas
  get    "/admin/ideas" $ do
    ideas <- liftIO $ query database GetIdeas
    json ideas


  -- Get all ideas of current user
  get    "/ideas" $ do
    jwtBSMaybe <- Scotty.header "x-access-token"
    page <- param "page"

    runIfRegisteredUser database jwtBSMaybe
      (\emailAddress jwkValue jwtValue -> do
        claimsResult <- liftIO $
          doJwtVerify emailAddress jwkValue jwtValue

        case claimsResult of
          Left error -> do
            status status400
            json $ toJsonError $ show error

          Right _ -> do
            ideas <- liftIO $
              query database $ GetIdeasByEmail emailAddress
            let ideasPerPage = 10

            if (page :: Int) < 1
            then do
              status status400
              json $ toJsonError "Page number must be > 0"
            else do
              json $ (List.chunksOf ideasPerPage ideas)
                ^? element (page - 1)
                <&> (<&> DbIdea.toIdea)
      )


  -- Update Idea
  put    "/ideas/:id" $ do
    jwtBSMaybe <- Scotty.header "x-access-token"
    id <- param "id"
    postIdea <- jsonData

    runIfRegisteredUser database jwtBSMaybe
      (\emailAddress jwkValue jwtValue -> do
        claimsResult <- liftIO $
          doJwtVerify emailAddress jwkValue jwtValue
        let verifiedIdea = verifyIdea postIdea
        validateAndReplaceIdea
          database
          emailAddress
          id
          claimsResult
          verifiedIdea
      )


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


  notFound $
    json $ toJsonError "This endpoint does not exist"



main :: IO ()
main = do
  portMaybe <- getEnv "PORT" <&> readMaybe

  case portMaybe of
    Nothing -> die "Port must be a number"
    Just port -> do
      database <- openLocalStateFrom "my-idea-pool-db/" (Database [] [])
      scotty port $ app database
