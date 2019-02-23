{-# LANGUAGE TypeFamilies #-}

module Main where

import Protolude as P hiding (get, put)

import Crypto.JWT hiding (param, header)
import Control.Lens.Setter ((?~))
import Data.Acid as Acid
import Data.Aeson (Value(..), toJSON, object)
import Data.String (fromString)
import Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Time
import Data.Time.Clock.POSIX
import Network.HTTP.Types.Status
import Web.Scotty

import AccessToken
import Database
import DbUser
import DbIdea
import Idea
import PostIdea
import SignupUser
import Types


toJsonError :: Text -> Value
toJsonError reason =
  object [("reason", String reason)]


makeClaims :: SignupUser -> IO ClaimsSet
makeClaims user = do
  now <- getCurrentTime
  pure $ emptyClaimsSet
    & (claimIss ?~ fromString (T.unpack $ SignupUser.name user))
    & (claimIat ?~ NumericDate now)
    & addClaim "id" "123"


doJwtSign :: JWK -> ClaimsSet -> IO (Either JWTError SignedJWT)
doJwtSign jwkValue claims = runExceptT $ do
  minimalHeader <- makeJWSHeader jwkValue
  let jwtHeader = minimalHeader & (typ ?~ HeaderParam () "JWT")
  signClaims jwkValue jwtHeader claims


doJwtVerify :: JWK -> SignedJWT -> IO (Either JWTError ClaimsSet)
doJwtVerify jwkValue jwt = runExceptT $ do
  let config = defaultJWTValidationSettings (== "bob")
  verifyClaims config jwkValue jwt


app :: AcidState Database -> ScottyM ()
app database = do
  -- Tokens
  post   "/access-tokens/refresh" $ do
    refreshToken <- jsonData
    userMaybe <- liftIO $ query database $ GetUserByToken refreshToken

    case userMaybe of
      Nothing -> do
        status notFound404
        json $ toJsonError
          "A user with the provided refresh-token does not exist"
      Just _ -> do
        newToken <- liftIO getRefreshToken
        liftIO $ update database $ SetTokenWhere refreshToken newToken
        json $ object [("refresh_token", toJSON newToken)]


  post   "/access-tokens" $ do
    loginUser <- jsonData
    newToken <- liftIO getRefreshToken
    result <- liftIO $ update database $ LogUserIn
      (Types.email loginUser)
      (Types.password loginUser)
      newToken
    case result of
      Left errorMessage -> do
        status notFound404
        json $ toJsonError errorMessage
      Right user -> do
        -- Must exist as user verification was successfull
        let refreshToken = fromMaybe
              (RefreshToken "Not possible") $ DbUser.refresh_token user
        status created201
        json $ AccessToken
          { jwt = ""  -- TODO
          , refresh_token = refreshToken
          }


  delete "/access-tokens" $ do
    refreshToken <- jsonData
    userMaybe <- liftIO $ query database $ GetUserByToken refreshToken

    case userMaybe of
      Nothing -> do
        status notFound404
        json $ toJsonError
          "A user with the provided refresh-token does not exist"
      Just _ -> do
        _ <- liftIO $ update database $ LogoutUserByToken refreshToken
        status noContent204
        json $ Object mempty


  -- Users
  post   "/users" $ do
    (signupUser :: SignupUser) <- jsonData
    dbUser <- liftIO $ toDbUser signupUser
    _ <- liftIO $ update database $ AddUser dbUser
    claims <- liftIO $ makeClaims signupUser
    jwkValue <- genJWK (OctGenParam 256 {-bytes-})
    signedJwtEither <- liftIO $ doJwtSign jwkValue claims
    case signedJwtEither of
      Left error -> liftIO $ die $ show error
      Right jwt -> do
        status created201
        json $ AccessToken
          { jwt = TL.toStrict $ TL.decodeUtf8 $ encodeCompact jwt
          , refresh_token = RefreshToken ""  -- TODO
          }


  get    "/users" $ do
    users <- liftIO $ query database GetUsers
    json users


  get    "/me" $ do
    accessTokenMaybe <- header "x-access-token"

    case accessTokenMaybe of
      Nothing -> do
        status notFound404
        json $ toJsonError "An access token must be provided"
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


  -- Ideas
  post   "/ideas" $ do
    (postIdea :: PostIdea) <- jsonData

    case verifyIdea postIdea of
      Left message -> do
        status unprocessableEntity422
        json $ toJsonError $ "Idea contains invalid data: " <> message

      Right verifiedIdea -> do
        newId <- liftIO getId
        now <- liftIO getCurrentTime

        let
          dbIdea = DbIdea
            { id = newId
            , content = PostIdea.content verifiedIdea
            , impact = PostIdea.impact verifiedIdea
            , ease = PostIdea.ease verifiedIdea
            , confidence = PostIdea.confidence verifiedIdea
            , average_score = getAverageScore postIdea
            , created_at = floor $ utcTimeToPOSIXSeconds now
            , created_by = "TODO"
            }

        _ <- liftIO $ update database $ AddIdea dbIdea

        status created201
        json dbIdea


  get    "/ideas" $ do
    ideas <- liftIO $ query database GetIdeas
    json ideas


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


  delete "/ideas/:id" $ do
    (id :: Text) <- param "id"
    status noContent204
    json $ Object mempty


main :: IO ()
main = do
  database <- openLocalStateFrom "my-idea-pool-db/" (Database [] [])
  scotty 3000 $ app database
