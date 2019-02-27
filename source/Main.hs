{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Protolude as P hiding (get, put)

import Crypto.JWT as Crypto hiding (param)
import Control.Lens
import Data.Acid as Acid
import Data.Aeson as Aeson (Value(..), object, eitherDecode)
import Data.List.Extra as List (chunksOf)
import Data.String (fromString)
import Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
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
  post "/access-tokens/refresh" $ do
    fullBody <- body

    case (eitherDecode fullBody & over _Left T.pack) of
      Left errorMessage -> badRequest errorMessage
      Right refreshToken -> do
        userResult <- liftIO $ query database $ GetUserByToken refreshToken

        case userResult of
          Left errorMessage -> notFoundAction errorMessage
          Right dbUser -> refreshTokenToWebToken dbUser refreshToken


  -- User Login
  post "/access-tokens" $ do
    fullBody <- body

    case (eitherDecode fullBody & over _Left T.pack) of
      Left errorMessage -> badRequest errorMessage
      Right loginUser -> do
        newToken <- liftIO getRefreshToken
        loginResult <- liftIO $ update database $ LogUserIn
          (Types.email loginUser)
          (Types.password loginUser)
          newToken

        case loginResult of
          Left (statusCode, errorMessage) -> do
            status statusCode
            json $ toJsonError errorMessage
          Right dbUser -> do
            (DbUser.refresh_token dbUser)
            <&> refreshTokenToAccessToken dbUser
            & fromMaybe (status internalServerError500)


  -- User Logout
  delete "/access-tokens" $ do
    jwtBSMaybe <- Scotty.header "x-access-token"

    runIfRegisteredUser database jwtBSMaybe
      (\emailAddress jwkValue jwtValue -> do
        claimsResult <- liftIO $
          doJwtVerify emailAddress jwkValue jwtValue

        case claimsResult of
          Left error -> badRequest $ show error
          Right _ -> do
            fullBody <- body

            case (eitherDecode fullBody & over _Left T.pack) of
              Left errorMessage -> badRequest errorMessage
              Right refreshToken -> do
                userResult <- liftIO $ query database
                  $ GetUserByToken refreshToken

                case userResult of
                  Left errorMessage -> notFoundAction errorMessage
                  Right _ -> do
                    logoutResult <- liftIO $ update database
                          $ LogoutByEmailAndToken emailAddress refreshToken

                    case logoutResult of
                      Left (statusCode, errorMessage) -> do
                        status statusCode
                        json $ toJsonError errorMessage
                      Right _ -> do
                        status noContent204
                        json $ Object mempty
      )


  -- User Signup
  post "/users" $ do
    fullBody <- body

    case (eitherDecode fullBody & over _Left T.pack) of
      Left errorMessage -> badRequest errorMessage
      Right signupUser -> do
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
  get "/admin/users" $ do
    users <- liftIO $ query database GetUsers
    json users


  -- Get Current User's Info
  get "/me" $ do
    jwtBSMaybe <- Scotty.header "x-access-token"

    runIfRegisteredUser database jwtBSMaybe
      (\emailAddress jwkValue jwtValue -> do
        claimsResult <- liftIO $
          doJwtVerify emailAddress jwkValue jwtValue

        case claimsResult of
          Left error -> badRequest $ show error
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
  post "/ideas" $ do
    jwtBSMaybe <- Scotty.header "x-access-token"
    fullBody <- body
    let postIdeaResult = eitherDecode fullBody & over _Left T.pack

    runIfRegisteredUser database jwtBSMaybe
      (\emailAddress jwkValue jwtValue -> do
        claimsResult <- liftIO $
          doJwtVerify emailAddress jwkValue jwtValue
        let verifiedIdea = postIdeaResult >>= verifyIdea
        validateAndAddIdea
          database
          emailAddress
          claimsResult
          verifiedIdea
      )


  -- Get all Ideas
  get "/admin/ideas" $ do
    ideas <- liftIO $ query database GetIdeas
    json ideas


  -- Get all ideas of current user
  get "/ideas" $ do
    jwtBSMaybe <- Scotty.header "x-access-token"
    page <- (param "page" `rescue` (\_ -> pure 1))

    runIfRegisteredUser database jwtBSMaybe
      (\emailAddress jwkValue jwtValue -> do
        claimsResult <- liftIO $
          doJwtVerify emailAddress jwkValue jwtValue

        case claimsResult of
          Left error -> badRequest $ show error
          Right _ -> do
            ideas <- liftIO $
              query database $ GetIdeasByEmail emailAddress
            let
              ideasPerPage = 10
              ideasByScoreDesc = ideas & sortBy
                (\a b -> compare
                          (DbIdea.average_score b)
                          (DbIdea.average_score a))

            if (page :: Int) < 1
            then badRequest "Page number must be > 0"
            else do
              json $ fromMaybe [] $
                (List.chunksOf ideasPerPage ideasByScoreDesc)
                ^? element (page - 1)
                <&> (<&> DbIdea.toIdea)
      )


  -- Update Idea
  put    "/ideas/:id" $ do
    jwtBSMaybe <- Scotty.header "x-access-token"
    id <- param "id"
    fullBody <- body

    case (eitherDecode fullBody & over _Left T.pack) of
      Left errorMessage -> badRequest errorMessage
      Right postIdea ->
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
          Left error -> badRequest $ show error
          Right _ -> do
            deletionResult <- liftIO $ update database
              $ DeleteIdeaIfBy emailAddress id
            case deletionResult of
              Left (statusCode, errorMesage) -> do
                status statusCode
                json $ toJsonError errorMesage
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
