{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Protolude hiding (get, put)

import Crypto.JWT hiding (param)
import Control.Lens.Setter ((.~))
import Control.Lens (view)
import qualified Control.Monad.State as State
import Data.Acid as Acid
import Data.Aeson (ToJSON, FromJSON, Value(..), toJSON)
import Data.Monoid (mconcat)
import Data.SafeCopy
import Data.String (fromString)
import Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Time
import GHC.Generics
import Network.HTTP.Types.Status
import Test.QuickCheck (arbitrary, Gen(..))
import Web.Scotty
import Web.Scotty.Internal.Types
import AccessToken
import Idea
import PostIdea
import User
import SignupUser


newtype Config = Config { ideasPerPage :: Int }

defaultConfig :: Config
defaultConfig = Config
  { ideasPerPage = 10
  }


newtype RefreshToken = RefreshToken { refresh_token :: Text }
  deriving (Show, Generic)

instance FromJSON RefreshToken
instance Parsable RefreshToken where
  parseParam txt = Right $ RefreshToken $ show txt


newtype WebToken = WebToken { jwt :: Text }
  deriving (Show, Generic)

instance ToJSON WebToken


newtype PageRecord = PageRecord { page :: Text }
  deriving (Show, Generic)

instance ToJSON PageRecord


data LoginUser = LoginUser
  { email :: Text
  , password :: Text
  } deriving (Show, Generic)

instance FromJSON LoginUser


data Database = Database [SignupUser]

$(deriveSafeCopy 0 'base ''Database)


addUser :: SignupUser -> Update Database ()
addUser user = do
  Database users <- State.get
  State.put $ Database (user : users)

getUsers :: Query Database [SignupUser]
getUsers = do
  Database users <- ask
  pure users

$(makeAcidic ''Database ['addUser, 'getUsers])


makeClaims :: SignupUser -> IO ClaimsSet
makeClaims user = do
  utc <- getCurrentTime
  pure $ emptyClaimsSet
    & claimIss .~ Just (fromString $ T.unpack $ SignupUser.name user)
    & claimIat .~ Just (NumericDate utc)
    & addClaim "id" "123"


doJwtSign :: JWK -> ClaimsSet -> IO (Either JWTError SignedJWT)
doJwtSign jwk claims = runExceptT $ do
  minimalHeader <- makeJWSHeader jwk
  let header = minimalHeader & typ .~ Just (HeaderParam () "JWT")
  signClaims jwk header claims


doJwtVerify :: JWK -> SignedJWT -> IO (Either JWTError ClaimsSet)
doJwtVerify jwk jwt = runExceptT $ do
  let config = defaultJWTValidationSettings (== "bob")
  verifyClaims config jwk jwt


app :: AcidState Database -> ScottyM ()
app database = do
  -- Tokens
  post   "/access-tokens/refresh" $ do
    (refreshToken :: RefreshToken) <- jsonData
    json $ WebToken ""  -- TODO


  post   "/access-tokens" $ do
    (user :: LoginUser) <- jsonData
    status created201
    json $ AccessToken
      { jwt = ""  -- TODO
      , refresh_token = ""  -- TODO
      }


  delete "/access-tokens" $ do
    (refreshToken :: RefreshToken) <- jsonData
    status noContent204
    json $ Object mempty


  -- Users
  get    "/me" $ do
    users <- liftIO $ query database GetUsers
    putText "All users:"
    mapM_ putStrLn [ "  " ++ (show user) | user <- users ]

    json users


  post   "/users" $ do
    (user :: SignupUser) <- jsonData
    _ <- liftIO $ update database $ AddUser user
    claims <- liftIO $ makeClaims user
    jwk <- genJWK (OctGenParam 256 {-bytes-})
    print $ toJSON $ view jwkMaterial jwk
    signedJwtEither <- liftIO $ doJwtSign jwk claims
    case signedJwtEither of
      Left error -> liftIO $ die $ show error
      Right jwt -> do
        status created201
        json $ AccessToken
          { jwt = TL.toStrict $ TL.decodeUtf8 $ encodeCompact jwt
          , refresh_token = ""  -- TODO
          }


  -- Ideas
  get    "/ideas" $ do
    (page :: Int) <- param "page"
    json ([] :: [Idea])  -- TODO


  post   "/ideas" $ do
    (postIdea :: PostIdea) <- jsonData

    status created201
    json $ Idea
      { id = ""  -- TODO
      , content = PostIdea.content postIdea
      , impact = PostIdea.impact postIdea
      , ease = PostIdea.ease postIdea
      , confidence = PostIdea.confidence postIdea
      , average_score = 0  -- TODO
      , created_at = 0  -- TODO
      }


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
  database <- openLocalStateFrom "my-idea-pool-db/" (Database [])
  scotty 3000 $ app database
