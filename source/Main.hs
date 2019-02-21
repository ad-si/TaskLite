module Main where

import Protolude hiding (get, put)

import Data.Aeson (ToJSON, FromJSON, Value(..))
import Data.Monoid (mconcat)
import Data.Text as T
import GHC.Generics
import Network.HTTP.Types.Status
import Web.Scotty

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


app :: ScottyM ()
app = do
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
    json $ User
      { email = ""  -- TODO
      , name = ""  -- TODO
      , avatar_url = ""  -- TODO
      }


  post   "/users" $ do
    (user :: SignupUser) <- jsonData
    status created201
    json $ AccessToken
      { jwt = ""  -- TODO
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
main = scotty 3000 app
