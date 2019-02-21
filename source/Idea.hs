module Idea where

import Protolude hiding (get, put)

import Data.Aeson (ToJSON, Value(..))
import Data.Monoid (mconcat)
import Data.Text as T
import GHC.Generics
import Network.HTTP.Types.Status
import Web.Scotty
import AccessToken
import User


data Idea = Idea
  { id            :: Text
  , content       :: Text
  , impact        :: Int
  , ease          :: Int
  , confidence    :: Int
  , average_score :: Float
  , created_at    :: Integer
  } deriving (Show, Generic)

instance ToJSON Idea
