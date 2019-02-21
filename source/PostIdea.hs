module PostIdea where

import Protolude hiding (get, put)

import Data.Aeson (ToJSON, FromJSON, Value(..))
import Data.Monoid (mconcat)
import Data.Text as T
import GHC.Generics
import Network.HTTP.Types.Status
import Web.Scotty


data PostIdea = PostIdea
  { content    :: Text
  , impact     :: Int
  , ease       :: Int
  , confidence :: Int
  } deriving (Show, Generic)

instance FromJSON PostIdea
instance ToJSON PostIdea
