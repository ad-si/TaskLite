module Idea where

import Protolude hiding (get, put)

import Data.Aeson (ToJSON)


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
