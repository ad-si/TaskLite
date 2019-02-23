{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module DbIdea where

import Protolude hiding (get, put)

import Data.Aeson (ToJSON)
import Data.SafeCopy


data DbIdea = DbIdea
  { id            :: Text
  , content       :: Text
  , impact        :: Int
  , ease          :: Int
  , confidence    :: Int
  , average_score :: Float
  , created_at    :: Integer
  , created_by    :: Text  -- email
  } deriving (Show, Generic)

instance ToJSON DbIdea

$(deriveSafeCopy 0 'base ''DbIdea)
