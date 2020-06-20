{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module DbIdea where

import Protolude hiding (get, put)

import Data.Aeson (ToJSON)
import Data.SafeCopy
import Idea

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


toIdea :: DbIdea -> Idea
toIdea dbIdea =
  Idea
    { Idea.id = (DbIdea.id dbIdea)
    , Idea.content = (DbIdea.content dbIdea)
    , Idea.impact = (DbIdea.impact dbIdea)
    , Idea.ease = (DbIdea.ease dbIdea)
    , Idea.confidence = (DbIdea.confidence dbIdea)
    , Idea.average_score = (DbIdea.average_score dbIdea)
    , Idea.created_at = (DbIdea.created_at dbIdea)
    }

