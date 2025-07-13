module PostIdea where

import Protolude hiding (get, put)

import Data.Aeson (FromJSON, ToJSON)
import Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified DbIdea as DbIdea


data PostIdea = PostIdea
  { content :: Text
  , impact :: Int
  , ease :: Int
  , confidence :: Int
  }
  deriving (Show, Generic)


instance FromJSON PostIdea
instance ToJSON PostIdea


toDbIdea :: Text -> Text -> UTCTime -> PostIdea -> DbIdea.DbIdea
toDbIdea id emailAddress now postIdea =
  DbIdea.DbIdea
    { DbIdea.id = id
    , DbIdea.content = PostIdea.content postIdea
    , DbIdea.impact = PostIdea.impact postIdea
    , DbIdea.ease = PostIdea.ease postIdea
    , DbIdea.confidence = PostIdea.confidence postIdea
    , DbIdea.average_score = getAverageScore postIdea
    , DbIdea.created_at = floor $ utcTimeToPOSIXSeconds now
    , DbIdea.created_by = emailAddress
    }


verifyIdea :: PostIdea -> Either Text PostIdea
verifyIdea idea =
  if
    | T.length (content idea) > 255 ->
        Left "Content must be less than 256 characters long"
    | impact idea < 1 -> Left "Impact must be greater than 0"
    | impact idea > 10 -> Left "Impact must be lower than 11"
    | ease idea < 1 -> Left "Ease must be greater than 0"
    | ease idea > 10 -> Left "Ease must be lower than 11"
    | confidence idea < 1 -> Left "Confidence must be greater than 0"
    | confidence idea > 10 -> Left "Confidence must be lower than 11"
    | otherwise -> Right idea


getAverageScore :: PostIdea -> Float
getAverageScore idea =
  ( (fromIntegral $ impact idea)
      + (fromIntegral $ ease idea)
      + (fromIntegral $ confidence idea)
  )
    / 3
