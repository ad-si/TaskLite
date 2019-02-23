module PostIdea where

import Protolude hiding (get, put)

import Data.Aeson (ToJSON, FromJSON)
import Data.Text as T


data PostIdea = PostIdea
  { content    :: Text
  , impact     :: Int
  , ease       :: Int
  , confidence :: Int
  } deriving (Show, Generic)

instance FromJSON PostIdea
instance ToJSON PostIdea


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
getAverageScore idea = 1.337
