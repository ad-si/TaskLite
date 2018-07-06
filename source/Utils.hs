{-|
Several utility functions (e.g for parsing & serializing UTC stamps)
-}

module Utils where

import Protolude as P

import Codec.Crockford as Crock
import Data.Text as T
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Hourglass


type IdText = Text
type TagText = Text

data Filter a = NoFilter | Only a
  deriving (Eq, Ord, Show)


(<++>) :: Doc ann -> Doc ann -> Doc ann
x <++> y =
  x <> softline <> softline <> y


(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) =
  flip (<$>)


parseUtc :: Text -> Maybe DateTime
parseUtc utcText =
  let
    utcString = unpack $ T.toLower utcText

    tParse :: [Char] -> Maybe DateTime
    tParse formatString =
      timeParse (toFormat formatString) utcString
  in
    -- From long (specific) to short (unspecific)
        (timeParse ISO8601_DateAndTime utcString)
    <|> (tParse "YYYY-MM-DDtH:MI")
    <|> (tParse "YYYYMMDDtHMIS")
    <|> (tParse "YYYY-MM-DD H:MI:S")
    <|> (tParse "YYYY-MM-DD H:MI")
    <|> (tParse "YYYY-MM-DD")


ulidToDateTime :: Text -> Maybe DateTime
ulidToDateTime =
  (fmap $
    timeGetDateTimeOfDay
    . Elapsed
    . (`div` 1000)
  )
  . Crock.decode
  . unpack
  . T.take 10
