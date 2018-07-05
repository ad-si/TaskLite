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
    isoFormatSpace = toFormat ("YYYY-MM-DD H:MI:S" :: [Char])
    isoFormatNoSec = toFormat ("YYYY-MM-DD H:MI" :: [Char])
    isoDate        = toFormat ("YYYY-MM-DD" :: [Char])
    isoFormat      = toFormat ("YYYYMMDDTHMIS" :: [Char])
    utcString = T.unpack utcText
  in
        (timeParse ISO8601_DateAndTime utcString)
    <|> (timeParse isoFormatSpace utcString)
    <|> (timeParse isoFormatNoSec utcString)
    <|> (timeParse isoDate utcString)
    <|> (timeParse isoFormat utcString)


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
