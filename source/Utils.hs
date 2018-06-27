module Utils where

import Protolude as P

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Hourglass


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
    isoFormat = toFormat ("YYYYMMDDTHMIS" :: [Char])
    utcString = T.unpack utcText
  in
        (timeParse ISO8601_DateAndTime utcString)
    <|> (timeParse isoFormatSpace utcString)
    <|> (timeParse isoFormat utcString)
