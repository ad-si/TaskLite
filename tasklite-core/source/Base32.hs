-- | Adapted from https://hackage.haskell.org/package/crockford
module Base32 (decode) where

import Data.Char as C (Char, toUpper)
import Data.Text as T (Text, unpack)
import Protolude (
  Applicative (pure),
  Integral,
  Maybe (..),
  Traversable (mapM),
  ($),
 )

import Data.ULID.Digits (unDigits)


{-|
Decodes a Crockford-encoded `String` into an integer, if possible.
Returns `Nothing` if the string is not a valid Crockford-encoded value.
-}
decode :: (Integral i) => Text -> Maybe i
decode base32text = do
  numbers <- mapM decodeChar $ T.unpack base32text
  pure $ unDigits 32 numbers


decodeChar :: (Integral i) => Char -> Maybe i
decodeChar c = case C.toUpper c of
  '0' -> Just 0
  'O' -> Just 0
  '1' -> Just 1
  'I' -> Just 1
  'L' -> Just 1
  '2' -> Just 2
  '3' -> Just 3
  '4' -> Just 4
  '5' -> Just 5
  '6' -> Just 6
  '7' -> Just 7
  '8' -> Just 8
  '9' -> Just 9
  'A' -> Just 10
  'B' -> Just 11
  'C' -> Just 12
  'D' -> Just 13
  'E' -> Just 14
  'F' -> Just 15
  'G' -> Just 16
  'H' -> Just 17
  'J' -> Just 18
  'K' -> Just 19
  'M' -> Just 20
  'N' -> Just 21
  'P' -> Just 22
  'Q' -> Just 23
  'R' -> Just 24
  'S' -> Just 25
  'T' -> Just 26
  'V' -> Just 27
  'W' -> Just 28
  'X' -> Just 29
  'Y' -> Just 30
  'Z' -> Just 31
  _ -> Nothing
