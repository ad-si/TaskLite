{-|
Several utility functions (e.g for parsing & serializing UTC stamps)
-}

module Utils where

import Protolude as P

import Data.Text as T
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Time (addUTCTime, UTCTime, ZonedTime, zonedTimeToUTC)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Hourglass
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.ULID
import Data.ULID.Random
import Data.ULID.TimeStamp
import System.Process

import Base32
import Config


type IdText = Text
type TagText = Text

data Filter a = NoFilter | Only a
  deriving (Eq, Ord, Show)

data ListModifiedFlag = AllItems | ModifiedItemsOnly
  deriving (Eq, Show)


(<++>) :: Doc ann -> Doc ann -> Doc ann
x <++> y =
  x <> softline <> softline <> y


parseUtcNum :: Int -> Maybe DateTime
parseUtcNum number =
  parseUtc (show number)


parseUtc :: Text -> Maybe DateTime
parseUtc utcText =
  let
    utcString = unpack $ T.toLower utcText

    -- TOOD: Remove after https://github.com/vincenthz/hs-hourglass/issues/50
    addSpaceAfter10 = T.intercalate " " . T.chunksOf 10
    addSpaceAfter13 = T.intercalate " " . T.chunksOf 13
    unixMicro = "EPOCH ms us" :: [Char]
    unixMilli = "EPOCH ms" :: [Char]

    tParse :: [Char] -> Maybe DateTime
    tParse formatString =
      timeParse (toFormat formatString) utcString
  in
    -- From long (specific) to short (unspecific)
        (timeParse ISO8601_DateAndTime utcString)
    <|> (tParse "YYYY-MM-DDtH:MI:S")
    <|> (tParse "YYYY-MM-DDtH:MI")
    <|> (tParse "YYYYMMDDtHMIS")
    <|> (tParse "YYYY-MM-DD H:MI:S")
    <|> (tParse "YYYY-MM-DD H:MI")
    <|> (tParse "YYYY-MM-DD")
    <|> timeParse
          (toFormat unixMicro)
          (unpack $ (addSpaceAfter10 . addSpaceAfter13) utcText)
    <|> timeParse (toFormat unixMilli) (unpack $ addSpaceAfter10 utcText)
    <|> (tParse "EPOCH")


parseUlidUtcSection :: Text -> Maybe DateTime
parseUlidUtcSection encodedUtc = do
  let
    decodedUtcMaybe = Base32.decode encodedUtc
    getElapsed val = Elapsed $ Seconds $ val `div` 1000

  elapsed <- fmap getElapsed decodedUtcMaybe
  milliSecPart <- fmap (`mod` 1000) decodedUtcMaybe

  let nanoSeconds = NanoSeconds $ milliSecPart * 1000000

  pure $ timeGetDateTimeOfDay $ ElapsedP elapsed nanoSeconds


ulidTextToDateTime :: Text -> Maybe DateTime
ulidTextToDateTime =
    parseUlidUtcSection . T.take 10


parseUlidText :: Text -> Maybe ULID
parseUlidText ulidText = do
  let
    mkUlidTimeMaybe text = fmap toUlidTime (ulidTextToDateTime text)

    mkUlidRandomMaybe :: Text -> Maybe ULIDRandom
    mkUlidRandomMaybe = readMaybe . T.unpack . T.drop 10

  ulidTime   <- mkUlidTimeMaybe ulidText
  ulidRandom <- mkUlidRandomMaybe ulidText
  pure $ ULID ulidTime ulidRandom


-- TODO: Remove after https://github.com/vincenthz/hs-hourglass/issues/52
rationalToElapsedP :: Rational -> ElapsedP
rationalToElapsedP secondsFrac =
  let (sec, nanoSec) = properFraction secondsFrac
  in ElapsedP (Elapsed (Seconds sec)) (NanoSeconds $ truncate $ nanoSec * 1e9)


-- TODO: Remove after https://github.com/vincenthz/hs-hourglass/issues/45
elapsedPToRational :: ElapsedP -> Rational
elapsedPToRational (ElapsedP (Elapsed (Seconds s)) (NanoSeconds ns)) =
  ((1e9 * (fromIntegral s)) + (fromIntegral ns)) / 1e9


toUlidTime :: DateTime -> ULIDTimeStamp
toUlidTime =
  mkULIDTimeStamp . realToFrac . elapsedPToRational . timeGetElapsedP


setDateTime :: ULID -> DateTime -> ULID
setDateTime ulid dateTime =
  ULID
    (toUlidTime dateTime)
    (random ulid)


-- | Currently not needed
-- addStartUtc :: DateTime -> Iso8601.Interval -> Iso8601.Interval
-- addStartUtc utc interval = case interval of
--   Iso8601.Interval (Iso8601.JustDuration duration) ->
--     Iso8601.Interval (Iso8601.StartDuration (dateTimeToUtcTime utc) duration)
--   _ -> interval


zonedTimeToDateTime :: ZonedTime -> DateTime
zonedTimeToDateTime zTime = zTime
  & zonedTimeToUTC
  & utcTimeToPOSIXSeconds
  & toRational
  & rationalToElapsedP
  & timeFromElapsedP


utcTimeToDateTime :: UTCTime -> DateTime
utcTimeToDateTime utcTime = utcTime
  & utcTimeToPOSIXSeconds
  & toRational
  & rationalToElapsedP
  & timeFromElapsedP


dateTimeToUtcTime :: DateTime -> UTCTime
dateTimeToUtcTime dateTime = dateTime
  & timeGetElapsedP
  & elapsedPToRational
  & fromRational
  & flip addUTCTime (posixSecondsToUTCTime 0)


-- From https://mail.haskell.org/pipermail/haskell-cafe/2009-August/065854.html
numDigits :: Integer -> Integer -> Integer
numDigits base num =
  let
    ilog b n
     | n < b     = (0, n)
     | otherwise = let (e, r) = ilog (b * b) n
                   in  if r < b then (2 * e, r) else (2 * e+1, r `div` b)
  in
    1 + fst (ilog base num)


executeHooks :: Text -> [Hook] -> IO (Doc AnsiStyle)
executeHooks stdinText hooks = do
  let stdinStr = T.unpack stdinText
  cmdOutput <- forM hooks $ \hook ->
    case (hook & filePath) of
      Just fPath -> readProcess fPath [] stdinStr
      Nothing -> do
        let ipret = hook & interpreter
        if | ipret `elem` ["ruby", "rb"] ->
              readProcess "ruby" ["-e", (T.unpack $ hook & body)] stdinStr

           | ipret `elem` ["javascript", "js", "node", "node.js"] ->
              readProcess "node" ["-e", (T.unpack $ hook & body)] stdinStr

           | ipret `elem` ["python", "python3", "py"] ->
              readProcess "python3" ["-c", (T.unpack $ hook & body)] stdinStr

           | otherwise ->
              pure mempty

  pure $ cmdOutput
    <&> T.pack
    & T.unlines
    & pretty
