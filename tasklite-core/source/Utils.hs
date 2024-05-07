{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use maybe" #-}

{-|
Several utility functions (e.g for parsing & serializing UTC stamps)
-}
module Utils where

import Protolude (
  Alternative ((<|>)),
  Applicative (pure),
  Char,
  Double,
  Eq,
  Fractional (fromRational, (/)),
  Functor (fmap),
  IO,
  Int,
  Integer,
  Integral (div, mod),
  Maybe (..),
  Monoid (mempty),
  Num ((*), (+)),
  Ord ((<), (>)),
  Rational,
  Real (toRational),
  RealFrac (properFraction, truncate),
  Semigroup ((<>)),
  Show,
  Word16,
  flip,
  forM,
  fromIntegral,
  fromMaybe,
  fst,
  otherwise,
  readMaybe,
  realToFrac,
  show,
  stderr,
  ($),
  (&),
  (.),
  (<&>),
 )
import Protolude qualified as P

import Control.Monad.Catch (catchAll)
import Data.Colour.RGBSpace (RGB (..))
import Data.Hourglass (
  DateTime,
  Elapsed (Elapsed),
  ElapsedP (..),
  ISO8601_DateAndTime (ISO8601_DateAndTime),
  NanoSeconds (NanoSeconds),
  Seconds (Seconds),
  Time (timeFromElapsedP),
  TimeFormat (toFormat),
  TimeFormatString,
  Timeable (timeGetElapsedP),
  timeGetDateTimeOfDay,
  timeParse,
  timePrint,
 )
import Data.Text as T (
  Text,
  chunksOf,
  drop,
  intercalate,
  pack,
  take,
  toLower,
  unlines,
  unpack,
 )
import Data.Time (UTCTime, ZonedTime, addUTCTime, zonedTimeToUTC)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.ULID (ULID (ULID, random, timeStamp))
import Data.ULID.Random (ULIDRandom, mkULIDRandom)
import Data.ULID.TimeStamp (ULIDTimeStamp, mkULIDTimeStamp)
import Prettyprinter (Doc, Pretty (pretty), softline)
import Prettyprinter.Render.Terminal (
  AnsiStyle,
  Color (Black),
  colorDull,
 )
import System.Console.ANSI (ConsoleLayer (..), hGetLayerColor)
import System.Process (readProcess)

import Base32 (decode)
import Config (
  Config (bodyStyle),
  Hook (body, filePath, interpreter),
 )
import Control.Arrow ((>>>))
import Prettyprinter.Internal.Type (Doc (Empty))
import System.Random (mkStdGen)


type IdText = Text
type TagText = Text


data Filter a = NoFilter | Only a
  deriving (Eq, Ord, Show)


data ListModifiedFlag = AllItems | ModifiedItemsOnly
  deriving (Eq, Show)


-- | Combine documents with 2 newlines
(<++>) :: Doc ann -> Doc ann -> Doc ann
x <++> y =
  x <> softline <> softline <> y


-- | Combine documents with 2 newlines if both documents are non-empty
(<$$>) :: Doc ann -> Doc ann -> Doc ann
Empty <$$> y = y
x <$$> Empty = x
x <$$> y = x <++> y


infixr 6 <$$>


vsepCollapse :: [Doc ann] -> Doc ann
vsepCollapse = P.foldr (<$$>) Empty


zeroTime :: DateTime
zeroTime = timeFromElapsedP 0


emptyUlid :: ULID
emptyUlid =
  ULID
    { timeStamp = mkULIDTimeStamp 0
    , random = mkULIDRandom (mkStdGen 0) & P.fst
    }


-- | ULID time section if timestamp == 1970-01-01 00:00:00.000
zeroUlidTxt :: Text
zeroUlidTxt = "0000000000"


utcFormatReadable :: TimeFormatString
utcFormatReadable =
  toFormat ("YYYY-MM-DD H:MI:S" :: [Char])


parseUtcNum :: Int -> Maybe DateTime
parseUtcNum number =
  parseUtc (show number)


parseUtc :: Text -> Maybe DateTime
parseUtc utcText =
  let
    utcString = unpack $ T.toLower utcText

    -- TODO: Remove after https://github.com/vincenthz/hs-hourglass/issues/50
    addSpaceAfter10 = T.intercalate " " . T.chunksOf 10
    addSpaceAfter13 = T.intercalate " " . T.chunksOf 13
    unixMicro = "EPOCH ms us" :: [Char]
    unixMilli = "EPOCH ms" :: [Char]

    tParse :: [Char] -> Maybe DateTime
    tParse formatString =
      timeParse (toFormat formatString) utcString
  in
    -- From long (specific) to short (unspecific)
    timeParse ISO8601_DateAndTime utcString
      -- <|> tParse "YYYY-MM-DDtH:MI:S.ns"
      <|> tParse "YYYY-MM-DDtH:MI:S.msusns"
      <|> tParse "YYYY-MM-DDtH:MI:S.msus"
      <|> tParse "YYYY-MM-DDtH:MI:S.ms"
      <|> tParse "YYYY-MM-DDtH:MI:S"
      <|> tParse "YYYY-MM-DDtH:MI"
      <|> tParse "YYYYMMDDtHMIS"
      <|> tParse "YYYY-MM-DD H:MI:S"
      <|> tParse "YYYY-MM-DD H:MI"
      <|> tParse "YYYY-MM-DD"
      <|> timeParse
        (toFormat unixMicro)
        (unpack $ (addSpaceAfter10 . addSpaceAfter13) utcText)
      <|> timeParse (toFormat unixMilli) (unpack $ addSpaceAfter10 utcText)
      <|> tParse "EPOCH"


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
  T.take 10 >>> parseUlidUtcSection


{-| `ulidText2utc` converts a ULID to a UTC timestamp

>>> ulidText2utc "01hq68smfe0r9entg3x4rb9441"
Just "2024-02-21 16:43:17.358"
-}
ulidText2utc :: Text -> Maybe Text
ulidText2utc ulid =
  ulid
    & ulidTextToDateTime
    <&> (timePrint (toFormat ("YYYY-MM-DD H:MI:S.ms" :: [Char])) >>> T.pack)


parseUlidText :: Text -> Maybe ULID
parseUlidText ulidText = do
  let
    mkUlidTimeMaybe text = text & ulidTextToDateTime <&> toUlidTime

    mkUlidRandomMaybe :: Text -> Maybe ULIDRandom
    mkUlidRandomMaybe = readMaybe . T.unpack . T.drop 10

  ulidTime <- mkUlidTimeMaybe ulidText
  ulidRandom <- mkUlidRandomMaybe ulidText
  pure $ ULID ulidTime ulidRandom


-- TODO: Remove after https://github.com/vincenthz/hs-hourglass/issues/52
rationalToElapsedP :: Rational -> ElapsedP
rationalToElapsedP secondsFrac =
  let (sec, nanoSec) = properFraction secondsFrac
  in  ElapsedP (Elapsed (Seconds sec)) (NanoSeconds $ truncate $ nanoSec * 1e9)


-- TODO: Remove after https://github.com/vincenthz/hs-hourglass/issues/45
elapsedPToRational :: ElapsedP -> Rational
elapsedPToRational (ElapsedP (Elapsed (Seconds s)) (NanoSeconds ns)) =
  ((1e9 * fromIntegral s) + fromIntegral ns) / 1e9


toUlidTime :: DateTime -> ULIDTimeStamp
toUlidTime =
  timeGetElapsedP
    >>> elapsedPToRational
    >>> realToFrac
    >>> mkULIDTimeStamp


setDateTime :: ULID -> DateTime -> ULID
setDateTime ulid dateTime =
  ULID
    (toUlidTime dateTime)
    (random ulid)


{-| Currently not needed
 addStartUtc :: DateTime -> Iso8601.Interval -> Iso8601.Interval
 addStartUtc utc interval = case interval of
   Iso8601.Interval (Iso8601.JustDuration duration) ->
     Iso8601.Interval (Iso8601.StartDuration (dateTimeToUtcTime utc) duration)
   _ -> interval
-}
zonedTimeToDateTime :: ZonedTime -> DateTime
zonedTimeToDateTime zTime =
  zTime
    & zonedTimeToUTC
    & utcTimeToPOSIXSeconds
    & toRational
    & rationalToElapsedP
    & timeFromElapsedP


utcTimeToDateTime :: UTCTime -> DateTime
utcTimeToDateTime utcTime =
  utcTime
    & utcTimeToPOSIXSeconds
    & toRational
    & rationalToElapsedP
    & timeFromElapsedP


dateTimeToUtcTime :: DateTime -> UTCTime
dateTimeToUtcTime dateTime =
  dateTime
    & timeGetElapsedP
    & elapsedPToRational
    & fromRational
    & flip addUTCTime (posixSecondsToUTCTime 0)


-- From https://mail.haskell.org/pipermail/haskell-cafe/2009-August/065854.html
numDigits :: Integer -> Integer -> Integer
numDigits base num =
  let
    ilog b n
      | n < b = (0, n)
      | otherwise =
          let (e, r) = ilog (b * b) n
          in  if r < b then (2 * e, r) else (2 * e + 1, r `div` b)
  in
    1 + fst (ilog base num)


executeHooks :: Text -> [Hook] -> IO (Doc AnsiStyle)
executeHooks stdinText hooks = do
  let stdinStr = T.unpack stdinText
  cmdOutput <- forM hooks $ \hook ->
    case hook.filePath of
      Just fPath -> readProcess fPath [] stdinStr
      Nothing -> do
        let ipret = hook.interpreter
        if
          | ipret `P.elem` ["ruby", "rb"] ->
              readProcess "ruby" ["-e", T.unpack hook.body] stdinStr
          | ipret `P.elem` ["javascript", "js", "node", "node.js"] ->
              readProcess "node" ["-e", T.unpack hook.body] stdinStr
          | ipret `P.elem` ["python", "python3", "py"] ->
              readProcess "python3" ["-c", T.unpack hook.body] stdinStr
          | otherwise ->
              pure mempty

  pure $
    cmdOutput
      <&> T.pack
      & T.unlines
      & pretty


applyColorMode :: Config -> IO Config
applyColorMode conf = do
  layerColorBgMb <-
    catchAll
      (hGetLayerColor stderr Background)
      (\_ -> pure Nothing)

  let
    calcLuminance :: RGB Word16 -> Double
    calcLuminance (RGB{..}) =
      ( 0.3 * fromIntegral channelRed
          + 0.6 * fromIntegral channelGreen
          + 0.1 * fromIntegral channelBlue
      )
        / 65536

    isLightMode =
      layerColorBgMb
        <&> calcLuminance
        & fromMaybe 0 -- Default to dark mode
        & (> 0.5)

  pure $
    if isLightMode
      then conf{bodyStyle = colorDull Black}
      else conf
