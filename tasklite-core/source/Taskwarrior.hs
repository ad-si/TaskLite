{-|
Datatype and conversion functions for Taskwarrior JSON format export.

See: https://taskwarrior.org/docs/commands/export/
-}
module Taskwarrior where

import Protolude (
  Alternative ((<|>)),
  Applicative (pure),
  Char,
  Eq ((/=)),
  Float,
  Generic,
  Int,
  Maybe (..),
  Monad ((>>=)),
  Num ((-)),
  Ord ((<), (>)),
  Semigroup ((<>)),
  Show,
  Text,
  fromMaybe,
  otherwise,
  ($),
  (&),
  (<&>),
  (=<<),
 )
import Protolude qualified as P

import Control.Arrow ((>>>))
import Data.Aeson (ToJSON (..), Value (Object, String), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.Hourglass (DateTime, TimeFormatString, timePrint, toFormat)
import Data.Text qualified as T
import Data.ULID (ulidToInteger)
import Numeric (showHex)

import FullTask (FullTask (..))
import Note (Note (..))
import Task (TaskState (..))
import Utils (parseUlidText, parseUtc, ulidTextToDateTime)


-- | Taskwarrior date format: YYYYMMDDTHHMMSSZ
twDateFormat :: TimeFormatString
twDateFormat = toFormat ("YYYYMMDDTHMISZ" :: [Char])


{-| Convert a ULID to a UUIDv4 (RFC 4122) format for Taskwarrior compatibility.
Decodes the ULID to a 128-bit Integer, then formats it as a UUID hex string.
Format: xxxxxxxx-xxxx-4xxx-xxxx-xxxxxxxxxxxx
-}
ulidToUuid :: Text -> Text
ulidToUuid ulidText =
  case parseUlidText ulidText of
    Nothing -> T.replicate 36 "0" -- Fallback for invalid input
    Just ulid ->
      let
        -- Convert ULID to 128-bit integer
        n = ulidToInteger ulid

        -- Convert to 32-char hex string (padded with leading zeros)
        hexStr = T.pack $ padLeft 32 '0' $ showHex n ""

        -- Insert hyphens at positions 8, 12, 16, 20
        insertAt :: Int -> Text -> Text -> Text
        insertAt pos char txt =
          let (before, after) = T.splitAt pos txt
          in  before <> char <> after

        padLeft :: Int -> Char -> [Char] -> [Char]
        padLeft len c s =
          let padLen = len - P.length s
          in  P.replicate padLen c <> s
      in
        hexStr
          & insertAt 8 "-"
          & insertAt 13 "-"
          & insertAt 18 "-"
          & insertAt 23 "-"


{-| Convert TaskLite's ISO8601 date to Taskwarrior format
TaskLite uses "YYYY-MM-DD HH:MM:SS.mmm"
Taskwarrior uses "YYYYMMDDTHHMMSSZ"
-}
convertDateFormat :: Text -> Maybe Text
convertDateFormat tlDate = do
  let
    stripped = T.strip tlDate

    -- Try parsing "YYYY-MM-DD HH:MM:SS.mmm" format
    parseUtcText :: Text -> Maybe DateTime
    parseUtcText txt = do
      let parts = T.splitOn " " txt
      case parts of
        [datePart, timePart] -> do
          let
            dateNoDash = T.filter (/= '-') datePart
            timeNoDash = T.filter (/= ':') $ T.takeWhile (/= '.') timePart
            combined = dateNoDash <> "T" <> timeNoDash <> "Z"
          parseUtc combined
        _ -> Nothing

  dateTime <- parseUtcText stripped <|> parseUtc stripped
  pure $ T.pack $ timePrint twDateFormat dateTime


-- | Taskwarrior annotation format
data TWAnnotation = TWAnnotation
  { entry :: Text
  , description :: Text
  }
  deriving (Generic, Show)


instance ToJSON TWAnnotation where
  toJSON TWAnnotation{..} =
    object
      [ "entry" .= entry
      , "description" .= description
      ]


-- | Convert a TaskLite task state to Taskwarrior status
taskStateToTwStatus :: Maybe TaskState -> Maybe Text -> Text
taskStateToTwStatus mState mClosedUtc =
  case mState of
    Just Done -> "completed"
    Just Obsolete -> "deleted"
    Just Deletable -> "deleted"
    Nothing ->
      case mClosedUtc of
        Just _ -> "completed" -- Closed without explicit state = completed
        Nothing -> "pending"


{-| Convert a TaskLite FullTask to Taskwarrior JSON format.
Uses a deterministic UUID derived from the ULID for consistency.
-}
fullTaskToTwJson :: FullTask -> Value
fullTaskToTwJson task =
  let
    -- Convert ULID to UUID format
    uuid :: Text
    uuid = ulidToUuid task.ulid

    -- Convert date fields from TaskLite to Taskwarrior format
    convertDate :: Maybe Text -> Maybe Text
    convertDate = (>>= convertDateFormat)

    -- Entry date (created_utc) - derive from ULID if not present
    entryDate :: Maybe Text
    entryDate =
      (task.created_utc >>= convertDateFormat)
        <|> (task.ulid & ulidTextToDateTime <&> (timePrint twDateFormat >>> T.pack))

    -- Modified date
    modifiedDate :: Maybe Text
    modifiedDate = convertDateFormat task.modified_utc

    -- Status based on state and closed_utc
    status :: Text
    status = taskStateToTwStatus task.state task.closed_utc

    -- End date (when completed/deleted)
    endDate :: Maybe Text
    endDate = convertDate task.closed_utc

    -- Due date
    dueDate :: Maybe Text
    dueDate = convertDate task.due_utc

    -- Wait date (awake_utc in TaskLite = wait in Taskwarrior)
    waitDate :: Maybe Text
    waitDate = convertDate task.awake_utc

    -- Scheduled date (ready_utc in TaskLite)
    scheduledDate :: Maybe Text
    scheduledDate = convertDate task.ready_utc

    -- Convert notes to Taskwarrior annotations
    annotations :: Maybe [TWAnnotation]
    annotations =
      task.notes <&> \noteList ->
        noteList <&> \note ->
          TWAnnotation
            { entry =
                fromMaybe
                  ""
                  (ulidTextToDateTime note.ulid <&> (timePrint twDateFormat >>> T.pack))
            , description = note.body
            }

    -- Build the JSON object with only present fields
    requiredFields :: [Pair]
    requiredFields =
      [ "uuid" .= uuid
      , "status" .= status
      , "description" .= task.body
      , "entry" .= fromMaybe "" entryDate
      ]

    optionalField :: (ToJSON a) => Text -> Maybe a -> [Pair]
    optionalField name = \case
      Just val -> [Key.fromText name .= val]
      Nothing -> []
  in
    object $
      requiredFields
        <> optionalField "modified" modifiedDate
        <> optionalField "end" endDate
        <> optionalField "due" dueDate
        <> optionalField "wait" waitDate
        <> optionalField "scheduled" scheduledDate
        <> optionalField "tags" task.tags
        <> optionalField "annotations" annotations
        <> optionalField "priority" (priorityToTwPriority =<< task.priority)
        <> optionalField "recur" task.recurrence_duration
        <> optionalField "project" (metadataProject task.metadata)


{-| Convert TaskLite priority (Float) to Taskwarrior priority (H/M/L)
TaskLite uses numeric values, higher = more important
We'll map: > 1 -> H, 0-1 -> M, < 0 -> L
-}
priorityToTwPriority :: Float -> Maybe Text
priorityToTwPriority p
  | p > 2 = Just "H"
  | p > 0 = Just "M"
  | p < 0 = Just "L"
  | otherwise = Nothing


-- | Extract project from metadata if present
metadataProject :: Maybe Value -> Maybe Text
metadataProject mMeta = do
  meta <- mMeta
  case meta of
    Object obj -> do
      projVal <- KeyMap.lookup "project" obj
      case projVal of
        String txt -> Just txt
        _ -> Nothing
    _ -> Nothing
