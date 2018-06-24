{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module ImportExport where

import Protolude as P

import Codec.Crockford as Crock
import Data.Aeson as Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Text as T
import Data.Hourglass
import Data.ULID
import Data.ULID.TimeStamp
import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Sqlite
import Database.Beam.Schema.Tables
import Database.Beam.Sqlite.Syntax (SqliteExpressionSyntax)
import Database.SQLite.Simple as Sql
import Database.SQLite.Simple.FromField as Sql.FromField
import Database.SQLite.Simple.ToField as Sql.ToField
import Database.SQLite.Simple.Internal hiding (result)
import Database.SQLite.Simple.Ok
import Foreign.C
import Lib
import System.Directory
import Time.System
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Text.Prettyprint.Doc.Render.Terminal
import Unsafe (unsafeHead)
import Utils
import qualified SqlUtils as SqlU
import FullTask
import Task (TaskState(..), textToTaskState)


importTask :: IO ()
importTask = do
  content <- BL.getContents
  let task = Aeson.eitherDecode content :: Either [Char] FullTask
  print task


showQuoteless :: Show a => a -> Text
showQuoteless =
  let isQuote = (== '"')
  in T.dropWhile isQuote . T.dropWhileEnd isQuote . T.pack . show


parseUtc :: Text -> Maybe DateTime
parseUtc utcText =
  let
    isoFormat = toFormat ("YYYYMMDDTHMIS" :: [Char])
    utcString = T.unpack utcText
  in
        (timeParse ISO8601_DateAndTime utcString)
    <|> (timeParse isoFormat utcString)


setDateTime :: ULID -> DateTime -> ULID
setDateTime ulid dateTime = ULID
  (mkULIDTimeStamp $ realToFrac
    (timeFromElapsed $ timeGetElapsed dateTime :: CTime))
  (random ulid)


instance FromJSON FullTask where
  parseJSON = withObject "task" $ \o -> do
    entry        <- o .:? "entry"
    creation     <- o .:? "creation"
    created_at   <- o .:? "created_at"
    let createdUtcMaybe = fromMaybe (timeFromElapsed 0 :: DateTime)
          (parseUtc =<< (entry <|> creation <|> created_at))

    o_body       <- o .:? "body"
    description  <- o .:? "description"
    let body = fromMaybe "" (o_body <|> description)

    o_state      <- o .:? "state"
    status       <- o .:? "status"
    let state = fromMaybe Open (textToTaskState =<< (o_state <|> status))

    priority_adjustment <- o .:? "priority_adjustment"
    urgency             <- o .:? "urgency"
    o_priority          <- optional (o .: "priority")
    let priority = priority_adjustment <|> urgency <|> o_priority

    modified          <- o .:? "modified"
    modified_at       <- o .:? "modified_at"
    o_modified_utc    <- o .:? "modified_utc"
    modification_date <- o .:? "modification_date"
    updated_at        <- o .:? "updated_at"
    let
      maybeModified = modified <|> modified_at <|> o_modified_utc
        <|> modification_date <|> updated_at
      modified_utc = T.pack $ timePrint ISO8601_DateAndTime $
        fromMaybe createdUtcMaybe (parseUtc =<< maybeModified)

    o_tags  <- o .:? "tags"
    project <- o .:? "project"
    let
      projects = fmap (:[]) project
      tags = o_tags  <> projects

    due       <- o .:? "due"
    o_due_utc <- o .:? "due_utc"
    due_on    <- o .:? "due_on"
    let
      maybeDue = due <|> o_due_utc <|> due_on
      due_utc = fmap
        (T.pack . (timePrint ISO8601_DateAndTime))
        (parseUtc =<< maybeDue)

    closed       <- o .:? "closed"
    o_closed_utc <- o .:? "closed_utc"
    closed_on    <- o .:? "closed_on"
    end          <- o .:? "end"
    o_end_utc    <- o .:? "end_utc"
    end_on       <- o .:? "end_on"
    let
      maybeClosed = closed <|> o_closed_utc <|> closed_on
        <|> end <|> o_end_utc <|> end_on
      closed_utc = fmap
        (T.pack . (timePrint ISO8601_DateAndTime))
        (parseUtc =<< maybeClosed)

    let ulid = ""
    let tempTask = FullTask {..}

    o_ulid  <- o .:? "ulid"
    let
      ulidGenerated = (ulidFromInteger . abs . toInteger . hash) tempTask
      ulidCombined = setDateTime ulidGenerated createdUtcMaybe
      ulid = T.toLower $ fromMaybe ""
        (o_ulid <|> Just (show ulidCombined))

    -- let showInt = show :: Int -> Text
    -- uuid           <- o .:? "uuid"
    -- -- Map `show` over `Parser` & `Maybe` to convert possible `Int` to `Text`
    -- id             <- (o .:? "id" <|> ((showInt <$>) <$> (o .:? "id")))
    -- let id = (uuid <|> id)

    pure tempTask{ulid = ulid}


dumpCsv :: IO ()
dumpCsv = do
  execWithConn $ \connection -> do
    rows <- (query_ connection "select * from tasks_view") :: IO [FullTask]

    putStrLn $ Csv.encodeDefaultOrderedByName rows


dumpNdjson :: IO ()
dumpNdjson = do
  execWithConn $ \connection -> do
    rows <- (query_ connection "select * from tasks_view") :: IO [FullTask]

    forM_ rows $ putStrLn . Aeson.encode
