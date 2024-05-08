module UtilsSpec where

import Protolude (
  Functor (fmap),
  Maybe (..),
  Text,
  show,
  ($),
  (&),
 )

import Data.Aeson (decode)
import Data.Hourglass (
  Elapsed (Elapsed),
  ElapsedP (ElapsedP),
  timeGetDateTimeOfDay,
 )
import Test.Hspec (Spec, it, shouldBe)

import Task (
  Task (body, due_utc, metadata, modified_utc, state, ulid, user),
  emptyTask,
 )
import Utils (parseUlidText, parseUlidUtcSection)


exampleTask :: Task
exampleTask =
  emptyTask
    { ulid = "01hq68smfe0r9entg3x4rb9441"
    , body = "Buy milk"
    , state = Nothing
    , modified_utc = "2024-02-21 16:43:17"
    , due_utc = Just "2025-07-08 10:22:56"
    , user = "john"
    , metadata = "{\"source\":\"fridge\"}" & decode
    }


spec :: Spec
spec = do
  it "correctly parses beginning of UNIX epoch" $
    do
      parseUlidUtcSection "0000000000"
      `shouldBe` Just (timeGetDateTimeOfDay $ Elapsed 0)

  it "correctly parses 36 ms after UNIX epoch" $
    do
      parseUlidUtcSection "0000000014"
      `shouldBe` Just (timeGetDateTimeOfDay $ ElapsedP 0 36000000)

  it "correctly parses a ULID string" $ do
    let ulidText = "0000000014T4R3JR7HMQNREEW8" :: Text

    fmap show (parseUlidText ulidText) `shouldBe` Just ulidText
