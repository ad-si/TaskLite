module UtilsSpec where

import Protolude (
  Bool (False, True),
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
import Prettyprinter (Doc, pretty)
import Prettyprinter.Render.Terminal (AnsiStyle, bold, underlined)
import Test.Hspec (Spec, it, shouldBe, shouldNotBe)

import Config (defaultConfig)
import Config qualified
import Task (
  Task (body, due_utc, metadata, modified_utc, state, ulid, user),
  emptyTask,
 )
import Utils (maybeBold, maybeUnderlined, parseUlidText, parseUlidUtcSection)


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

  it "applies bold style when colors are enabled" $ do
    let conf = defaultConfig{Config.noColor = False}
    maybeBold conf `shouldBe` bold

  it "returns empty style when colors are disabled (maybeBold)" $ do
    let conf = defaultConfig{Config.noColor = True}
    maybeBold conf `shouldNotBe` bold

  it "applies underlined style when colors are enabled" $ do
    let conf = defaultConfig{Config.noColor = False}
    maybeUnderlined conf `shouldBe` underlined

  it "returns empty style when colors are disabled (maybeUnderlined)" $ do
    let conf = defaultConfig{Config.noColor = True}
    maybeUnderlined conf `shouldNotBe` underlined
