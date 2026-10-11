module NativeMessagingSpec where

import Protolude (Bool (False), Maybe (..), Text, not, pure, ($), (&), (<&>))
import Protolude qualified as P

import Data.Aeson (Value (Object, String))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Database.SQLite.Simple (Only (Only), query)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Config (defaultConfig)
import NativeMessaging (Request (ImportEmail), handleRequest)
import Task (Task (metadata))
import TestUtils (withMemoryDb)


lookupField :: Text -> Value -> Maybe Value
lookupField key = \case
  Object obj -> KeyMap.lookup (Key.fromText key) obj
  _ -> Nothing


spec :: Spec
spec = do
  let
    conf = defaultConfig
    email =
      [ "From: Jane Doe <jane@example.com>"
      , "Date: Mon, 03 Jun 2024 10:20:30 +0000"
      , "Subject: Review the report"
      , ""
      , "Please review it."
      ]
        & T.intercalate "\r\n"
        & T.encodeUtf8

  describe "handleRequest" $ do
    it "stores the time of adding an email in the metadata" $ do
      (added, existing, metadatas) <- withMemoryDb conf $ \connection -> do
        added <- handleRequest conf connection (ImportEmail email)
        existing <- handleRequest conf connection (ImportEmail email)
        let ulid = case lookupField "ulid" added of
              Just (String text) -> text
              _ -> ""
        tasks :: [Task] <-
          query connection "SELECT * FROM tasks WHERE ulid == ?" (Only ulid)
        pure (added, existing, tasks <&> metadata)

      lookupField "status" added `shouldBe` Just (String "added")
      lookupField "status" existing `shouldBe` Just (String "exists")
      case metadatas of
        [Just metadataValue] ->
          lookupField "addedUtc" metadataValue `shouldSatisfy` \case
            -- The current time and not the email's date
            Just (String utc) -> not ("2024-06-03" `T.isPrefixOf` utc)
            _ -> False
        _ -> P.die "Expected exactly one task with metadata"
