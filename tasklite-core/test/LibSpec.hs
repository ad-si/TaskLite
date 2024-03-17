module LibSpec where

import Protolude (
  Maybe (..),
  Text,
  pure,
  show,
  ($),
 )
import Protolude qualified as P

import Config (defaultConfig)
import Test.Hspec (
  Spec,
  describe,
  it,
  shouldBe,
 )

import Lib (countTasks, insertTags, insertTask)
import Task (Task (body, ulid), zeroTask)
import TestUtils (withMemoryDb)


spec :: Spec
spec = do
  describe "Lib" $ do
    it "counts tasks" $ do
      withMemoryDb defaultConfig $ \memConn -> do
        let
          task1 =
            zeroTask
              { ulid = "01hs68z7mdg4ktpxbv0yfafznq"
              , body = "New task"
              }
          task2 =
            zeroTask
              { ulid = "01hs690f9hkzk9z7zews9j2k1d"
              , body = "New task"
              }

        count0 <- countTasks defaultConfig memConn P.mempty
        show count0 `shouldBe` ("0" :: Text)

        insertTask memConn task1
        count1 <- countTasks defaultConfig memConn P.mempty
        show count1 `shouldBe` ("1" :: Text)

        insertTask memConn task2
        count2 <- countTasks defaultConfig memConn P.mempty
        show count2 `shouldBe` ("2" :: Text)

        insertTags memConn Nothing task2 ["test"]
        countWithTag <- countTasks defaultConfig memConn (Just ["+test"])
        show countWithTag `shouldBe` ("1" :: Text)

        pure ()
