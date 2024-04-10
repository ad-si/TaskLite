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
  shouldContain,
  shouldNotContain,
 )

import Data.Hourglass (DateTime)
import Lib (countTasks, insertRecord, insertTags, newTasks)
import Task (Task (body, closed_utc, state, ulid), TaskState (Done), zeroTask)
import TestUtils (withMemoryDb)


spec :: DateTime -> Spec
spec now = do
  describe "Lib" $ do
    it "counts tasks" $ do
      withMemoryDb defaultConfig $ \memConn -> do
        let
          task1 =
            zeroTask
              { ulid = "01hs68z7mdg4ktpxbv0yfafznq"
              , body = "New task 1"
              }
          task2 =
            zeroTask
              { ulid = "01hs690f9hkzk9z7zews9j2k1d"
              , body = "New task 2"
              }

        count0 <- countTasks defaultConfig memConn P.mempty
        show count0 `shouldBe` ("0" :: Text)

        insertRecord "tasks" memConn task1
        count1 <- countTasks defaultConfig memConn P.mempty
        show count1 `shouldBe` ("1" :: Text)

        insertRecord "tasks" memConn task2
        count2 <- countTasks defaultConfig memConn P.mempty
        show count2 `shouldBe` ("2" :: Text)

        insertTags memConn Nothing task2 ["test"]
        countWithTag <- countTasks defaultConfig memConn (Just ["+test"])
        show countWithTag `shouldBe` ("1" :: Text)

        pure ()

    it "gets new tasks" $ do
      withMemoryDb defaultConfig $ \memConn -> do
        let
          task1 =
            zeroTask
              { ulid = "01hs68z7mdg4ktpxbv0yfafznq"
              , body = "New task 1"
              }
          task2 =
            zeroTask
              { ulid = "01hs6zsf3c0vqx6egfnmbqtmvy"
              , body = "New task 2"
              , closed_utc = Just "2024-04-10T18:54:10Z"
              , state = Just Done
              }

        insertRecord "tasks" memConn task1
        insertRecord "tasks" memConn task2

        cliOutput <- newTasks defaultConfig now memConn (Just ["state:done"])
        show cliOutput `shouldContain` "New task 2"
        show cliOutput `shouldNotContain` "New task 1"
