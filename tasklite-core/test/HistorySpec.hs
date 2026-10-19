module HistorySpec where

import Protolude (Maybe (..), Text, fromMaybe, show, ($), (<&>))
import Protolude qualified as P

import Data.Hourglass (DateTime)
import Test.Hspec (Spec, describe, it, shouldBe)

import Config (defaultConfig)
import FullTask (
  FullTask (closed_utc, created_utc, state, ulid),
  emptyFullTask,
 )
import History (
  GraphLayout (..),
  HistoryPeriod (..),
  HistoryRow (..),
  formatHistory,
  getGraphLayout,
  getHistory,
  getHistoryRows,
 )
import Task (TaskState (Deletable, Done, Obsolete))
import Utils (parseUtc)


toDateTime :: Text -> DateTime
toDateTime utc = fromMaybe (P.panic "Invalid UTC") $ parseUtc utc


now :: DateTime
now = toDateTime "2024-04-02 12:00:00"


exampleTasks :: [FullTask]
exampleTasks =
  [ emptyFullTask
      { ulid = "01hrbqhfx0000000000000000a"
      , created_utc = Just "2024-03-06 10:00:00"
      }
  , emptyFullTask
      { ulid = "01hrbqhfx0000000000000000b"
      , created_utc = Just "2024-03-06 11:00:00"
      , closed_utc = Just "2024-04-01 09:00:00"
      , state = Just Done
      }
  , emptyFullTask
      { ulid = "01hrbqhfx0000000000000000c"
      , created_utc = Just "2024-03-10 12:00:00"
      , closed_utc = Just "2024-03-11 12:00:00"
      , state = Just Obsolete
      }
  , emptyFullTask
      { ulid = "01hrbqhfx0000000000000000d"
      , created_utc = Just "2024-03-27 08:00:00"
      , closed_utc = Just "2024-03-28 08:00:00"
      , state = Just Deletable
      }
  ]


spec :: Spec
spec = do
  describe "getHistoryRows" $ do
    it "groups tasks by year" $ do
      getHistoryRows Annual now exampleTasks
        `shouldBe` [HistoryRow "2024" 4 1 1 1]

    it "groups tasks by month" $ do
      getHistoryRows Monthly now exampleTasks
        `shouldBe` [ HistoryRow "2024-03" 4 0 1 1
                   , HistoryRow "2024-04" 0 1 0 0
                   ]

    it "groups tasks by week starting on Monday and fills gaps" $ do
      getHistoryRows Weekly now exampleTasks
        `shouldBe` [ HistoryRow "2024-03-04" 3 0 0 0
                   , HistoryRow "2024-03-11" 0 0 1 0
                   , HistoryRow "2024-03-18" 0 0 0 0
                   , HistoryRow "2024-03-25" 1 0 0 1
                   , HistoryRow "2024-04-01" 0 1 0 0
                   ]

    it "groups tasks by day" $ do
      let rows = getHistoryRows Daily now exampleTasks
      P.length rows `shouldBe` 28
      P.take 2 rows
        `shouldBe` [ HistoryRow "2024-03-06" 2 0 0 0
                   , HistoryRow "2024-03-07" 0 0 0 0
                   ]
      P.drop 26 rows
        `shouldBe` [ HistoryRow "2024-04-01" 0 1 0 0
                   , HistoryRow "2024-04-02" 0 0 0 0
                   ]

    it "includes empty periods up to the current one" $ do
      let rows = getHistoryRows Monthly (toDateTime "2024-06-15") exampleTasks
      (rows <&> (.period))
        `shouldBe` ["2024-03", "2024-04", "2024-05", "2024-06"]

    it "falls back to the ULID for the creation date" $ do
      getHistoryRows
        Monthly
        (toDateTime "2024-03-08")
        [emptyFullTask{ulid = "01hrbqhfx0000000000000000a"}]
        `shouldBe` [HistoryRow "2024-03" 1 0 0 0]

  describe "getGraphLayout" $ do
    it "splits the width proportionally to the largest values" $ do
      getGraphLayout 40 10 30 `shouldBe` GraphLayout 10 30 1.0

    it "keeps at least 1 character for each side with values" $ do
      let widths layout = (layout.closedWidth, layout.addedWidth)
      widths (getGraphLayout 10 1 1000) `shouldBe` (1, 9)
      widths (getGraphLayout 10 1000 1) `shouldBe` (9, 1)

    it "uses the whole width if one side has no values" $ do
      getGraphLayout 10 0 5 `shouldBe` GraphLayout 0 10 2.0
      getGraphLayout 10 5 0 `shouldBe` GraphLayout 10 0 2.0
      getGraphLayout 10 0 0 `shouldBe` GraphLayout 0 10 0

  describe "formatHistory" $ do
    it "renders a table with an average row and a graph" $ do
      show
        ( formatHistory
            defaultConfig
            Weekly
            P.False
            (getHistoryRows Weekly now exampleTasks)
        )
        `shouldBe` ( "Week        Added  Done  Obsolete  Deletable  Net\
                     \   Closed│Added\n\
                     \2024-03-04      3     0         0          0    3\
                     \         │+++++++++++++++++++++\n\
                     \2024-03-11      0     0         1          0   -1\
                     \  -------│\n\
                     \2024-03-18      0     0         0          0    0\
                     \         │\n\
                     \2024-03-25      1     0         0          1    0\
                     \  ~~~~~~~│+++++++\n\
                     \2024-04-01      0     1         0          0   -1\
                     \  XXXXXXX│\n\
                     \Average         1     0         0          0    0\
                     \         │+++++++\n\
                     \Legend: + Added  X Done  - Obsolete  ~ Deletable" ::
                       Text
                   )

    it "shows a warning if there are no tasks" $ do
      show (formatHistory defaultConfig Weekly P.False [])
        `shouldBe` ("⚠️ No tasks available" :: Text)

  describe "getHistory" $ do
    it "only shows the most recent periods that fit the available lines" $ do
      show (getHistory defaultConfig Monthly now (Just 5) exampleTasks)
        `shouldBe` ( "Month    Added  Done  Obsolete  Deletable  Net\
                     \                           Closed│\n\
                     \2024-04      0     1         0          0   -1\
                     \  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX│\n\
                     \Legend: + Added  X Done  - Obsolete  ~ Deletable\n\
                     \This list is truncated. \
                     \List all by piping into `cat` or `less`." ::
                       Text
                   )
