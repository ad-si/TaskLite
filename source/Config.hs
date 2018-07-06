{-|
The default config primarily defines the styling and formatting
-}

module Config where

import Protolude as P

import Data.Hourglass
import Data.Text.Prettyprint.Doc.Render.Terminal


data Config = Config
  { tableName :: Text
  , idWidth :: Int
  , idStyle :: AnsiStyle
  , priorityStyle :: AnsiStyle
  , dateStyle :: AnsiStyle
  , bodyStyle :: AnsiStyle
  , closedStyle :: AnsiStyle
  , dueStyle :: AnsiStyle
  , tagStyle :: AnsiStyle
  , utcFormat :: TimeFormatString
  , mainDir :: FilePath
  , dbName :: FilePath
  , dateWidth :: Int
  , bodyWidth :: Int
  , prioWidth :: Int
  , headCount :: Int
  , maxWidth :: Int
  , progressBarWidth :: Int
  }


conf :: Config
conf = Config
  { tableName = "tasks"
  , idWidth = 4
  , idStyle = color Green
  , priorityStyle = color Magenta
  , dateStyle = color Yellow
  , bodyStyle = color White
  , closedStyle = color Black
  , dueStyle = color Red
  , tagStyle = color Blue
  , utcFormat = toFormat ("YYYY-MM-DD H:MI:S" :: [Char])
  , mainDir = "tasklite"
  , dbName = "main.db"
  , dateWidth = 10
  , bodyWidth = 10
  , prioWidth = 4
  , headCount = 20
  , maxWidth = 120
  , progressBarWidth = 24
  }
