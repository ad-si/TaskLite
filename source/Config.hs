{-|
The default config primarily defines the styling and formatting
-}

module Config where

import Protolude as P

import Data.Aeson
import Data.Hourglass
import Data.Text
import Data.Text.Prettyprint.Doc (pretty)
import Data.Text.Prettyprint.Doc.Render.Terminal
import Data.Text.Prettyprint.Doc.Render.Terminal.Internal (ansiForeground)
import Data.Text.Prettyprint.Doc.Internal
import Data.Yaml (encode)


data Config = Config
  { tableName :: Text
  , idStyle :: AnsiStyle
  , priorityStyle :: AnsiStyle
  , dateStyle :: AnsiStyle
  , bodyStyle :: AnsiStyle
  , bodyClosedStyle :: AnsiStyle
  , closedStyle :: AnsiStyle
  , dueStyle :: AnsiStyle
  , overdueStyle :: AnsiStyle
  , tagStyle :: AnsiStyle
  , utcFormat :: TimeFormatString
  , dataDir :: FilePath
  , dbName :: FilePath
  , dateWidth :: Int
  , bodyWidth :: Int
  , prioWidth :: Int
  , headCount :: Int
  , maxWidth :: Int
  , progressBarWidth :: Int
  } deriving (Generic, Show)


instance FromJSON Config where
  parseJSON = withObject "config" $ \o -> do
    idStyle         <- o .:? "idStyle" .!= idStyle defaultConfig
    priorityStyle   <- o .:? "priorityStyle".!= priorityStyle defaultConfig
    tableName       <- o .:? "tableName" .!= tableName defaultConfig
    dateStyle       <- o .:? "dateStyle" .!= dateStyle defaultConfig
    bodyStyle       <- o .:? "bodyStyle" .!= bodyStyle defaultConfig
    bodyClosedStyle <- o .:? "bodyClosedStyle".!= bodyClosedStyle defaultConfig
    closedStyle     <- o .:? "closedStyle" .!= closedStyle defaultConfig
    dueStyle        <- o .:? "dueStyle" .!= dueStyle defaultConfig
    overdueStyle    <- o .:? "overdueStyle" .!= overdueStyle defaultConfig
    tagStyle        <- o .:? "tagStyle" .!= tagStyle defaultConfig
    utcFormat       <- o .:? "utcFormat" .!= utcFormat defaultConfig
    dataDir         <- o .:? "dataDir" .!= dataDir defaultConfig
    dbName          <- o .:? "dbName" .!= dbName defaultConfig
    dateWidth       <- o .:? "dateWidth" .!= dateWidth defaultConfig
    bodyWidth       <- o .:? "bodyWidth" .!= bodyWidth defaultConfig
    prioWidth       <- o .:? "prioWidth" .!= prioWidth defaultConfig
    headCount       <- o .:? "headCount" .!= headCount defaultConfig
    maxWidth        <- o .:? "maxWidth" .!= maxWidth defaultConfig
    progressBarWidth <- o .:? "progressBarWidth"
      .!= progressBarWidth defaultConfig

    pure $ Config {..}


instance ToJSON Config

instance Pretty Config where
  pretty = pretty
    . dropEnd 1 -- Drop trailing newline to maybe add it later
    . decodeUtf8
    . Data.Yaml.encode


parseColor :: Text -> Maybe Color
parseColor colorTxt =
  let
    colorOnly = fromMaybe colorTxt $ stripPrefix "dull " colorTxt
    colorToType = \case
      "black"   -> Just Black
      "red"     -> Just Red
      "green"   -> Just Green
      "yellow"  -> Just Yellow
      "blue"    -> Just Blue
      "magenta" -> Just Magenta
      "cyan"    -> Just Cyan
      "white"   -> Just White
      _ -> Nothing
  in
    colorToType colorOnly


parseAnsiStyle :: Text -> Maybe AnsiStyle
parseAnsiStyle colorTxt =
  let
    func = if "dull" `isInfixOf` colorTxt
      then colorDull
      else color
    colorMaybe = parseColor colorTxt
  in
    fmap func colorMaybe


instance FromJSON AnsiStyle where
  parseJSON = withText "AnsiStyle" $ \value -> do
    pure $ fromMaybe (color Black) $ parseAnsiStyle value

instance ToJSON AnsiStyle where
  toJSON style = String $ show $ ansiForeground style

instance FromJSON TimeFormatString where
  parseJSON = withText "TimeFormatString" $ \_ -> do
    pure (toFormat ("YYYY-MM-DD H:MI:S" :: [Char]))

instance ToJSON TimeFormatString where
  toJSON = pure "YYYY-MM-DD H:MI:S"

-- instance Pretty AnsiStyle where
--   pretty = pretty . (\_ -> ". " :: Text)


defaultConfig :: Config
defaultConfig = Config
  { tableName = "tasks"
  , idStyle = color Green
  , priorityStyle = color Magenta
  , dateStyle = colorDull Black
  , bodyStyle = color White
  , bodyClosedStyle = color Black
  , closedStyle = colorDull Black
  , dueStyle = color Yellow
  , overdueStyle = color Red
  , tagStyle = color Blue
  , utcFormat = toFormat ("YYYY-MM-DD H:MI:S" :: [Char])
  , dataDir = "~/TaskLite"
  , dbName = "main.db"
  , dateWidth = 10
  , bodyWidth = 10
  , prioWidth = 4
  , headCount = 20
  , maxWidth = 120
  , progressBarWidth = 24
  }
