{-|
The default config primarily defines the styling and formatting
-}

module Config where

import Protolude as P hiding (modify)

import Data.Aeson
import System.FilePath (takeBaseName)
import Data.Hourglass
import Data.Text as T
import Prettyprinter.Render.Terminal
import Prettyprinter.Render.Terminal.Internal (ansiForeground)
import Prettyprinter.Internal
import Data.Yaml (encode)


data Hook = Hook
  { filePath :: Maybe FilePath
  , interpreter :: Text
  , body :: Text
  } deriving (Generic, Show)

instance ToJSON Hook
instance FromJSON Hook

emptyHook :: Hook
emptyHook = Hook
  { filePath = Nothing
  , interpreter = ""
  , body = ""
  }


data HookSet = HookSet
  { pre :: [Hook]
  , post :: [Hook]
  } deriving (Generic, Show)

instance ToJSON HookSet

-- | Necessary to make fields optional without using a Maybe type
instance FromJSON HookSet where
  parseJSON = withObject "hookSet" $ \o -> do
    pre   <- o .:? "pre" .!= pre emptyHookSet
    post  <- o .:? "post".!= post emptyHookSet

    pure $ HookSet {..}

emptyHookSet :: HookSet
emptyHookSet = HookSet
  { pre = []
  , post = []
  }


data HooksConfig = HooksConfig
  { directory :: FilePath
  , launch :: HookSet
  , add :: HookSet
  , modify :: HookSet
  , exit :: HookSet
  -- TODO: , delete :: HookSet
  } deriving (Generic, Show)

instance ToJSON HooksConfig

-- | Necessary to make fields optional without using a Maybe type
instance FromJSON HooksConfig where
  parseJSON = withObject "hooksConfig" $ \o -> do
    directory  <- o .:? "directory" .!= directory defaultHooksConfig
    launch     <- o .:? "launch".!= launch defaultHooksConfig
    add        <- o .:? "add" .!= add defaultHooksConfig
    modify     <- o .:? "modify" .!= modify defaultHooksConfig
    exit       <- o .:? "exit" .!= exit defaultHooksConfig

    pure $ HooksConfig {..}

defaultHooksConfig :: HooksConfig
defaultHooksConfig = HooksConfig
  { directory = ""
  , launch = emptyHookSet
  , add = emptyHookSet
  , modify = emptyHookSet
  , exit = emptyHookSet
  }


addHookFilesToConfig :: Config -> [(FilePath, b, Text)]  -> Config
addHookFilesToConfig =
  let
    buildHook :: FilePath -> Text -> Hook
    buildHook filePath content =
      case lines content of
        firstLine : rest -> Hook
          { filePath = Just filePath
          , interpreter = firstLine
              & T.replace "#!" ""
              & T.strip
          , body = rest
              & unlines
              & T.strip
          }
        _ -> emptyHook

    addToHookSet :: Hook -> Text -> HookSet -> HookSet
    addToHookSet hook stage hookSet =
      case stage of
        "pre"  -> hookSet { pre  = (hookSet & pre) <> [hook] }
        "post" -> hookSet { post = (hookSet & post) <> [hook] }
        _ -> hookSet

    addToHooksConfig :: Text -> Text -> Hook -> HooksConfig -> HooksConfig
    addToHooksConfig event stage hook hConfig =
      case event of
        "launch" -> hConfig
          { launch = addToHookSet hook stage (hConfig & launch) }
        "add"    -> hConfig
          { add = addToHookSet hook stage (hConfig & add) }
        "modify" -> hConfig
          { modify = addToHookSet hook stage (hConfig & modify) }
        "exit"   -> hConfig
          { exit = addToHookSet hook stage (hConfig & exit) }
        _ -> hConfig

  in
    P.foldl $ \conf (filePath, _, fileContent) ->
        case split (== '-') $ pack $ takeBaseName filePath of
          [stage, event] ->
            conf { hooks = addToHooksConfig event stage
              (buildHook filePath fileContent)
              (conf & hooks)
            }

          _ -> conf


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
  , utcFormatShort :: TimeFormatString
  , dataDir :: FilePath
  , dbName :: FilePath
  , dateWidth :: Int
  , bodyWidth :: Int
  , prioWidth :: Int
  , headCount :: Int
  , maxWidth :: Int
  , progressBarWidth :: Int
  , hooks :: HooksConfig
  } deriving (Generic, Show)


-- | Necessary to make fields optional without using a Maybe type
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
    utcFormatShort  <- o .:? "utcFormatShort" .!= utcFormatShort defaultConfig
    dataDir         <- o .:? "dataDir" .!= dataDir defaultConfig
    dbName          <- o .:? "dbName" .!= dbName defaultConfig
    dateWidth       <- o .:? "dateWidth" .!= dateWidth defaultConfig
    bodyWidth       <- o .:? "bodyWidth" .!= bodyWidth defaultConfig
    prioWidth       <- o .:? "prioWidth" .!= prioWidth defaultConfig
    headCount       <- o .:? "headCount" .!= headCount defaultConfig
    maxWidth        <- o .:? "maxWidth" .!= maxWidth defaultConfig
    progressBarWidth <- o .:? "progressBarWidth"
                                  .!= progressBarWidth defaultConfig
    hooks           <- o .:? "hooks" .!= hooks defaultConfig


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
    func = if "dull" `T.isInfixOf` colorTxt
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
  , dateStyle = color Black
  , bodyStyle = color White
  , bodyClosedStyle = color Black
  , closedStyle = colorDull Black
  , dueStyle = color Yellow
  , overdueStyle = color Red
  , tagStyle = color Blue
  , utcFormat = toFormat ("YYYY-MM-DD H:MI:S" :: [Char])
  , utcFormatShort = toFormat ("YYYY-MM-DD H:MI" :: [Char])
  , dataDir = ""
  , dbName = "main.db"
  , dateWidth = 10
  , bodyWidth = 10
  , prioWidth = 4
  , headCount = 20
  , maxWidth = 120
  , progressBarWidth = 24
  , hooks = HooksConfig
      { directory = ""
      , launch = emptyHookSet
      , add = emptyHookSet
      , modify = emptyHookSet
      , exit = emptyHookSet
      }
  }
