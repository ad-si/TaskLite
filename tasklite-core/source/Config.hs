{-|
The default config primarily defines the styling and formatting
-}
module Config where

import Protolude (
  Applicative (pure),
  Bool (False),
  Char,
  Eq ((==)),
  FilePath,
  Functor (fmap),
  Generic,
  Int,
  Maybe (..),
  Semigroup ((<>)),
  Show,
  Text,
  decodeUtf8,
  fromMaybe,
  lines,
  show,
  unlines,
  ($),
  (&),
  (.),
  (<=),
  (>>=),
 )
import Protolude qualified as P

import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  Value (String),
  withObject,
  withText,
  (.!=),
  (.:?),
 )
import Data.Hourglass (TimeFormat (toFormat), TimeFormatString)
import Data.Text (dropEnd, pack, split, stripPrefix)
import Data.Text qualified as T
import Data.Yaml (encode)
import Prettyprinter.Internal (Pretty (pretty))
import Prettyprinter.Render.Terminal (AnsiStyle, Color (..), color, colorDull)
import Prettyprinter.Render.Terminal.Internal (ansiForeground)
import System.FilePath (takeBaseName)


data Hook = Hook
  { filePath :: Maybe FilePath
  , interpreter :: Text
  , body :: Text
  }
  deriving (Generic, Show)


instance ToJSON Hook
instance FromJSON Hook


emptyHook :: Hook
emptyHook =
  Hook
    { filePath = Nothing
    , interpreter = ""
    , body = ""
    }


data HookSet = HookSet
  { pre :: [Hook]
  , post :: [Hook]
  }
  deriving (Generic, Show)


instance ToJSON HookSet


-- | Necessary to make fields optional without using a Maybe type
instance FromJSON HookSet where
  parseJSON = withObject "hookSet" $ \o -> do
    pre <- o .:? "pre" .!= pre emptyHookSet
    post <- o .:? "post" .!= post emptyHookSet

    pure $ HookSet{..}


emptyHookSet :: HookSet
emptyHookSet =
  HookSet
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
  }
  deriving (Generic, Show)


instance ToJSON HooksConfig


-- | Necessary to make fields optional without using a Maybe type
instance FromJSON HooksConfig where
  parseJSON = withObject "hooksConfig" $ \o -> do
    directory <- o .:? "directory" .!= directory defaultHooksConfig
    launch <- o .:? "launch" .!= launch defaultHooksConfig
    add <- o .:? "add" .!= add defaultHooksConfig
    modify <- o .:? "modify" .!= modify defaultHooksConfig
    exit <- o .:? "exit" .!= exit defaultHooksConfig

    pure $ HooksConfig{..}


defaultHooksConfig :: HooksConfig
defaultHooksConfig =
  HooksConfig
    { directory = ""
    , launch = emptyHookSet
    , add = emptyHookSet
    , modify = emptyHookSet
    , exit = emptyHookSet
    }


addHookFilesToConfig :: Config -> [(FilePath, b, Text)] -> (Config, [Text])
addHookFilesToConfig config = do
  let
    buildHook :: FilePath -> Text -> Hook
    buildHook filePath content =
      case lines content of
        firstLine : rest ->
          Hook
            { filePath = Just filePath
            , interpreter =
                firstLine
                  & T.replace "#!" ""
                  & T.strip
            , body =
                rest
                  & unlines
                  & T.strip
            }
        _ -> emptyHook

    addToHookSet :: Hook -> Text -> HookSet -> HookSet
    addToHookSet hook stage hookSet =
      case stage of
        "pre" -> hookSet{pre = hookSet.pre <> [hook]}
        "post" -> hookSet{post = hookSet.post <> [hook]}
        _ -> hookSet

    addToHooksConfig :: Text -> Text -> Hook -> HooksConfig -> HooksConfig
    addToHooksConfig event stage hook hConfig =
      case event of
        "launch" ->
          hConfig
            { launch = addToHookSet hook stage (hConfig & launch)
            }
        "add" ->
          hConfig
            { add = addToHookSet hook stage (hConfig & add)
            }
        "modify" ->
          hConfig
            { modify = addToHookSet hook stage (hConfig & modify)
            }
        "exit" ->
          hConfig
            { exit = addToHookSet hook stage (hConfig & exit)
            }
        _ -> hConfig

    getStageAndEvent :: FilePath -> [Text]
    getStageAndEvent filePath = do
      let fileName =
            filePath
              & takeBaseName
              & pack

      -- Prefix with "_" to ignore files
      if "_" `T.isPrefixOf` fileName
        then []
        else
          fileName
            & split (== '_')
            & P.headMay
            & fromMaybe ""
            & split (== '-')

  P.foldl'
    ( \(conf, errors) (filePath, _, fileContent) ->
        case getStageAndEvent filePath of
          [stage, event] ->
            ( conf
                { hooks =
                    addToHooksConfig
                      event
                      stage
                      (buildHook filePath fileContent)
                      conf.hooks
                }
            , errors <> []
            )
          [] -> (conf, errors)
          filenameParts ->
            ( conf
            , errors
                <> [ ("`" <> (filenameParts & T.intercalate "-") <> "` ")
                      <> "is not a correct hook name.\n"
                      <> "Hook file names must be in the format: "
                      <> "<stage>-<event>_<description>.<ext>\n"
                      <> "E.g `post-launch.v`, or `pre-exit.lua`.\n"
                   ]
            )
    )
    (config, [])


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
  , maxWidth :: Maybe Int -- Automatically uses terminal width if not set
  , progressBarWidth :: Int
  , hooks :: HooksConfig
  , noColor :: Bool
  }
  deriving (Generic, Show)


-- | Necessary to make fields optional without using a Maybe type
{- FOURMOLU_DISABLE -}
instance FromJSON Config where
  parseJSON = withObject "config" $ \o -> do
    idStyle         <- o .:? "idStyle" .!= defaultConfig.idStyle
    priorityStyle   <- o .:? "priorityStyle".!= defaultConfig.priorityStyle
    tableName       <- o .:? "tableName" .!= defaultConfig.tableName
    dateStyle       <- o .:? "dateStyle" .!= defaultConfig.dateStyle
    bodyStyle       <- o .:? "bodyStyle" .!= defaultConfig.bodyStyle
    bodyClosedStyle <- o .:? "bodyClosedStyle".!= defaultConfig.bodyClosedStyle
    closedStyle     <- o .:? "closedStyle" .!= defaultConfig.closedStyle
    dueStyle        <- o .:? "dueStyle" .!= defaultConfig.dueStyle
    overdueStyle    <- o .:? "overdueStyle" .!= defaultConfig.overdueStyle
    tagStyle        <- o .:? "tagStyle" .!= defaultConfig.tagStyle
    utcFormat       <- o .:? "utcFormat" .!= defaultConfig.utcFormat
    utcFormatShort  <- o .:? "utcFormatShort" .!= defaultConfig.utcFormatShort
    dataDir         <- o .:? "dataDir" .!= defaultConfig.dataDir
    dbName          <- o .:? "dbName" .!= defaultConfig.dbName
    dateWidth       <- o .:? "dateWidth" .!= defaultConfig.dateWidth
    bodyWidth       <- o .:? "bodyWidth" .!= defaultConfig.bodyWidth
    prioWidth       <- o .:? "prioWidth" .!= defaultConfig.prioWidth
    headCount       <- o .:? "headCount" .!= defaultConfig.headCount
    maxWidthMb     <- o .:? "maxWidth"
    progressBarWidth <- o .:? "progressBarWidth"
                                .!= defaultConfig.progressBarWidth
    hooks           <- o .:? "hooks" .!= defaultConfig.hooks
    noColor         <- o .:? "noColor" .!= defaultConfig.noColor

    let maxWidth = maxWidthMb >>=
          \w -> if w <= 0 then defaultConfig.maxWidth else Just w

    pure $ Config{..}

{- FOURMOLU_ENABLE -}


instance ToJSON Config


instance Pretty Config where
  pretty =
    pretty
      . dropEnd 1 -- Drop trailing newline to maybe add it later
      . decodeUtf8
      . Data.Yaml.encode


parseColor :: Text -> Maybe Color
parseColor colorTxt =
  let
    colorOnly = fromMaybe colorTxt $ stripPrefix "dull " colorTxt
    colorToType = \case
      "black" -> Just Black
      "red" -> Just Red
      "green" -> Just Green
      "yellow" -> Just Yellow
      "blue" -> Just Blue
      "magenta" -> Just Magenta
      "cyan" -> Just Cyan
      "white" -> Just White
      _ -> Nothing
  in
    colorToType colorOnly


parseAnsiStyle :: Text -> Maybe AnsiStyle
parseAnsiStyle colorTxt =
  let
    func =
      if "dull" `T.isInfixOf` colorTxt
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
defaultConfig =
  Config
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
    , maxWidth = Nothing
    , progressBarWidth = 24
    , hooks =
        HooksConfig
          { directory = ""
          , launch = emptyHookSet
          , add = emptyHookSet
          , modify = emptyHookSet
          , exit = emptyHookSet
          }
    , noColor = False
    }
