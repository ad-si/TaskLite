{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use maybe" #-}

module Hooks where

import Protolude (
  Applicative (pure),
  IO,
  Maybe (..),
  Show,
  otherwise,
  ($),
  (&),
  (<&>),
 )
import Protolude qualified as P

import Control.Arrow ((>>>))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Options.Applicative.Arrows (left)
import Prettyprinter (Doc, annotate, pretty)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (Red, Yellow))
import System.FilePath (takeExtension)
import System.Process (readProcess)

import Config (Config, Hook (body, filePath, interpreter))
import ImportTask (ImportTask)
import LuaRunner (runLuaHook)
import Utils (colr, (<!!>))


data HookTiming = PreEvent | PostEvent
  deriving (Show)


data HookType
  = LaunchHook
  | AddHook
  | ModifyHook
  | ExitHook
  deriving (Show)


data HookEvent = HookEvent HookType HookTiming
  deriving (Show)


-- | Output of a hook that must be parsed by TaskLite
data HookResult
  = BasicHookResult
      { message :: Maybe Text
      , warning :: Maybe Text
      , error :: Maybe Text
      }
  | TaskHookResult
      { task :: Maybe ImportTask
      , message :: Maybe Text
      , warning :: Maybe Text
      , error :: Maybe Text
      }
  deriving (Show, P.Generic)


instance Aeson.FromJSON HookResult where
  parseJSON = Aeson.withObject "HookResult" $ \v -> do
    taskMb <- v Aeson..:? "task"
    messageMb <- v Aeson..:? "message"
    warningMb <- v Aeson..:? "warning"
    errorMb <- v Aeson..:? "error"

    case taskMb of
      Just task -> pure $ TaskHookResult task messageMb warningMb errorMb
      Nothing -> pure $ BasicHookResult messageMb warningMb errorMb


data ExecMode = ExecFile | ExecStdin


type String = [P.Char]


executeHooks :: Text -> [Hook] -> IO [P.Either Text HookResult]
executeHooks stdinText hooks = do
  let
    stdinStr = T.unpack stdinText

    getInterpreter :: String -> (String, [String], ExecMode)
    getInterpreter s =
      if
        | s `P.elem` ["javascript", "js", "node", "node.js"] ->
            ("node", ["-e"], ExecStdin)
        | s `P.elem` ["python", "python3", "py"] ->
            ("python3", ["-c"], ExecStdin)
        | s `P.elem` ["ruby", "rb"] ->
            ("ruby", ["-e"], ExecStdin)
        | s `P.elem` ["v", "vsh"] ->
            -- `crun` keeps the binary after execution
            ("v", ["-raw-vsh-tmp-prefix", "_v_executable_"], ExecFile)
        | otherwise ->
            ("", [""], ExecFile)

    -- Execute a Lua hook using the embedded interpreter
    runEmbeddedLua :: Text -> IO String
    runEmbeddedLua luaCode = do
      result <- runLuaHook luaCode stdinText
      case result of
        P.Right output -> pure $ T.unpack output
        P.Left err ->
          pure $
            T.unpack $
              TE.decodeUtf8 $
                BL.toStrict $
                  Aeson.encode $
                    Aeson.object ["error" Aeson..= err]

    -- Check if a file extension or interpreter name indicates Lua
    isLua :: String -> P.Bool
    isLua s = s `P.elem` ["lua"]

  hookToResult <-
    P.sequence $
      hooks <&> \hook -> do
        case hook.filePath of
          Just fPath -> do
            case fPath & takeExtension & P.drop 1 of
              "" ->
                -- Is executed with shell
                readProcess fPath [] stdinStr
              ext
                -- Use embedded Lua interpreter for .lua files
                | isLua ext -> do
                    fileContent <- P.readFile fPath
                    runEmbeddedLua fileContent
                | otherwise -> do
                    let (interpreterCmd, cliFlags, execMode) = getInterpreter ext
                    case execMode of
                      ExecStdin -> do
                        fileContent <- P.readFile fPath
                        readProcess
                          interpreterCmd
                          (P.concat [cliFlags, [T.unpack fileContent]])
                          stdinStr
                      ExecFile -> do
                        readProcess
                          interpreterCmd
                          (P.concat [cliFlags, [fPath]])
                          stdinStr
          ---
          Nothing -> do
            -- Use embedded Lua interpreter for inline Lua hooks
            if isLua (T.unpack hook.interpreter)
              then runEmbeddedLua hook.body
              else do
                let
                  (interpreterCmd, cliFlags, _) =
                    getInterpreter (T.unpack hook.interpreter)
                readProcess
                  interpreterCmd
                  (P.concat [cliFlags, [T.unpack hook.body]])
                  stdinStr

  let parsedHookResults :: [P.Either Text HookResult] =
        hookToResult
          & P.filter (T.pack >>> T.strip >>> T.null >>> P.not)
            <&> ( ( \hookOutput -> do
                      Aeson.eitherDecodeStrictText (T.pack hookOutput)
                  )
                    >>> left T.pack
                )

  pure parsedHookResults


formatHookResult :: Config -> HookResult -> Doc AnsiStyle
formatHookResult conf hookResult =
  ""
    <!!> pretty hookResult.message
    <!!> annotate (colr conf Yellow) (pretty hookResult.warning)
    <!!> annotate (colr conf Red) (pretty hookResult.error)
    <!!> ""
