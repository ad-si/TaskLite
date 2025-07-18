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

import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as T
import System.Process (readProcess)

import Config (Hook (body, filePath, interpreter))
import Control.Arrow ((>>>))
import ImportTask (ImportTask)
import Options.Applicative.Arrows (left)
import Prettyprinter (Doc, annotate, pretty)
import Prettyprinter.Render.Terminal (AnsiStyle, Color (Red, Yellow), color)
import System.FilePath (takeExtension)
import Utils ((<!!>))


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
        | s `P.elem` ["lua"] ->
            ("lua", ["-e"], ExecStdin)
        | s `P.elem` ["python", "python3", "py"] ->
            ("python3", ["-c"], ExecStdin)
        | s `P.elem` ["ruby", "rb"] ->
            ("ruby", ["-e"], ExecStdin)
        | s `P.elem` ["v", "vsh"] ->
            -- `crun` keeps the binary after execution
            ("v", ["-raw-vsh-tmp-prefix", "_v_executable_"], ExecFile)
        | otherwise ->
            ("", [""], ExecFile)

  hookToResult <-
    P.sequence $
      hooks <&> \hook -> do
        case hook.filePath of
          Just fPath -> do
            case fPath & takeExtension & P.drop 1 of
              "" ->
                -- Is executed with shell
                readProcess fPath [] stdinStr
              ext -> do
                let (interpreter, cliFlags, execMode) = getInterpreter ext
                case execMode of
                  ExecStdin -> do
                    fileContent <- P.readFile fPath
                    readProcess
                      interpreter
                      (P.concat [cliFlags, [T.unpack fileContent]])
                      stdinStr
                  ExecFile -> do
                    readProcess
                      interpreter
                      (P.concat [cliFlags, [fPath]])
                      stdinStr
          ---
          Nothing -> do
            let
              (interpreter, cliFlags, _) =
                getInterpreter (T.unpack hook.interpreter)
            readProcess
              interpreter
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


formatHookResult :: HookResult -> Doc AnsiStyle
formatHookResult hookResult =
  ""
    <!!> pretty hookResult.message
    <!!> annotate (color Yellow) (pretty hookResult.warning)
    <!!> annotate (color Red) (pretty hookResult.error)
    <!!> ""
