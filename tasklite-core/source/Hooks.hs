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
import System.FilePath (takeExtension)


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


data HookResult
  = BasicHookResult
      { message :: Maybe Text
      , warning :: Maybe Text
      , error :: Maybe Text
      }
  | PreAddHookResult
      { taskToAdd :: Maybe ImportTask
      , message :: Maybe Text
      , warning :: Maybe Text
      , error :: Maybe Text
      }
  | PostAddHookResult
      { taskAdded :: ImportTask
      , message :: Maybe Text
      , warning :: Maybe Text
      , error :: Maybe Text
      }
  | PreModifyHookResult
      { taskToModify :: ImportTask
      , message :: Maybe Text
      , warning :: Maybe Text
      , error :: Maybe Text
      }
  | PostModifyHookResult
      { taskModified :: ImportTask
      , message :: Maybe Text
      , warning :: Maybe Text
      , error :: Maybe Text
      }
  deriving (Show, P.Generic)


instance Aeson.FromJSON HookResult where
  parseJSON = Aeson.withObject "PreAddHookResult" $ \v -> do
    message <- v Aeson..:? "message"
    warning <- v Aeson..:? "warning"
    error <- v Aeson..:? "error"

    taskToAddMb <- v Aeson..:? "taskToAdd"
    taskAddedMb <- v Aeson..:? "taskAdded"
    taskToModifyMb <- v Aeson..:? "taskToModify"
    taskModifiedMb <- v Aeson..:? "taskModified"

    case (taskToAddMb, taskAddedMb, taskToModifyMb, taskModifiedMb) of
      (Just taskToAdd, _, _, _) -> do
        pure $ PreAddHookResult taskToAdd message warning error
      (_, Just taskAdded, _, _) -> do
        pure $ PostAddHookResult taskAdded message warning error
      (_, _, Just taskToModify, _) -> do
        pure $ PreModifyHookResult taskToModify message warning error
      (_, _, _, Just taskModified) -> do
        pure $ PostModifyHookResult taskModified message warning error
      (_, _, _, _) -> do
        pure $ BasicHookResult message warning error


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
                -- Is excuted with shell
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
