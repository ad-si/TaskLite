{-|
Module for executing Lua scripts using an embedded interpreter (HsLua).
This replaces the previous approach of spawning external `lua` processes.
-}
module LuaRunner (runLuaHook) where

import Protolude (
  Applicative (pure),
  IO,
  Maybe (..),
  Text,
  ($),
  (<>),
 )
import Protolude qualified as P

import Control.Exception (try)
import Control.Monad (forM)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import HsLua (
  Exception,
  Lua,
  NumResults (..),
  Result (..),
  Status (..),
  dostring,
  getfield,
  getglobal,
  gettop,
  liftIO,
  newtable,
  openlibs,
  pop,
  pushHaskellFunction,
  pushnil,
  pushstring,
  run,
  runPeek,
  setfield,
  setglobal,
  tostring,
 )
import HsLua.Aeson (peekValue, pushValue)


-- | Run a Lua script with JSON input, return the captured output
runLuaHook :: Text -> Text -> IO (P.Either Text Text)
runLuaHook luaCode stdinJson = do
  -- Create an IORef to capture print output
  outputRef <- newIORef ""
  -- Create an IORef to track if stdin has been consumed
  stdinConsumedRef <- newIORef P.False

  result <- try @Exception $ run @Exception $ do
    -- Open standard Lua libraries
    openlibs

    -- Set up tl.json module for JSON parsing
    setupTlJsonModule

    -- Set up custom io.read to return our stdin content (nil after first read)
    setupStdinReader stdinJson stdinConsumedRef

    -- Set up custom print function to capture output
    setupPrintCapture outputRef

    -- Execute the Lua code
    status <- dostring (TE.encodeUtf8 luaCode)
    case status of
      OK -> pure $ P.Right ()
      _err -> do
        errMsg <- tostring (-1)
        pure $ P.Left $ case errMsg of
          Just msg -> TE.decodeUtf8Lenient msg
          Nothing -> "Lua error: " <> T.pack (P.show status)

  case result of
    P.Left exception ->
      pure $ P.Left $ "Lua execution failed: " <> T.pack (P.show exception)
    P.Right (P.Left errMsg) ->
      pure $ P.Left errMsg
    P.Right (P.Right ()) -> do
      output <- readIORef outputRef
      pure $ P.Right output


-- | Override io.read to return our stdin content (only on first call, then nil)
setupStdinReader :: Text -> IORef P.Bool -> Lua ()
setupStdinReader stdinContent consumedRef = do
  -- Get the io table
  _ <- getglobal "io"

  -- Create a function that returns the stdin content on first call, nil thereafter
  let readFunc :: Lua NumResults
      readFunc = do
        consumed <- liftIO $ readIORef consumedRef
        if consumed
          then do
            pushnil
            pure (NumResults 1)
          else do
            liftIO $ modifyIORef' consumedRef (P.const P.True)
            pushstring (TE.encodeUtf8 stdinContent)
            pure (NumResults 1)

  -- Set io.read to our custom function
  pushHaskellFunction readFunc
  setfield (-2) "read"

  -- Pop the io table
  pop 1


{-| Override print to capture output to an IORef
Mimics standard Lua print: handles multiple arguments separated by tabs,
appends a newline at the end
-}
setupPrintCapture :: IORef Text -> Lua ()
setupPrintCapture outputRef = do
  let printFunc :: Lua NumResults
      printFunc = do
        -- Get the number of arguments
        nargs <- gettop
        -- Convert each argument to string
        parts <- forM [1 .. nargs] $ \i -> do
          strMb <- tostring i
          pure $ case strMb of
            Just str -> TE.decodeUtf8Lenient str
            Nothing -> ""
        -- Join with tabs and append newline (like standard Lua print)
        liftIO $ modifyIORef' outputRef (<> T.intercalate "\t" parts <> "\n")
        pure (NumResults 0)

  pushHaskellFunction printFunc
  setglobal "print"


{-| Set up the tl.json module for JSON encoding/decoding.
This provides a namespaced JSON module for TaskLite Lua hooks.
-}
setupTlJsonModule :: Lua ()
setupTlJsonModule = do
  -- Create the json table with decode/encode/null
  newtable

  -- Add json.decode function
  let decodeFunc :: Lua NumResults
      decodeFunc = do
        strMb <- tostring 1
        case strMb of
          Nothing -> do
            pushnil
            pure (NumResults 1)
          Just jsonBytes -> do
            let jsonText = TE.decodeUtf8Lenient jsonBytes
            case Aeson.eitherDecode (BL.fromStrict $ TE.encodeUtf8 jsonText) of
              P.Left _err -> do
                pushnil
                pure (NumResults 1)
              P.Right value -> do
                pushValue value
                pure (NumResults 1)

  pushHaskellFunction decodeFunc
  setfield (-2) "decode"

  -- Add json.encode function
  let encodeFunc :: Lua NumResults
      encodeFunc = do
        -- Peek the Lua value at stack position 1 and convert to Aeson Value
        valueResult <- runPeek $ peekValue 1
        case valueResult of
          Failure _ _ -> do
            pushstring "null"
            pure (NumResults 1)
          Success value -> do
            let jsonBytes = BL.toStrict $ Aeson.encode value
            pushstring jsonBytes
            pure (NumResults 1)

  pushHaskellFunction encodeFunc
  setfield (-2) "encode"

  -- Add json.null constant for comparing against JSON null values
  pushnil
  setfield (-2) "null"

  -- Create the tl namespace table
  newtable
  -- Stack: json, tl

  -- Get json table and set it as tl.json
  _ <- getglobal "package"
  _ <- getfield (-1) "loaded"
  -- Stack: json, tl, package, loaded

  -- We need to set up tl.json, so first set json into tl
  pop 2
  -- Stack: json, tl

  -- Duplicate json table and assign to tl.json
  _ <- pushstring "json"
  -- Stack: json, tl, "json"
  -- We need to copy the json table (at -3) to be the value
  -- Actually, let's restructure this more clearly

  -- Pop and start fresh with a cleaner approach
  pop 2

  -- Create tl table
  newtable
  -- Stack: tl

  -- Create json table inside tl
  newtable
  -- Stack: tl, json

  -- Add decode to json
  pushHaskellFunction decodeFunc
  setfield (-2) "decode"

  -- Add encode to json
  pushHaskellFunction encodeFunc
  setfield (-2) "encode"

  -- Add null to json
  pushnil
  setfield (-2) "null"

  -- Set json as field of tl
  setfield (-2) "json"
  -- Stack: tl (with tl.json set)

  -- Register tl as global
  setglobal "tl"

  -- Make tl.json available via require("tl.json")
  _ <- getglobal "package"
  _ <- getfield (-1) "loaded"
  -- Stack: package, loaded
  _ <- getglobal "tl"
  _ <- getfield (-1) "json"
  -- Stack: package, loaded, tl, json
  setfield (-3) "tl.json"
  -- Stack: package, loaded, tl
  pop 3
