{-# LANGUAGE QuasiQuotes #-}

module CliSpec where

import Protolude (Bool (False, True), Maybe (Just, Nothing), ($), (&), (<>))
import Protolude qualified as P

import Data.Text qualified as T
import Options.Applicative (
  ParseError (ShowHelpText),
  ParserFailure,
  ParserHelp,
  defaultPrefs,
  parserFailure,
  renderFailure,
 )
import Prettyprinter (annotate, defaultLayoutOptions, pretty)
import Prettyprinter.Render.Terminal (Color (Red), bold, color)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec (
  Spec,
  describe,
  it,
  shouldBe,
  shouldContain,
  shouldNotContain,
 )

import Cli (
  commandParserInfo,
  hPutDocWithConfig,
  printOutput,
  renderIOWithConfig,
 )
import Config (Config, Hook (Hook), HookSet (HookSet), defaultConfig)
import Config qualified
import System.Directory (
  Permissions (executable, readable),
  emptyPermissions,
  setPermissions,
 )
import Utils (raw)


createHook :: Config -> P.FilePath -> P.Text -> P.IO ()
createHook conf name content = do
  let filePath = conf.hooks.directory <> "/" <> name
  P.writeFile filePath content
  setPermissions
    filePath
    ( emptyPermissions
        { executable = P.True
        , readable = P.True
        }
    )


spec :: P.FilePath -> Spec
spec tmpDirPath = do
  describe "CLI" $ do
    it "includes header, body, and footer in help output" $ do
      let
        failure :: ParserFailure ParserHelp =
          parserFailure
            defaultPrefs
            (commandParserInfo defaultConfig)
            (ShowHelpText P.Nothing)
            []
        helpText =
          renderFailure failure "xxx" & P.fst

      helpText `shouldContain` "Usage: xxx"
      helpText `shouldContain` "developed by"

    it "prints current version" $ do
      _ <- printOutput "test-app" (Just ["version"]) defaultConfig
      () `shouldBe` ()

    it "calls task lifecycle hooks (add, modify) stored in config" $ do
      let
        getLuaHook body =
          Hook
            { Config.filePath = Nothing
            , Config.interpreter = "lua"
            , Config.body = body
            }
        preAddHook =
          getLuaHook
            [raw|
              io.stderr:write("🏃 Executing pre-add script …\n")
              io.stderr:write(
                "ℹ️ Receives an object with arguments:\n",
                io.read("*a"),
                "\n"
              )
              -- print("{}")
            |]
        postAddHook =
          getLuaHook
            [raw|
              io.stderr:write("🏃 Executing post-add script …\n")
              io.stderr:write(
                "ℹ️ Receives an object with arguments:\n",
                  io.read("*a"),
                  "\n"
              )
              -- print("{}")
            |]
        preModifyHook =
          getLuaHook
            [raw|
              io.stderr:write("🏃 Executing pre-modify script …\n")
              io.stderr:write(
                "ℹ️ Receives an object with arguments:\n",
                io.read("*a"),
                "\n"
              )
              -- print("{}")
            |]
        postModifyHook =
          getLuaHook
            [raw|
              io.stderr:write("🏃 Executing post-modify script …\n")
              io.stderr:write(
                "ℹ️ Receives an object with arguments:\n",
                io.read("*a"),
                "\n"
              )
              -- print("{}")
            |]
        testConf =
          defaultConfig
            { Config.hooks =
                defaultConfig.hooks
                  { Config.add =
                      HookSet
                        { pre = [preAddHook]
                        , post = [postAddHook]
                        }
                  , Config.modify =
                      HookSet
                        { pre = [preModifyHook]
                        , post = [postModifyHook]
                        }
                  }
            }

      _ <- printOutput "test-app" (Just ["add", "buy milk"]) testConf

      () `shouldBe` ()

    it "calls launch hooks stored in files" $ do
      let
        testConf =
          defaultConfig
            { Config.hooks =
                defaultConfig.hooks{Config.directory = tmpDirPath}
            }
        hookFor = createHook testConf

      hookFor
        "pre-launch.lua"
        [raw|
          io.stderr:write("🏃 Executing pre-launch script …\n")
          io.stderr:write(
            "ℹ️ Receives no input:",
            io.read("*a"),
            "\n"
          )
        |]

      hookFor
        "post-launch.lua"
        [raw|
          io.stderr:write("🏃 Executing post-launch script …\n")
          io.stderr:write(
            "ℹ️ Receives an object with arguments:",
            io.read("*a"),
            "\n"
          )
        |]

      _ <- printOutput "test-app" (Just ["head"]) testConf

      () `shouldBe` ()

    it "hPutDocWithConfig outputs plain text when noColor is True" $ do
      withSystemTempFile "test-output.txt" $ \filePath handle -> do
        let
          testDoc = annotate (color Red <> bold) (pretty ("Styled text" :: P.Text))
          conf = defaultConfig{Config.noColor = True}

        hPutDocWithConfig conf handle testDoc
        hClose handle

        -- Read the file content
        content <- P.readFile filePath
        -- Should contain plain text without ANSI escape codes
        T.unpack content `shouldNotContain` "\ESC["
        T.unpack content `shouldContain` "Styled text"

    it "hPutDocWithConfig outputs ANSI codes when noColor is False" $ do
      withSystemTempFile "test-output.txt" $ \filePath handle -> do
        let
          testDoc = annotate (color Red) (pretty ("Styled text" :: P.Text))
          conf = defaultConfig{Config.noColor = False}

        hPutDocWithConfig conf handle testDoc
        hClose handle

        -- Read the file content
        content <- P.readFile filePath
        -- Should contain ANSI escape codes
        T.unpack content `shouldContain` "\ESC["

    it "renderIOWithConfig outputs plain text when noColor is True" $ do
      withSystemTempFile "test-output.txt" $ \filePath handle -> do
        let
          testDoc = annotate bold (pretty ("Rendered text" :: P.Text))
          conf = defaultConfig{Config.noColor = True}
          layoutOpts = defaultLayoutOptions

        renderIOWithConfig conf handle layoutOpts testDoc
        hClose handle

        -- Read the file content
        content <- P.readFile filePath
        -- Should contain plain text without ANSI escape codes
        T.unpack content `shouldNotContain` "\ESC["
        T.unpack content `shouldContain` "Rendered text"

    it "renderIOWithConfig outputs ANSI codes when noColor is False" $ do
      withSystemTempFile "test-output.txt" $ \filePath handle -> do
        let
          testDoc = annotate bold (pretty ("Rendered text" :: P.Text))
          conf = defaultConfig{Config.noColor = False}
          layoutOpts = defaultLayoutOptions

        renderIOWithConfig conf handle layoutOpts testDoc
        hClose handle

        -- Read the file content
        content <- P.readFile filePath
        -- Should contain ANSI escape codes
        T.unpack content `shouldContain` "\ESC["
