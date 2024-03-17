module CliSpec where

import Protolude (($), (&))
import Protolude qualified as P

import Options.Applicative (
  ParseError (ShowHelpText),
  ParserFailure,
  ParserHelp,
  defaultPrefs,
  parserFailure,
  renderFailure,
 )
import Test.Hspec (Spec, describe, it, shouldContain)

import Cli (commandParserInfo)
import Config (defaultConfig)


spec :: Spec
spec = do
  describe "CLI" $ do
    it "should include header, body, and footer in help output" $ do
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
