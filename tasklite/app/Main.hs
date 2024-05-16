{-|
This is the main module which provides the CLI
-}
module Main where

import Protolude (
  Bool (True),
  Either (..),
  IO,
  Maybe (Nothing),
  die,
  writeFile,
  ($),
 )

import Data.Text qualified as T
import Data.Yaml (decodeFileEither, prettyPrintParseException)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import System.Directory (
  XdgDirectory (..),
  createDirectoryIfMissing,
  getXdgDirectory,
 )
import System.FilePath ((</>))

import Cli (exampleConfig, printOutput)


main :: IO ()
main = do
  -- Necessary for Docker image
  setLocaleEncoding utf8

  let appName = "tasklite"

  configDirectory <- getXdgDirectory XdgConfig appName
  createDirectoryIfMissing True configDirectory

  let configPath = configDirectory </> "config.yaml"

  configResult <- decodeFileEither configPath

  case configResult of
    Left error -> do
      if "file not found"
        `T.isInfixOf` T.pack (prettyPrintParseException error)
        then do
          writeFile configPath exampleConfig
          configResult2 <- decodeFileEither configPath

          case configResult2 of
            Left error2 -> die $ T.pack $ prettyPrintParseException error2
            Right configUser -> printOutput appName Nothing configUser
        else die $ T.pack $ prettyPrintParseException error
    Right configUser ->
      printOutput appName Nothing configUser
