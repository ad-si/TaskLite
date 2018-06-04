{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import Protolude

-- import Lib
import qualified Data.Text as T
import Data.ULID
import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple
import Options.Applicative


toParserInfo :: Parser a -> Text -> ParserInfo a
toParserInfo parser description =
  info (helper <*> parser) (fullDesc <> progDesc (T.unpack description))


data Command
  = Add Text
  | Done Text
  | Count
  deriving (Show, Eq)


addParser :: Parser Command
addParser = Add <$>
  strArgument (metavar "BODY" <> help "Body of the task")

addParserInfo :: ParserInfo Command
addParserInfo =
  toParserInfo addParser "Add a new task"


doneParser :: Parser Command
doneParser = Done <$>
  strArgument (metavar "TASK_ID" <> help "Id of the task (Ulid)")

doneParserInfo :: ParserInfo Command
doneParserInfo =
  toParserInfo doneParser "Mark a task as done"


countParser :: Parser Command
countParser = pure Count

countParserInfo :: ParserInfo Command
countParserInfo =
  toParserInfo countParser "Output number of open tasks"


commandParser :: Parser Command
commandParser =
  hsubparser
    (  commandGroup "Basic Commands:"
    <> command "add" addParserInfo
    <> command "done" doneParserInfo
    )
  <|> hsubparser
    (  commandGroup "Advanced Commands:"
    <> command "count" countParserInfo
    )

commandParserInfo :: ParserInfo Command
commandParserInfo = info
  (commandParser <**> helper)
  fullDesc


main :: IO ()
main = do
  command <- execParser commandParserInfo
  case command of
    Add body -> putStrLn $ "Add task \"" <> body <> "\""
    Done id -> putStrLn $ "Close task with id \"" <> id <> "\""
    Count -> putStrLn ("100" :: [Char])
