{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import Protolude

import Lib
import qualified Data.Text as T
import Options.Applicative


toParserInfo :: Parser a -> Text -> ParserInfo a
toParserInfo parser description =
  info (helper <*> parser) (fullDesc <> progDesc (T.unpack description))


data Command
  = List TaskState
  | AddTask Text
  | SetDone Text
  | Count TaskState
  deriving (Show, Eq)


addParser :: Parser Command
addParser = AddTask <$>
  strArgument (metavar "BODY" <> help "Body of the task")

addParserInfo :: ParserInfo Command
addParserInfo =
  toParserInfo addParser "Add a new task"


doneParser :: Parser Command
doneParser = SetDone <$>
  strArgument (metavar "TASK_ID" <> help "Id of the task (Ulid)")

doneParserInfo :: ParserInfo Command
doneParserInfo =
  toParserInfo doneParser "Mark a task as done"


countParser :: Parser Command
countParser = pure (Count Open)

countParserInfo :: ParserInfo Command
countParserInfo =
  toParserInfo countParser "Output number of open tasks"


commandParser :: Parser Command
commandParser =
  pure (List Open)
  <|>
  ( hsubparser
    (  commandGroup "Basic Commands:"
    <> command "add" addParserInfo
    <> command "done" doneParserInfo
    )
  <|> hsubparser
    (  commandGroup "Advanced Commands:"
    <> command "count" countParserInfo
    )
  )

commandParserInfo :: ParserInfo Command
commandParserInfo = info
  (commandParser <**> helper)
  fullDesc


main :: IO ()
main = do
  cliCommand <- execParser commandParserInfo
  case cliCommand of
    List taskState -> case taskState of
      Open -> listOpenTasks
      Waiting -> putStrLn ("List all waiting tasks" :: [Char])
      Done -> putStrLn ("List all done tasks" :: [Char])
      Obsolete -> putStrLn ("List all obsolete tasks" :: [Char])
    AddTask body -> addTask body
    SetDone idSubstr -> closeTask idSubstr
    Count taskState -> case taskState of
      Open -> putStrLn ("100" :: [Char])
      Waiting -> putStrLn ("20" :: [Char])
      Done -> putStrLn ("10" :: [Char])
      Obsolete -> putStrLn ("14" :: [Char])
