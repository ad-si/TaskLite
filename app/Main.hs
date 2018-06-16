{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where

import Protolude

import Lib
import qualified Data.Text as T
import Options.Applicative
import Utils

toParserInfo :: Parser a -> Text -> ParserInfo a
toParserInfo parser description =
  info (helper <*> parser) (fullDesc <> progDesc (T.unpack description))

type IdText = Text

data Command
  = List (Filter TaskState)
  | AddTask IdText
  | DoTask IdText
  | EndTask IdText
  | Count (Filter TaskState)
  deriving (Show, Eq)


addParser :: Parser Command
addParser = AddTask <$>
  strArgument (metavar "BODY" <> help "Body of the task")

addParserInfo :: ParserInfo Command
addParserInfo =
  toParserInfo addParser "Add a new task"


doneParser :: Parser Command
doneParser = DoTask <$>
  strArgument (metavar "TASK_ID" <> help "Id of the task (Ulid)")

doneParserInfo :: ParserInfo Command
doneParserInfo =
  toParserInfo doneParser "Mark a task as done"


countParser :: Parser Command
countParser = pure $ Count NoFilter

countParserInfo :: ParserInfo Command
countParserInfo =
  toParserInfo countParser "Output number of open tasks"


commandParser :: Parser Command
commandParser =
  pure (List $ Only Open)
  <|>
  ( hsubparser
    (  commandGroup "Basic Commands:"
    <> command "add" addParserInfo
    <> command "do" doneParserInfo
    <> command "end" (toParserInfo (DoTask <$>
        strArgument (metavar "TASK_ID" <> help "Id of the task (Ulid)"))
        "Mark a task as obsolete")
    )
  <|> hsubparser
    (  commandGroup "List Commands:"
    <> command "all" (toParserInfo (pure $ List NoFilter)
        "List all tasks")
    <> command "done" (toParserInfo (pure $ List $ Only Done)
        "List all done tasks")
    <> command "waiting" (toParserInfo (pure $ List $ Only Waiting)
        "List all waiting tasks")
    <> command "obsolete" (toParserInfo (pure $ List $ Only Obsolete)
        "List all obsolete tasks")
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
    List taskFilter -> listTasks taskFilter
    AddTask body -> addTask body
    DoTask idSubstr -> doTask idSubstr
    EndTask idSubstr -> endTask idSubstr
    Count taskFilter -> countTasks taskFilter
