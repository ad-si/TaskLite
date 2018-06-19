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
type TagText = Text

data Command
  = List (Filter TaskState)
  | AddTask IdText
  | DoTask IdText
  | EndTask IdText
  | DeleteTask IdText
  | AddTag IdText TagText
  | Count (Filter TaskState)
  | Csv
  | Ndjson
  -- TODO: Add demo mode
  -- | Demo
  | Help
  deriving (Show, Eq)


addParser :: Parser Command
addParser = AddTask <$>
  strArgument (metavar "BODY" <> help "Body of the task")

doneParser :: Parser Command
doneParser = DoTask <$>
  strArgument (metavar "TASK_ID" <> help "Id of the task (Ulid)")

countParser :: Parser Command
countParser = pure $ Count NoFilter


commandParser :: Parser Command
commandParser =
  pure (List $ Only Open)
  <|>
  ( subparser
    (  commandGroup "Basic Commands:"
    <> command "add" (toParserInfo addParser "Add a new task")
    <> command "do" (toParserInfo doneParser "Mark a task as done")
    <> command "end" (toParserInfo (EndTask <$>
        strArgument (metavar "TASK_ID" <> help "Id of the task (Ulid)"))
        "Mark a task as obsolete")
    <> command "delete" (toParserInfo (DeleteTask <$>
        strArgument (metavar "TASK_ID" <> help "Id of the task (Ulid)"))
        "Delete a task from the database (Attention: Irreversible)")
    <> command "tag" (toParserInfo (AddTag
      <$> strArgument (metavar "TASK_ID" <> help "Id of the task (Ulid)")
      <*> strArgument (metavar "TAG" <> help "The tag"))
      "Add a tag to a task")
    )
  <|> subparser
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
  <|> subparser
    (  commandGroup "Export Commands:"
    <> command "csv" (toParserInfo (pure Csv)
        "Export tasks in CSV format")
    <> command "ndjson" (toParserInfo (pure Ndjson)
        "Export tasks in NDJSON format")
    )
  <|> subparser
    (  commandGroup "Advanced Commands:"
    <> command "count" (toParserInfo countParser "Output total number of tasks")
    <> command "help" (toParserInfo (pure $ Help) "Display current help page")
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
    Csv -> dumpCsv
    Ndjson -> dumpNdjson
    AddTask body -> addTask body
    DoTask idSubstr -> doTask idSubstr
    EndTask idSubstr -> endTask idSubstr
    DeleteTask idSubstr -> deleteTask idSubstr
    AddTag idSubstr tagText  -> addTag idSubstr tagText
    Count taskFilter -> countTasks taskFilter
    Help -> handleParseResult . Failure $
      parserFailure defaultPrefs commandParserInfo ShowHelpText mempty
