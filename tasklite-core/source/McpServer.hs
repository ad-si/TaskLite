{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module McpServer where

import Data.List (lookup)
import Protolude (
  IO,
  Maybe (..),
  Text,
  null,
  pure,
  putErrLn,
  show,
  words,
  ($),
  (<>),
 )

import Config (Config)
import Data.Either (Either (..))
import Lib (addTask, setupConnection)
import MCP.Server (
  ArgumentName,
  ArgumentValue,
  Content (ContentText),
  Error (InvalidRequest),
  InputSchemaDefinition (
    InputSchemaDefinitionObject,
    properties,
    required
  ),
  InputSchemaDefinitionProperty (
    InputSchemaDefinitionProperty,
    propertyDescription,
    propertyType
  ),
  McpServerHandlers (McpServerHandlers, prompts, resources, tools),
  McpServerInfo (..),
  PromptDefinition (..),
  PromptName,
  ResourceContent,
  ResourceDefinition,
  ToolDefinition (..),
  ToolName,
  URI,
  runMcpServerStdio,
 )


-- Server configuration
taskLiteServerInfo :: McpServerInfo
taskLiteServerInfo =
  McpServerInfo
    { serverName = "TaskLite MCP Server"
    , serverVersion = "0.1.0"
    , serverInstructions =
        "MCP server providing access to TaskLite task management functionality"
    }


promptListHandler :: IO [PromptDefinition]
promptListHandler =
  pure
    [ PromptDefinition
        { promptDefinitionName = "task_prompt"
        , promptDefinitionDescription = "A prompt for task management"
        , promptDefinitionArguments = []
        , promptDefinitionTitle = Nothing
        }
    ]


promptGetHandler ::
  PromptName ->
  [(ArgumentName, ArgumentValue)] ->
  IO (Either Error Content)
promptGetHandler _ _ = pure $ Left (InvalidRequest "No prompts available")


resourceListHandler :: IO [ResourceDefinition]
resourceListHandler = pure []


resourceGetHandler ::
  URI ->
  [(ArgumentName, ArgumentValue)] ->
  IO (Either Error ResourceContent)
resourceGetHandler _ _ = pure $ Left (InvalidRequest "No resources available")


toolGetHandler ::
  Config ->
  ToolName ->
  [(ArgumentName, ArgumentValue)] ->
  IO (Either Error Content)
toolGetHandler conf toolName args = do
  let params = case lookup "task_body" args of
        Just v -> v
        Nothing -> case lookup "task_id" args of
          Just v' -> v'
          Nothing -> ""
  result <- handleTool conf toolName params
  pure $ Right result


handleTool :: Config -> ToolName -> Text -> IO Content
handleTool conf "add_task" taskBody = do
  let bodyWords = words taskBody
  if null bodyWords
    then pure $ ContentText "Error: Task body cannot be empty"
    else do
      connection <- setupConnection conf
      task <- addTask conf connection bodyWords
      pure $ ContentText $ "Task added: " <> show task
handleTool _ _ _ = pure $ ContentText "Unknown tool"


toolListHandler :: IO [ToolDefinition]
toolListHandler =
  pure
    [ ToolDefinition
        { toolDefinitionName = "add_task"
        , toolDefinitionDescription = "Add a new task"
        , toolDefinitionInputSchema =
            InputSchemaDefinitionObject
              { properties =
                  [
                    ( "task_body"
                    , InputSchemaDefinitionProperty
                        { propertyType = "string"
                        , propertyDescription = "The body of the task to add"
                        }
                    )
                  ]
              , required = ["task_body"]
              }
        , toolDefinitionTitle = Nothing
        }
    ]


-- Start the MCP server
startMcpServer :: Config -> IO ()
startMcpServer conf = do
  let
    handlers =
      McpServerHandlers
        { prompts = Just (promptListHandler, promptGetHandler)
        , resources = Nothing
        , tools = Just (toolListHandler, toolGetHandler conf)
        }

  putErrLn @Text "Starting TaskLite MCP Server …"
  runMcpServerStdio taskLiteServerInfo handlers
