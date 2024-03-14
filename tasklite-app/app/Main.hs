{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Protolude as P hiding (on)

import Control.Monad (void)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text as T
import Data.Vector (Vector)
import Pipes
import System.Directory (
  XdgDirectory (..),
  createDirectoryIfMissing,
  getHomeDirectory,
  getXdgDirectory,
 )
import System.FilePath ((</>))

import Data.Yaml (decodeFileEither, prettyPrintParseException)
import Database.SQLite.Simple (close, query_)
import Lib

import GI.Gtk (
  Label (..),
  ListBox (..),
  ListBoxRow (..),
  ScrolledWindow (..),
  Window (..),
 )
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

import Config
import FullTask


newtype AppState = AppState {tasks :: Vector Text}


data Event
  = Task Text
  | Closed
  deriving (Show)


viewApp :: AppState -> AppView Window Event
viewApp AppState{..} =
  bin
    Window
    [ #title := "TaskLite"
    , on #deleteEvent (const (True, Closed))
    , #widthRequest := 400
    , #heightRequest := 300
    ]
    $ bin ScrolledWindow []
    $ container ListBox []
    $ tasks <&> \taskBody ->
      bin ListBoxRow [#activatable := False, #selectable := False] $
        widget Label [#label := taskBody]


updateApp :: AppState -> Event -> Transition AppState Event
updateApp AppState{..} (Task taskBody) =
  Transition AppState{tasks = tasks <> [taskBody]} (pure Nothing)
updateApp _ Closed = Exit


main :: IO ()
main = do
  let appName = "tasklite"

  configDirectory <- getXdgDirectory XdgConfig appName
  createDirectoryIfMissing True configDirectory

  let configPath = configDirectory </> "config.yaml"

  configUserEither <- decodeFileEither configPath

  case configUserEither of
    Left error -> die $ T.pack $ prettyPrintParseException error
    Right configUser -> do
      configUserNorm <-
        if (dataDir configUser /= "")
          then pure $ configUser
          else do
            xdgDataDir <- getXdgDirectory XdgData appName
            pure $ configUser{dataDir = xdgDataDir}

      config <- case (T.stripPrefix "~/" $ T.pack $ dataDir configUserNorm) of
        Nothing ->
          pure $ configUser{dataDir = dataDir configUserNorm}
        Just rest -> do
          homeDir <- getHomeDirectory
          pure $ configUser{dataDir = homeDir </> T.unpack rest}

      connection <- setupConnection config

      tasks <-
        query_
          connection
          [sql|
            SELECT *
            FROM tasks_view
            ORDER BY ulid ASC
            LIMIT 10
          |]

      void $
        run
          App
            { view = viewApp
            , update = updateApp
            , inputs =
                [(tasks :: [FullTask]) <&> FullTask.body <&> Task & Pipes.each]
            , initialState = AppState []
            }

      -- TODO: Use withConnection instead
      close connection
