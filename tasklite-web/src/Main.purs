module Main where

import Prelude hiding (div)

import Effect (Effect)
import Pux (EffModel, start)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, div, span)
import Text.Smolder.Markup (text, (#!))


data Event
  = RequestTasks
  | ReceiveTasks (Either String Tasks)

newtype Task = Task
  { ulid :: String
  , title :: String
  }

type Tasks = Array Task

type State =
  { tasks :: Tasks
  , status :: String
  }


initialState :: State
initialState =
  { tasks: []
  , status: "Nothing loaded from server yet"
  }


-- | Decode the JSON containing the tasks received from the server
instance decodeJsonTask :: DecodeJson Task where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .? "id"
    title <- obj .? "title"
    pure $ Task { id: id, title: title }


-- | Return markup from the state
view :: State -> HTML Event
view count =
  div do
    button #! onClick (const Increment) $ text "Increment"
    span $ text (show count)
    button #! onClick (const Decrement) $ text "Decrement"


-- | Start and render the app
main :: Effect Unit
main = do
  app <- start
    { initialState: 0
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
