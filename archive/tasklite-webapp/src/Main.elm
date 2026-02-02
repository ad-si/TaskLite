module Main exposing (..)

import Browser
import Debug exposing (toString)
import Html.Styled
  exposing
    ( Html
    , div
    , h1
    , img
    , input
    , li
    , p
    , span
    , text
    , toUnstyled
    , ul
    )
import Html.Styled.Attributes
  exposing
    ( css
    , src
    , type_
    )
import Http
import Json.Decode as Decode
  exposing
    ( Decoder
    , decodeString
    , float
    , int
    , list
    , nullable
    , string
    )
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import List.Extra exposing (remove)
import Tailwind.Breakpoints
import Tailwind.Utilities as T


{-| All tasks with this tag will be displayed
and the tag will not be listed separately for each task.
-}
tagToDisplay =
  "focus"


loadTasks : Cmd Msg
loadTasks =
  Http.get
    { -- Proxied to tasklite-server
    url = "/tasks?tags=" ++ tagToDisplay
    , expect = Http.expectJson GotTasks tasksDecoder
    }


---- MODEL ----
type alias Note =
  { body : String
  , ulid : String
  }


noteDecoder : Decoder Note
noteDecoder =
  Decode.succeed Note
    |> required "body" string
    |> required "ulid" string


type alias Task =
  { awake_utc : Maybe String
  , review_utc : Maybe String
  , state : Maybe String
  , repetition_duration : Maybe String
  , priority : Float
  , recurrence_duration : Maybe String
  , body : Maybe String
  , user : Maybe String
  , ulid : Maybe String
  , modified_utc : Maybe String
  , group_ulid : Maybe String
  , closed_utc : Maybe String
  , notes : Maybe (List Note)
  , waiting_utc : Maybe String
  , ready_utc : Maybe String
  , tags : List String
  , due_utc : Maybe String
  }


zeroTask : Task
zeroTask =
  { awake_utc = Nothing
  , review_utc = Nothing
  , state = Nothing
  , repetition_duration = Nothing
  , priority = 0
  , recurrence_duration = Nothing
  , body = Just "Just a test"
  , user = Nothing
  , ulid = Nothing
  , modified_utc = Nothing
  , group_ulid = Nothing
  , closed_utc = Nothing
  , notes = Nothing
  , waiting_utc = Nothing
  , ready_utc = Nothing
  , tags = []
  , due_utc = Nothing
  }


taskDecoder : Decoder Task
taskDecoder =
  Decode.succeed Task
    |> required "awake_utc" (nullable string)
    |> required "review_utc" (nullable string)
    |> required "state" (nullable string)
    |> required "repetition_duration" (nullable string)
    |> required "priority" float
    |> required "recurrence_duration" (nullable string)
    |> required "body" (nullable string)
    |> required "user" (nullable string)
    |> required "ulid" (nullable string)
    |> required "modified_utc" (nullable string)
    |> required "group_ulid" (nullable string)
    |> required "closed_utc" (nullable string)
    |> required "notes" (nullable (list noteDecoder))
    |> required "waiting_utc" (nullable string)
    |> required "ready_utc" (nullable string)
    |> required "tags" (list string)
    |> required "due_utc" (nullable string)


tasksDecoder : Decoder (List Task)
tasksDecoder =
  list taskDecoder


type alias Model =
  { tasks : List Task
  , isLoading : Bool
  , errors : List Http.Error
  }


init : ( Model, Cmd Msg )
init =
  ( { tasks = []
    , isLoading = False
    , errors = []
    }
  , loadTasks
  )


---- UPDATE ----
type Msg
  = NoOp
  | LoadTasks
  | GotTasks (Result Http.Error (List Task))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LoadTasks ->
      ( { model | isLoading = True }, Cmd.none )
    GotTasks result ->
      case result of
        Ok tasks ->
          ( { model | isLoading = False, tasks = tasks }
          , Cmd.none
          )
        Err errMsg ->
          ( { model | isLoading = False, errors = [ errMsg ] }
          , Cmd.none
          )
    NoOp ->
      ( model, Cmd.none )


---- VIEW ----
viewTag : String -> Html Msg
viewTag tag =
  div
    [ css
        [ T.bg_blue_50
        , T.px_1
        , T.py_1
        , T.mr_1
        , T.rounded_sm
        , T.text_xs
        , T.text_gray_600
        ]
    ]
    [ text tag ]


viewTasks : List Task -> List (Html Msg)
viewTasks tasks =
  [ div
      [ css [ T.flex, T.flex_row, T.justify_center, T.p_2 ] ]
      [ ul
          [ css [ T.list_none, T.list_inside, T.max_w_full ] ]
          (List.map
              (\task -> li
                  [ css
                      [ T.bg_white
                      , T.px_4
                      , T.py_3
                      , T.mb_0_dot_5
                      , T.rounded_md
                      , T.shadow
                      , T.flex
                      , T.items_center
                      ]
                  ]
                  ([ input
                        [ css [ T.mr_3, T.w_5, T.h_5, T.flex_shrink_0 ]
                        , type_ "checkbox"
                        ]
                        []
                    , span
                        [ css
                            [ T.mr_2
                            , T.overflow_ellipsis
                            , T.overflow_hidden
                            ]
                        ]
                        [ text (Maybe.withDefault "" task.body) ]
                    ]
                    ++ (task.tags
                      |> remove tagToDisplay
                      |> List.map viewTag
                    )
                  )
              )
              tasks
          )
      ]
  ]


viewErrors : List Http.Error -> List (Html Msg)
viewErrors errors =
  [ div
      []
      [ ul
          []
          (List.map
              (\httpErr -> li
                  []
                  [ text (toString httpErr) ]
              )
              errors
          )
      ]
  ]


view : Model -> Browser.Document Msg
view model =
  { title = "TaskLite"
  , body = [ toUnstyled <|
        div
          [ css [ T.h_full ] ]
          (case model.isLoading || model.tasks == [] of
              True ->
                [ p [] [ text "Loading …" ] ]
              False ->
                case model.errors of
                  [] ->
                    viewTasks model.tasks
                  _ ->
                    viewErrors model.errors
          )
    ]
  }


---- PROGRAM ----
main : Program () Model Msg
main =
  Browser.application
    { view = view
    , init = \_ -> \_ -> \_ -> init
    , update = update
    , subscriptions = always Sub.none
    , onUrlRequest = \_ -> NoOp
    , onUrlChange = \_ -> NoOp
    }
