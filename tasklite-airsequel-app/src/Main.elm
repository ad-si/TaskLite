module Main exposing (..)

import Api.Mutation as Mutation
import Api.Object exposing (Tasks_head_row)
import Api.Object.Tasks_head_row as Tasks_head_row exposing (body)
import Api.Object.Tasks_mutation_response
import Api.Query as Query
import Api.Scalar exposing (Id(..))
import Browser
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html.Styled
    exposing
        ( button
        , div
        , form
        , h1
        , input
        , main_
        , p
        , span
        , text
        , toUnstyled
        )
import Html.Styled.Attributes
    exposing
        ( checked
        , css
        , disabled
        , type_
        , value
        )
import Html.Styled.Events exposing (onCheck, onClick, onInput, onSubmit)
import Iso8601
import Json.Decode
import List exposing (map)
import Random
import RemoteData exposing (RemoteData(..))
import Tailwind.Theme exposing (..)
import Tailwind.Utilities exposing (..)
import Task
import Time exposing (Posix)
import Ulid exposing (Ulid, ulidGenerator)


dbId : String
dbId =
    "tasklite"


graphqlApiUrl : String
graphqlApiUrl =
    "http://localhost:4185/dbs/" ++ dbId ++ "/graphql"


type alias TodoItem =
    { ulid : Maybe String
    , body : Maybe String
    , closed_utc : Maybe String
    , due_utc : Maybe String
    , tags : Maybe String
    }


type Msg
    = GotTasksResponse
        (RemoteData
            (Graphql.Http.Error (List TodoItem))
            (List TodoItem)
        )
    | NewTask String
    | AddTaskNow
    | AddTaskAt Posix
    | AddTask Posix Ulid
    | InsertAffectedRowsResponse (RemoteData (Graphql.Http.Error Int) Int)
    | SetCompleted String Bool
    | CompleteAffectedRowsResponse (RemoteData (Graphql.Http.Error Int) Int)
    | DeleteTask String
    | DeleteAffectedRowsResponse (RemoteData (Graphql.Http.Error Int) Int)


type alias Model =
    { remoteTodos :
        RemoteData
            (Graphql.Http.Error (List TodoItem))
            (List TodoItem)
    , newTask : String
    , submissionStatus : RemoteData (Graphql.Http.Error Int) Int
    }


type alias Flags =
    ()


viewError : Graphql.Http.Error a -> Html.Styled.Html Msg
viewError error =
    case error of
        Graphql.Http.GraphqlError _ graphqlErrors ->
            div []
                (graphqlErrors
                    |> List.map (\gqlError -> p [] [ text gqlError.message ])
                )

        Graphql.Http.HttpError httpError ->
            case httpError of
                Graphql.Http.BadUrl url ->
                    p [] [ text <| "Bad URL: " ++ url ]

                Graphql.Http.BadStatus response body ->
                    p []
                        [ text <|
                            "Bad status (code "
                                ++ String.fromInt response.statusCode
                                ++ "): "
                                ++ body
                        ]

                Graphql.Http.NetworkError ->
                    p [] [ text <| "Network error" ]

                Graphql.Http.Timeout ->
                    p [] [ text "Timeout" ]

                Graphql.Http.BadPayload response ->
                    p []
                        [ text <|
                            "Bad payload: "
                                ++ Json.Decode.errorToString response
                        ]


viewMaybe :
    Maybe val
    -> (val -> Html.Styled.Html Msg)
    -> Html.Styled.Html Msg
viewMaybe maybeValue viewFunc =
    case maybeValue of
        Nothing ->
            text ""

        Just value ->
            viewFunc value


viewTodo : TodoItem -> Html.Styled.Html Msg
viewTodo todo =
    div
        [ css
            [ bg_color white
            , px_3
            , py_2
            , mb_1
            , rounded_md
            , shadow
            , flex
            , items_center
            ]
        ]
        [ div
            [ css [ flex_1, items_center ] ]
            [ input
                [ type_ "checkbox"
                , checked
                    (case todo.closed_utc of
                        Just _ ->
                            True

                        _ ->
                            False
                    )
                , case todo.ulid of
                    Nothing ->
                        disabled True

                    Just ulid ->
                        onCheck (SetCompleted ulid)
                ]
                []
            , span
                [ css [ inline_block, mr_4 ] ]
                [ text (todo.body |> Maybe.withDefault "") ]
            , viewMaybe todo.due_utc
                (\due_utc ->
                    span
                        [ css [ text_color yellow_500, text_sm, mr_4 ] ]
                        [ text due_utc ]
                )
            , viewMaybe todo.tags
                (\tagsStr ->
                    span []
                        (tagsStr
                            |> String.split ","
                            |> List.map
                                (\tag ->
                                    span
                                        [ css [ text_color blue_400, mr_2 ] ]
                                        [ text <| "+" ++ tag ]
                                )
                        )
                )
            ]
        , div
            [ css
                [ text_xs
                , text_color gray_300
                , font_mono
                , mr_1
                ]
            ]
            [ text
                (todo.ulid
                    |> Maybe.withDefault ""
                    |> String.right 4
                )
            ]
        , button
            [ css
                [ cursor_pointer
                , rounded_full
                , border
                , border_solid
                , bg_color gray_100
                , border_color gray_300
                , text_color gray_400
                ]
            , case todo.ulid of
                Nothing ->
                    disabled True

                Just ulid ->
                    onClick (DeleteTask ulid)
            ]
            [ text "x" ]
        ]


viewBody : Model -> Html.Styled.Html Msg
viewBody model =
    div
        [ css [ h_full, bg_color gray_100, font_sans ] ]
        [ main_ [ css [ max_w_3xl, mx_auto, px_4, py_8 ] ]
            [ h1 [] [ text "TaskLite" ]
            , let
                inputForm =
                    form [ onSubmit AddTaskNow, css [ flex, mb_3 ] ]
                        [ input
                            [ type_ "text"
                            , onInput NewTask
                            , value model.newTask
                            , css
                                [ flex_1
                                , mr_2
                                , px_3
                                , py_2
                                , rounded
                                , border
                                , border_solid
                                , border_color gray_400
                                ]
                            ]
                            []
                        , input
                            [ type_ "submit"
                            , if model.newTask == "" then
                                disabled True

                              else
                                disabled False
                            ]
                            []
                        ]
              in
              case model.submissionStatus of
                NotAsked ->
                    inputForm

                Loading ->
                    p [] [ text "Submitting …" ]

                Failure error ->
                    viewError error

                Success _ ->
                    inputForm
            , case model.remoteTodos of
                NotAsked ->
                    p [] [ text "Initializing …" ]

                Loading ->
                    p [] [ text "Loading …" ]

                Success todos ->
                    div []
                        (todos
                            -- TODO: Remove after order is fixed in Airsequel
                            |> List.reverse
                            |> map viewTodo
                        )

                Failure error ->
                    viewError error
            ]
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Todo App"
    , body = [ toUnstyled <| viewBody model ]
    }


todosSelection : SelectionSet TodoItem Tasks_head_row
todosSelection =
    SelectionSet.map5 TodoItem
        Tasks_head_row.ulid
        Tasks_head_row.body
        Tasks_head_row.closed_utc
        Tasks_head_row.due_utc
        Tasks_head_row.tags


getTodos : Cmd Msg
getTodos =
    Query.tasks_head identity todosSelection
        |> Graphql.Http.queryRequest graphqlApiUrl
        |> Graphql.Http.send (RemoteData.fromResult >> GotTasksResponse)


insertTodo : Posix -> Ulid -> String -> Cmd Msg
insertTodo now ulid body =
    let
        ulidString =
            ulid
                |> Ulid.toString
                |> String.toLower
    in
    Mutation.insert_tasks
        { objects =
            [ { ulid = ulidString
              , body = body

              -- TODO: Replace after it's set automatically
              , modified_utc = now |> Iso8601.fromTime
              , user = Present "webapp"

              --
              , awake_utc = Absent
              , closed_utc = Absent
              , due_utc = Absent
              , group_ulid = Absent
              , metadata = Absent
              , priority_adjustment = Absent
              , ready_utc = Absent
              , recurrence_duration = Absent
              , repetition_duration = Absent
              , review_utc = Absent
              , rowid = Absent
              , state = Absent
              , waiting_utc = Absent
              }
            ]
        }
        Api.Object.Tasks_mutation_response.affected_rows
        |> Graphql.Http.mutationRequest graphqlApiUrl
        |> Graphql.Http.send
            (RemoteData.fromResult >> InsertAffectedRowsResponse)


setTodoCompleted : String -> Bool -> Cmd Msg
setTodoCompleted ulid _ =
    Mutation.update_tasks
        { filter =
            { ulid =
                Present
                    { eq = Present ulid
                    , gt = Absent
                    , gte = Absent
                    , lt = Absent
                    , lte = Absent
                    , neq = Absent
                    }
            , body = Absent
            , closed_utc = Absent
            , awake_utc = Absent
            , due_utc = Absent
            , group_ulid = Absent
            , metadata = Absent
            , modified_utc = Absent
            , priority_adjustment = Absent
            , ready_utc = Absent
            , recurrence_duration = Absent
            , repetition_duration = Absent
            , review_utc = Absent
            , rowid = Absent
            , state = Absent
            , user = Absent
            , waiting_utc = Absent
            }
        , set =
            { closed_utc = Present "TODO"

            --
            , awake_utc = Absent
            , body = Absent
            , due_utc = Absent
            , group_ulid = Absent
            , metadata = Absent
            , modified_utc = Absent
            , priority_adjustment = Absent
            , ready_utc = Absent
            , recurrence_duration = Absent
            , repetition_duration = Absent
            , review_utc = Absent
            , rowid = Absent
            , state = Absent
            , ulid = Absent
            , user = Absent
            , waiting_utc = Absent
            }
        }
        Api.Object.Tasks_mutation_response.affected_rows
        |> Graphql.Http.mutationRequest graphqlApiUrl
        |> Graphql.Http.send
            (RemoteData.fromResult >> CompleteAffectedRowsResponse)


deleteTodo : String -> Cmd Msg
deleteTodo ulid =
    Mutation.delete_tasks
        { filter =
            { ulid =
                Present
                    { eq = Present ulid
                    , gt = Absent
                    , gte = Absent
                    , lt = Absent
                    , lte = Absent
                    , neq = Absent
                    }
            , body = Absent
            , closed_utc = Absent

            --
            , awake_utc = Absent
            , due_utc = Absent
            , group_ulid = Absent
            , metadata = Absent
            , modified_utc = Absent
            , priority_adjustment = Absent
            , ready_utc = Absent
            , recurrence_duration = Absent
            , repetition_duration = Absent
            , review_utc = Absent
            , rowid = Absent
            , state = Absent
            , user = Absent
            , waiting_utc = Absent
            }
        }
        Api.Object.Tasks_mutation_response.affected_rows
        |> Graphql.Http.mutationRequest graphqlApiUrl
        |> Graphql.Http.send
            (RemoteData.fromResult >> DeleteAffectedRowsResponse)


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { remoteTodos = RemoteData.Loading
      , newTask = ""
      , submissionStatus = RemoteData.NotAsked
      }
    , getTodos
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTasksResponse response ->
            ( { model | remoteTodos = response }, Cmd.none )

        AddTaskNow ->
            ( { model | submissionStatus = RemoteData.Loading }
            , Task.perform AddTaskAt Time.now
            )

        AddTaskAt time ->
            ( { model | submissionStatus = RemoteData.Loading }
            , Random.generate (AddTask time) (ulidGenerator time)
            )

        AddTask time ulid ->
            ( { model | submissionStatus = RemoteData.Loading }
            , if model.newTask /= "" then
                insertTodo time ulid model.newTask

              else
                Cmd.none
            )

        NewTask body ->
            ( { model | newTask = body }, Cmd.none )

        InsertAffectedRowsResponse response ->
            ( { model
                | remoteTodos =
                    model.remoteTodos
                        |> RemoteData.map
                            (\todos ->
                                todos
                                    ++ [ { ulid = Nothing
                                         , body = Just "Loading …"
                                         , closed_utc = Nothing
                                         , due_utc = Nothing
                                         , tags = Nothing
                                         }
                                       ]
                            )
                , submissionStatus = response
              }
            , getTodos
            )

        SetCompleted ulid value ->
            ( { model
                | remoteTodos =
                    model.remoteTodos
                        |> RemoteData.map
                            (\todos ->
                                List.map
                                    (\todo ->
                                        if todo.ulid == Just ulid then
                                            { todo | closed_utc = Just "TODO" }

                                        else
                                            todo
                                    )
                                    todos
                            )
              }
            , setTodoCompleted ulid value
            )

        CompleteAffectedRowsResponse response ->
            ( { model | submissionStatus = response }
            , getTodos
            )

        DeleteTask ulid ->
            ( { model
                | remoteTodos =
                    RemoteData.map
                        (\todos ->
                            List.filter
                                (\todo -> todo.ulid /= Just ulid)
                                todos
                        )
                        model.remoteTodos
              }
            , deleteTodo ulid
            )

        DeleteAffectedRowsResponse response ->
            ( { model | submissionStatus = response }
            , getTodos
            )


main : Platform.Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
