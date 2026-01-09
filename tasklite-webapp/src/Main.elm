module Main exposing (..)

import Api.Enum.OrderingTerm exposing (OrderingTerm(..))
import Api.InputObject
    exposing
        ( buildStringComparison
        , buildTasks_filter
        , buildTasks_insert_input
        , buildTasks_set_input
        , buildTasks_view_filter
        , buildTasks_view_order_by
        )
import Api.Mutation as Mutation
import Api.Object exposing (Tasks_ready_row, Tasks_view_row)
import Api.Object.Tasks_mutation_response
import Api.Object.Tasks_ready_row as Tasks_ready_row exposing (body)
import Api.Object.Tasks_view_row as Tasks_view_row exposing (body)
import Api.Query as Query
import Api.Scalar exposing (Id(..))
import Browser
import Browser.Navigation exposing (Key, load, pushUrl)
import Css exposing (hover, url)
import Css.Media exposing (withMediaQuery)
import Graphql.Http
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, with)
import Html.Styled
    exposing
        ( a
        , button
        , div
        , form
        , h1
        , input
        , main_
        , nav
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
        , href
        , placeholder
        , title
        , type_
        , value
        )
import Html.Styled.Events exposing (onCheck, onClick, onInput, onSubmit)
import Iso8601
import Json.Decode
import List
import Random
import RemoteData exposing (RemoteData(..))
import Tailwind.Theme exposing (..)
import Tailwind.Utilities exposing (..)
import Task
import Time exposing (Posix)
import Ulid exposing (Ulid, ulidGenerator)
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, oneOf, parse, s, string)


graphqlApiUrl : String
graphqlApiUrl =
    "http://localhost:7458/graphql"


dark : List Css.Style -> Css.Style
dark =
    withMediaQuery [ "(prefers-color-scheme: dark)" ]


type alias TodoItem =
    { ulid : Maybe String
    , body : Maybe String
    , state : Maybe String
    , closed_utc : Maybe String
    , due_utc : Maybe String
    , review_utc : Maybe String
    , tags : Maybe String
    , repetition_duration : Maybe String
    , recurrence_duration : Maybe String
    }


emptyTodo : TodoItem
emptyTodo =
    { ulid = Nothing
    , body = Nothing
    , state = Nothing
    , closed_utc = Nothing
    , due_utc = Nothing
    , review_utc = Nothing
    , tags = Nothing
    , repetition_duration = Nothing
    , recurrence_duration = Nothing
    }


type Msg
    = ReceivedTime Posix
    | ReloadTasks
    | GotTasksResponse
        (RemoteData
            (Graphql.Http.Error (List TodoItem))
            (List TodoItem)
        )
    | NewTask String
    | AddTaskNow
    | AddTaskAt Posix
    | AddTask Posix Ulid
    | InsertAffectedRowsResponse (RemoteData (Graphql.Http.Error Int) Int)
    | SetDone String
    | CompleteAffectedRowsResponse (RemoteData (Graphql.Http.Error Int) Int)
    | DeleteTask String
    | DeleteAffectedRowsResponse (RemoteData (Graphql.Http.Error Int) Int)
    | UrlChanged Url
    | ClickedLink Browser.UrlRequest
    | NoOp


type alias Model =
    { key : Key
    , remoteTodos :
        RemoteData
            (Graphql.Http.Error (List TodoItem))
            (List TodoItem)
    , newTask : String
    , submissionStatus : RemoteData (Graphql.Http.Error Int) Int
    , now : Posix
    , tags : List String
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


viewTagStr : String -> Html.Styled.Html Msg
viewTagStr tagsStr =
    let
        tagList =
            tagsStr
                |> Json.Decode.decodeString (Json.Decode.list Json.Decode.string)
                |> Result.withDefault []
    in
    span []
        (tagList
            |> List.map
                (\tag ->
                    a
                        [ css
                            [ text_color blue_400
                            , mr_2
                            ]
                        , href <| "/tags/" ++ tag
                        ]
                        [ text <| "+" ++ tag ]
                )
        )


viewTodo : Posix -> TodoItem -> Html.Styled.Html Msg
viewTodo now todo =
    let
        ifDisabledElse disabledValue elseValue =
            case todo.ulid of
                Nothing ->
                    disabledValue

                Just ulid ->
                    case
                        ( todo.repetition_duration
                        , todo.recurrence_duration
                        )
                    of
                        ( Nothing, Nothing ) ->
                            elseValue ulid

                        _ ->
                            disabledValue
    in
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
            , dark [ bg_color neutral_800, text_color neutral_300 ]
            ]
        ]
        [ div
            [ css [ flex_1, items_center ] ]
            [ input
                [ type_ "checkbox"
                , css
                    [ mr_2
                    , relative
                    , Css.top (Css.px 1.5)
                    , ifDisabledElse
                        (Css.batch [ opacity_50, cursor_not_allowed ])
                        (\_ -> Css.batch [ opacity_60, cursor_pointer ])
                    ]
                , ifDisabledElse
                    (title <|
                        "Currently not supported.\n"
                            ++ "Please use the CLI to complete this task."
                    )
                    (\_ -> css [])
                , checked
                    (case todo.closed_utc of
                        Just _ ->
                            True

                        _ ->
                            False
                    )
                , ifDisabledElse
                    (disabled True)
                    (\ulid ->
                        onCheck
                            (\bool ->
                                if bool then
                                    SetDone ulid

                                else
                                    -- TODO: Implement
                                    NoOp
                            )
                    )
                ]
                []
            , viewMaybe todo.review_utc
                (\review_utc ->
                    if review_utc < Iso8601.fromTime now then
                        span
                            [ css [ text_color green_500, text_sm, mr_4 ]
                            , title "Must be reviewed"
                            ]
                            [ text "🔎" ]

                    else
                        text ""
                )
            , span
                [ css
                    [ inline_block
                    , mr_4
                    , case todo.due_utc of
                        Just due_utc ->
                            if due_utc < Iso8601.fromTime now then
                                Css.batch
                                    [ text_color red_600
                                    , dark [ text_color red_400 ]
                                    ]

                            else
                                text_color inherit

                        Nothing ->
                            text_color inherit
                    ]
                ]
                [ text (todo.body |> Maybe.withDefault "") ]
            , viewMaybe todo.due_utc
                (\due_utc ->
                    span
                        [ css
                            [ text_color yellow_500
                            , text_sm
                            , mr_4
                            , dark [ text_color yellow_200 ]
                            ]
                        ]
                        [ text due_utc ]
                )
            , viewMaybe todo.tags viewTagStr
            ]
        , div
            [ css
                [ text_xs
                , text_color gray_300
                , font_mono
                , mr_1
                , dark [ text_color neutral_600 ]
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
                , text_color gray_300
                , px_1_dot_5
                , hover [ text_color gray_500, border_color gray_500 ]
                , dark
                    [ bg_color neutral_800
                    , border_color neutral_600
                    , text_color neutral_600
                    , hover
                        [ bg_color neutral_600
                        , border_color neutral_400
                        , text_color neutral_400
                        ]
                    ]
                ]
            , case todo.ulid of
                Nothing ->
                    disabled True

                Just ulid ->
                    onClick (DeleteTask ulid)
            ]
            [ text "✕" ]
        ]


viewBody : Model -> Html.Styled.Html Msg
viewBody model =
    div
        [ css
            [ min_h_full
            , font_sans
            , dark [ bg_color neutral_900, text_color neutral_300 ]
            ]
        ]
        [ main_
            [ css [ max_w_3xl, mx_auto, px_4, py_8 ] ]
            [ nav [ css [ flex, items_baseline ] ]
                [ h1
                    [ css [ mb_4, inline_block, mr_4 ] ]
                    [ a
                        [ href "/"
                        , css
                            [ no_underline
                            , text_color inherit
                            , dark [ text_color neutral_300 ]
                            ]
                        ]
                        [ text "TaskLite" ]
                    ]
                , div [ css [ text_color gray_500 ] ]
                    (model.tags
                        |> List.map
                            (\tag ->
                                span [ css [ mr_2 ] ] [ text <| "+" ++ tag ]
                            )
                    )
                , div
                    [ css [ flex, flex_1, justify_end ] ]
                    [ button
                        [ css
                            [ mb_4
                            , border_none
                            , text_2xl
                            , cursor_pointer
                            , bg_color transparent
                            , dark [ opacity_60, hover [ opacity_100 ] ]
                            ]
                        , onClick ReloadTasks
                        ]
                        [ text "🔁" ]
                    ]
                ]
            , let
                inputForm =
                    form [ onSubmit AddTaskNow, css [ flex, mb_3 ] ]
                        [ input
                            [ type_ "text"
                            , placeholder "Add a new task …"
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
                                , placeholder_color gray_400
                                , dark
                                    [ bg_color neutral_800
                                    , border_color neutral_500
                                    , text_color neutral_500
                                    ]
                                ]
                            ]
                            []
                        , input
                            [ type_ "submit"
                            , css
                                [ cursor_pointer
                                , px_3
                                , py_2
                                , rounded
                                , border
                                , border_solid
                                , border_color gray_400
                                , hover
                                    [ bg_color gray_200
                                    , border_color gray_600
                                    ]
                                , dark
                                    [ bg_color neutral_800
                                    , border_color neutral_500
                                    , text_color neutral_500
                                    , hover
                                        [ bg_color neutral_600
                                        , border_color neutral_300
                                        , text_color neutral_300
                                        ]
                                    ]
                                ]
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
                    div [] (todos |> List.map (viewTodo model.now))

                Failure error ->
                    viewError error
            ]
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Todo App"
    , body = [ toUnstyled <| viewBody model ]
    }


tasksHeadSelection : SelectionSet TodoItem Tasks_ready_row
tasksHeadSelection =
    SelectionSet.succeed TodoItem
        |> with Tasks_ready_row.ulid
        |> with Tasks_ready_row.body
        |> with Tasks_ready_row.state
        |> with Tasks_ready_row.closed_utc
        |> with Tasks_ready_row.due_utc
        |> with Tasks_ready_row.review_utc
        |> with Tasks_ready_row.tags
        |> with Tasks_ready_row.repetition_duration
        |> with Tasks_ready_row.recurrence_duration


tasksViewSelection : SelectionSet TodoItem Tasks_view_row
tasksViewSelection =
    SelectionSet.succeed TodoItem
        |> with Tasks_view_row.ulid
        |> with Tasks_view_row.body
        |> with Tasks_view_row.state
        |> with Tasks_view_row.closed_utc
        |> with Tasks_view_row.due_utc
        |> with Tasks_view_row.review_utc
        |> with Tasks_view_row.tags
        |> with Tasks_view_row.repetition_duration
        |> with Tasks_view_row.recurrence_duration


getTodos : Cmd Msg
getTodos =
    Query.tasks_ready (\opts -> { opts | limit = Present 100 }) tasksHeadSelection
        |> Graphql.Http.queryRequest graphqlApiUrl
        |> Graphql.Http.send (RemoteData.fromResult >> GotTasksResponse)


{-| Refresh todos based on current view
-}
refreshTodos : Model -> Cmd Msg
refreshTodos model =
    case model.tags of
        [ tag ] ->
            getTodosWithTag tag

        _ ->
            getTodos


getTodosWithTag : String -> Cmd Msg
getTodosWithTag tag =
    if String.contains "," tag then
        -- TODO: Add support for specifying multiple tags
        Cmd.none

    else
        let
            setTags filter =
                { filter
                    | tags =
                        Present <|
                            buildStringComparison
                                (\c ->
                                    { c
                                        | like =
                                            Present <| "%" ++ tag ++ "%"
                                    }
                                )
                    , closed_utc =
                        Present <|
                            buildStringComparison (\c -> { c | eq = Null })
                }
        in
        Query.tasks_view
            (\opts ->
                { opts
                    | filter = Present <| buildTasks_view_filter setTags
                    , order_by =
                        Present
                            [ Just <|
                                buildTasks_view_order_by
                                    (\o -> { o | priority = Present Desc })
                            ]
                    , limit = Present 100
                }
            )
            tasksViewSelection
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
        identity
        { objects =
            [ buildTasks_insert_input
                { ulid = ulidString
                , body = body
                , modified_utc = now |> Iso8601.fromTime
                }
                (\opts -> { opts | user = Present "webapp" })
            ]
        }
        Api.Object.Tasks_mutation_response.affected_rows
        |> Graphql.Http.mutationRequest graphqlApiUrl
        |> Graphql.Http.send
            (RemoteData.fromResult >> InsertAffectedRowsResponse)


{-| Set task as done
TODO: Support completing repetition and recurrence tasks
-}
setTodoCompleted : String -> Cmd Msg
setTodoCompleted ulid =
    Mutation.update_tasks
        { filter =
            buildTasks_filter
                (\f ->
                    { f
                        | ulid =
                            Present <|
                                buildStringComparison
                                    (\c -> { c | eq = Present ulid })

                        -- TODO: Add when Airsequel supports
                        --       several filters simultaneously
                        -- recurrence_duration =
                        --     Present <|
                        --         buildStringComparison
                        --             (\c -> { c | eq = Present "0" })
                        -- repetition_duration =
                        --     Present <|
                        --         buildStringComparison
                        --             (\c -> { c | eq = Present "0" })
                    }
                )
        , set =
            { state = Present "Done" -- SQL trigger will update `closed_utc`

            -- TODO: Figure out why setting `modified_utc`
            --       prevents recursive trigger calls
            , modified_utc = Present ""

            --
            , closed_utc = Absent
            , awake_utc = Absent
            , body = Absent
            , due_utc = Absent
            , group_ulid = Absent
            , metadata = Absent
            , priority_adjustment = Absent
            , ready_utc = Absent
            , recurrence_duration = Absent
            , repetition_duration = Absent
            , review_utc = Absent
            , rowid = Absent
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
    -- TODO: Also delete tags and notes
    Mutation.delete_tasks
        { filter =
            { ulid =
                Present <|
                    buildStringComparison
                        (\c -> { c | eq = Present ulid })
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


type Route
    = Home
    | Tags String
    | New
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Url.Parser.top |> Url.Parser.map Home
        , (s "tags" </> string) |> Url.Parser.map Tags
        , s "new" |> Url.Parser.map New
        ]


handleRoute : Route -> Cmd Msg
handleRoute route =
    case route of
        Home ->
            Cmd.batch
                [ getTodos
                , Task.perform ReceivedTime Time.now
                ]

        Tags tagStr ->
            Cmd.batch
                [ getTodosWithTag tagStr
                , Task.perform ReceivedTime Time.now
                ]

        New ->
            Cmd.none

        NotFound ->
            Cmd.none


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    let
        route =
            url |> parse routeParser |> Maybe.withDefault NotFound
    in
    ( { key = key
      , remoteTodos = RemoteData.Loading
      , newTask = ""
      , submissionStatus = RemoteData.NotAsked
      , now = Time.millisToPosix 0
      , tags =
            case route of
                Tags tagStr ->
                    String.split "," tagStr

                _ ->
                    []
      }
    , handleRoute route
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedTime time ->
            ( { model | now = time }, Cmd.none )

        ReloadTasks ->
            ( { model
                | remoteTodos = RemoteData.Loading
                , submissionStatus = RemoteData.NotAsked
              }
            , refreshTodos model
            )

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
                                    ++ [ { emptyTodo
                                            | body = Just "Loading …"
                                         }
                                       ]
                            )
                , submissionStatus = response
              }
            , refreshTodos model
            )

        SetDone ulid ->
            ( { model
                | remoteTodos =
                    model.remoteTodos
                        |> RemoteData.map
                            (List.map
                                (\todo ->
                                    if todo.ulid == Just ulid then
                                        { todo | state = Just "Done" }

                                    else
                                        todo
                                )
                            )
              }
            , setTodoCompleted ulid
            )

        CompleteAffectedRowsResponse response ->
            ( { model | submissionStatus = response }
            , refreshTodos model
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
            , refreshTodos model
            )

        UrlChanged url ->
            let
                route =
                    url
                        |> parse routeParser
                        |> Maybe.withDefault NotFound
            in
            ( { model
                | remoteTodos = RemoteData.Loading
                , tags =
                    case route of
                        Tags tagStr ->
                            String.split "," tagStr

                        _ ->
                            []
              }
            , handleRoute route
            )

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, pushUrl model.key (Url.toString url) )

                Browser.External url ->
                    ( model, load url )

        NoOp ->
            ( model, Cmd.none )


main : Platform.Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChanged
        }
