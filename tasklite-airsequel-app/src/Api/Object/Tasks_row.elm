-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Api.Object.Tasks_row exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.SelectionSet exposing (SelectionSet)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Api.Object
import Api.Interface
import Api.Union
import Api.Scalar
import Api.InputObject
import Api.ScalarCodecs
import Json.Decode as Decode
import Graphql.Internal.Encode as Encode exposing (Value)

{-| 
-}
rowid : SelectionSet (Maybe Int) Api.Object.Tasks_row
rowid =
      Object.selectionForField "(Maybe Int)" "rowid" [] (Decode.int |> Decode.nullable)


{-| 
-}
ulid : SelectionSet String Api.Object.Tasks_row
ulid =
      Object.selectionForField "String" "ulid" [] (Decode.string)


{-| 
-}
body : SelectionSet String Api.Object.Tasks_row
body =
      Object.selectionForField "String" "body" [] (Decode.string)


{-| 
-}
modified_utc : SelectionSet String Api.Object.Tasks_row
modified_utc =
      Object.selectionForField "String" "modified_utc" [] (Decode.string)


{-| 
-}
awake_utc : SelectionSet (Maybe String) Api.Object.Tasks_row
awake_utc =
      Object.selectionForField "(Maybe String)" "awake_utc" [] (Decode.string |> Decode.nullable)


{-| 
-}
ready_utc : SelectionSet (Maybe String) Api.Object.Tasks_row
ready_utc =
      Object.selectionForField "(Maybe String)" "ready_utc" [] (Decode.string |> Decode.nullable)


{-| 
-}
waiting_utc : SelectionSet (Maybe String) Api.Object.Tasks_row
waiting_utc =
      Object.selectionForField "(Maybe String)" "waiting_utc" [] (Decode.string |> Decode.nullable)


{-| 
-}
review_utc : SelectionSet (Maybe String) Api.Object.Tasks_row
review_utc =
      Object.selectionForField "(Maybe String)" "review_utc" [] (Decode.string |> Decode.nullable)


{-| 
-}
due_utc : SelectionSet (Maybe String) Api.Object.Tasks_row
due_utc =
      Object.selectionForField "(Maybe String)" "due_utc" [] (Decode.string |> Decode.nullable)


{-| 
-}
closed_utc : SelectionSet (Maybe String) Api.Object.Tasks_row
closed_utc =
      Object.selectionForField "(Maybe String)" "closed_utc" [] (Decode.string |> Decode.nullable)


{-| 
-}
state : SelectionSet (Maybe String) Api.Object.Tasks_row
state =
      Object.selectionForField "(Maybe String)" "state" [] (Decode.string |> Decode.nullable)


{-| 
-}
group_ulid : SelectionSet (Maybe String) Api.Object.Tasks_row
group_ulid =
      Object.selectionForField "(Maybe String)" "group_ulid" [] (Decode.string |> Decode.nullable)


{-| 
-}
repetition_duration : SelectionSet (Maybe String) Api.Object.Tasks_row
repetition_duration =
      Object.selectionForField "(Maybe String)" "repetition_duration" [] (Decode.string |> Decode.nullable)


{-| 
-}
recurrence_duration : SelectionSet (Maybe String) Api.Object.Tasks_row
recurrence_duration =
      Object.selectionForField "(Maybe String)" "recurrence_duration" [] (Decode.string |> Decode.nullable)


{-| 
-}
priority_adjustment : SelectionSet (Maybe Float) Api.Object.Tasks_row
priority_adjustment =
      Object.selectionForField "(Maybe Float)" "priority_adjustment" [] (Decode.float |> Decode.nullable)


{-| 
-}
user : SelectionSet (Maybe String) Api.Object.Tasks_row
user =
      Object.selectionForField "(Maybe String)" "user" [] (Decode.string |> Decode.nullable)


{-| 
-}
metadata : SelectionSet (Maybe String) Api.Object.Tasks_row
metadata =
      Object.selectionForField "(Maybe String)" "metadata" [] (Decode.string |> Decode.nullable)
