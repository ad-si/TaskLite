-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Api.Object.Task_to_tag_row exposing (..)

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
rowid : SelectionSet (Maybe Int) Api.Object.Task_to_tag_row
rowid =
      Object.selectionForField "(Maybe Int)" "rowid" [] (Decode.int |> Decode.nullable)


{-| 
-}
ulid : SelectionSet String Api.Object.Task_to_tag_row
ulid =
      Object.selectionForField "String" "ulid" [] (Decode.string)


{-| 
-}
task_ulid : SelectionSet String Api.Object.Task_to_tag_row
task_ulid =
      Object.selectionForField "String" "task_ulid" [] (Decode.string)


{-| 
-}
tag : SelectionSet String Api.Object.Task_to_tag_row
tag =
      Object.selectionForField "String" "tag" [] (Decode.string)
