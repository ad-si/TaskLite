-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Api.Object.Tags_mutation_response exposing (..)

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

returning : SelectionSet decodesTo Api.Object.Tags_row
 -> SelectionSet (List decodesTo) Api.Object.Tags_mutation_response
returning object____ =
      Object.selectionForCompositeField "returning" [] (object____) (Basics.identity >> Decode.list)


affected_rows : SelectionSet Int Api.Object.Tags_mutation_response
affected_rows =
      Object.selectionForField "Int" "affected_rows" [] (Decode.int)
