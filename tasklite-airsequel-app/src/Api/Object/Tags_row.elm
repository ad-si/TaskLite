-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Api.Object.Tags_row exposing (..)

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
tag : SelectionSet (Maybe String) Api.Object.Tags_row
tag =
      Object.selectionForField "(Maybe String)" "tag" [] (Decode.string |> Decode.nullable)


{-| 
-}
open : SelectionSet (Maybe String) Api.Object.Tags_row
open =
      Object.selectionForField "(Maybe String)" "open" [] (Decode.string |> Decode.nullable)


{-| 
-}
closed : SelectionSet (Maybe String) Api.Object.Tags_row
closed =
      Object.selectionForField "(Maybe String)" "closed" [] (Decode.string |> Decode.nullable)


{-| 
-}
progress : SelectionSet (Maybe String) Api.Object.Tags_row
progress =
      Object.selectionForField "(Maybe String)" "progress" [] (Decode.string |> Decode.nullable)
