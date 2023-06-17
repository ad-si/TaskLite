-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Api.Scalar exposing (Codecs, Id(..), Upload(..), defaultCodecs, defineCodecs, unwrapCodecs, unwrapEncoder)


import Graphql.Internal.Builder.Object as Object
import Json.Decode as Decode exposing (Decoder)
import Graphql.Internal.Encode
import Json.Encode as Encode
import Graphql.Codec exposing (Codec)


type Id
    = Id String


type Upload
    = Upload String

defineCodecs :
    {codecId : Codec valueId
 , codecUpload : Codec valueUpload}
    -> Codecs valueId valueUpload
defineCodecs definitions =
    Codecs definitions


unwrapCodecs :
    Codecs valueId valueUpload
    -> {codecId : Codec valueId
 , codecUpload : Codec valueUpload}
unwrapCodecs (Codecs unwrappedCodecs) =
    unwrappedCodecs


unwrapEncoder :
    (RawCodecs valueId valueUpload -> Codec getterValue)
    -> Codecs valueId valueUpload
    -> getterValue
    -> Graphql.Internal.Encode.Value
unwrapEncoder getter (Codecs unwrappedCodecs) =
    (unwrappedCodecs |> getter |> .encoder) >> Graphql.Internal.Encode.fromJson


type Codecs valueId valueUpload
    = Codecs (RawCodecs valueId valueUpload)


type alias RawCodecs valueId valueUpload =
    {codecId : Codec valueId
 , codecUpload : Codec valueUpload}


defaultCodecs : RawCodecs Id Upload
defaultCodecs =
    {codecId =
  { encoder = \(Id raw) -> Encode.string raw
 , decoder = Object.scalarDecoder |> Decode.map Id }
 , codecUpload =
  { encoder = \(Upload raw) -> Encode.string raw
 , decoder = Object.scalarDecoder |> Decode.map Upload }}
