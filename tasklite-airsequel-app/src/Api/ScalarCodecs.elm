-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql
module Api.ScalarCodecs exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Api.Scalar exposing (defaultCodecs)


type alias Id
    = Api.Scalar.Id


type alias Upload
    = Api.Scalar.Upload


codecs : Api.Scalar.Codecs Id Upload
codecs =
    Api.Scalar.defineCodecs
        {
        codecId = defaultCodecs.codecId    , codecUpload = defaultCodecs.codecUpload
        }
