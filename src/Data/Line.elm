module Data.Line exposing (Line, decoder, worth)

import Data.Datum as Datum exposing (Datum)
import Json.Decode as Decode exposing (Decoder)
import List.Extra exposing (last)


type alias Line =
    { ticker : String
    , data : List Datum
    }


worth : Line -> Float
worth lineData =
    case List.Extra.last lineData.data of
        Just datum ->
            datum.value

        Nothing ->
            0


decoder : Decoder Line
decoder =
    Decode.map2 Line
        (Decode.field "ticker" Decode.string)
        (Decode.field "data" (Decode.list Datum.decoder))
