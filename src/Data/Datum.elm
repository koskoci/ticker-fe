module Data.Datum exposing (Datum, decoder)

import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Time


type alias Datum =
    { time : Time.Posix
    , value : Float
    }


fromTuple : ( Float, String ) -> Datum
fromTuple ( amount, isoString ) =
    case Iso8601.toTime isoString of
        Ok time ->
            Datum time amount

        _ ->
            Datum (Time.millisToPosix 0) 0


decoder : Decoder Datum
decoder =
    let
        tupleDecoder : Decoder ( Float, String )
        tupleDecoder =
            Decode.map2
                Tuple.pair
                (Decode.index 0 Decode.float)
                (Decode.index 1 Decode.string)
    in
    Decode.map fromTuple tupleDecoder
