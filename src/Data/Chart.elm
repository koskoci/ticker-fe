module Data.Chart exposing (Chart, decoder, worth)

import Data.Line as Line exposing (Line)
import Json.Decode as Decode exposing (Decoder)


type alias Chart =
    Maybe (List Line)


worth : Chart -> Maybe Float
worth chart =
    let
        worthOf =
            \lineList -> List.map Line.worth lineList |> List.sum
    in
    Maybe.map worthOf chart


decoder : Decoder Chart
decoder =
    Decode.at [ "history" ] (Decode.map Just (Decode.list Line.decoder))
