module Data.Chart exposing (Chart(..), decoder, worth)

import Data.Line as Line exposing (Line)
import Json.Decode as Decode exposing (Decoder)


type Chart
    = Chart (List Line)


worth : Chart -> Float
worth (Chart lineList) =
    List.map Line.worth lineList |> List.sum


decoder : Decoder Chart
decoder =
    Decode.at [ "history" ] (Decode.map Chart (Decode.list Line.decoder))
