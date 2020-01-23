module Data.Allocation exposing (Allocation, current)


type alias Allocation =
    { percentage : Float
    , ticker : String
    }


current : Maybe Float -> Maybe String -> Maybe Allocation
current currentPercentage currentTicker =
    case ( currentPercentage, currentTicker ) of
        ( Just percentage, Just ticker ) ->
            if percentage == 0 then
                Nothing

            else
                Just (Allocation percentage ticker)

        _ ->
            Nothing
