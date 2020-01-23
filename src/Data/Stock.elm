module Data.Stock exposing (Stock, build, encoder)

import Data.Allocation exposing (Allocation)
import Date exposing (Date)
import Json.Encode as Encode


type alias Stock =
    { ticker : String
    , startDate : Date
    , initialValue : Float
    }


encoder : Stock -> Encode.Value
encoder stock =
    Encode.object
        [ ( "ticker", Encode.string stock.ticker )
        , ( "startDate", Encode.string (Date.toIsoString stock.startDate) )
        , ( "initialValue", Encode.float stock.initialValue )
        ]


build : Date -> Int -> Allocation -> Stock
build startDate initialBalance allocation =
    { ticker = allocation.ticker
    , startDate = startDate
    , initialValue = allocation.percentage / 100 * toFloat initialBalance
    }
