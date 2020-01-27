module Helpers.LineChart exposing (chartConfig, colorOptions)

import Color
import Data.Allocation exposing (Allocation)
import Data.Datum exposing (Datum)
import LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import List.Extra exposing (cycle)
import Time


colorOptions : List Allocation -> List Color.Color
colorOptions portfolio =
    let
        options =
            [ Colors.pink, Colors.cyan, Colors.blue, Colors.teal, Colors.gold, Colors.purple, Colors.red, Colors.rust, Colors.green, Colors.strongBlue ]
    in
    cycle (List.length portfolio) options


chartConfig : LineChart.Config Datum msg
chartConfig =
    { y = Axis.default 450 "$" .value
    , x = Axis.time Time.utc 800 "time" (toFloat << Time.posixToMillis << .time)
    , container = containerConfig
    , interpolation = Interpolation.monotone
    , intersection = Intersection.default
    , legends = Legends.grouped .max .max -30 0
    , events = Events.default
    , junk = Junk.default
    , grid = Grid.dots 1 Colors.gray
    , area = Area.stacked 0.5
    , line = Line.default
    , dots = Dots.custom (Dots.empty 5 1)
    }


containerConfig : Container.Config msg
containerConfig =
    Container.custom
        { attributesHtml = []
        , attributesSvg = []
        , size = Container.relative
        , margin = Container.Margin 30 100 30 70
        , id = "line-chart-area"
        }
