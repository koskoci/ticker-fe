module Main exposing (main)

import Browser
import Date exposing (Date)
import DatePicker exposing (DatePicker, defaultSettings)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Iso8601
import Json.Decode exposing (Decoder, decodeString, field, int, list, string)
import Json.Encode exposing (encode, int, object, string)
import LineChart as LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk exposing (..)
import LineChart.Legends as Legends
import LineChart.Line as Line
import Task
import Time


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { backendState : BackendState
    , initialBalance : Maybe Int
    , startDate : Maybe Date
    , datePicker : DatePicker
    , portfolio : List Allocation
    , currentPercentage : Maybe Float
    , currentTicker : Maybe String
    , chartData : ChartData
    }


type alias Allocation =
    { percentage : Float
    , ticker : String
    }


type BackendState
    = Failure
    | Loading
    | Success (List Int)


type alias ChartData =
    { nora : List Datum
    , noah : List Datum
    , nina : List Datum
    }


type alias Datum =
    { time : Time.Posix
    , value : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( datePicker, datePickerCmd ) =
            DatePicker.init

        initialModel =
            { backendState = Loading
            , initialBalance = Nothing
            , startDate = Nothing
            , datePicker = datePicker
            , portfolio = []
            , currentPercentage = Nothing
            , currentTicker = Nothing
            , chartData = ChartData [] [] []
            }

        commands =
            Cmd.batch
                [ pingBackend
                , Cmd.map SetDatePicker datePickerCmd
                , generateData
                ]
    in
    ( initialModel, commands )


generateData : Cmd Msg
generateData =
    let
        msg =
            ReceiveData
                [ [ ( "2012-01-12", 100.1 )
                  , ( "2012-01-13", 150.1 )
                  , ( "2012-01-14", 107.1 )
                  ]
                , [ ( "2012-01-12", 23.1 )
                  , ( "2012-01-13", 34.1 )
                  , ( "2012-01-14", 45.1 )
                  ]
                , [ ( "2012-01-12", 13.1 )
                  , ( "2012-01-13", 15.1 )
                  , ( "2012-01-14", 19.1 )
                  ]
                ]
    in
    Task.succeed msg
        |> Task.perform identity


type Msg
    = GotList (Result Http.Error (List Int))
    | SetStartAmount String
    | SetDatePicker DatePicker.Msg
    | SetCurrentPercentage String
    | SetCurrentTicker String
    | AddAllocation
    | Submit
    | ReceiveData (List (List ( String, Float )))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotList result ->
            case result of
                Ok myList ->
                    { model | backendState = Success myList }
                        |> addCmd Cmd.none

                Err _ ->
                    { model | backendState = Failure }
                        |> addCmd Cmd.none

        SetStartAmount amountString ->
            { model | initialBalance = String.toInt amountString }
                |> addCmd Cmd.none

        SetDatePicker subMsg ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update (datePickerSettings model.datePicker) subMsg model.datePicker

                startDate =
                    case dateEvent of
                        DatePicker.Picked newDate ->
                            Just newDate

                        _ ->
                            model.startDate
            in
            { model
                | startDate = startDate
                , datePicker = newDatePicker
            }
                |> addCmd Cmd.none

        SetCurrentPercentage percentage ->
            { model | currentPercentage = String.toFloat percentage }
                |> addCmd Cmd.none

        SetCurrentTicker ticker ->
            case String.isEmpty ticker of
                True ->
                    { model | currentTicker = Nothing }
                        |> addCmd Cmd.none

                False ->
                    { model | currentTicker = Just (String.toUpper ticker) }
                        |> addCmd Cmd.none

        AddAllocation ->
            let
                remainingPercentage : Float
                remainingPercentage =
                    case model.currentPercentage of
                        Just currentPercentage ->
                            100 - totalPercentage model - currentPercentage

                        Nothing ->
                            100 - totalPercentage model
            in
            case currentAllocation model of
                Just allocation ->
                    if invalidAllocation model then
                        { model | currentPercentage = Nothing }
                            |> addCmd Cmd.none

                    else
                        { model
                            | portfolio = allocation :: model.portfolio
                            , currentPercentage = Just remainingPercentage
                            , currentTicker = Nothing
                        }
                            |> addCmd Cmd.none

                Nothing ->
                    ( model, Cmd.none )

        Submit ->
            ( model, postToBackend model )

        ReceiveData receivedData ->
            model
                |> setData receivedData
                |> addCmd Cmd.none


setData : List (List ( String, Float )) -> Model -> Model
setData receivedData model =
    case receivedData of
        [ n1, n2, n3 ] ->
            { model | chartData = ChartData (toData n1) (toData n2) (toData n3) }

        _ ->
            model


toData : List ( String, Float ) -> List Datum
toData receivedData =
    let
        toDatum : ( String, Float ) -> Datum
        toDatum ( isoString, amount ) =
            case Iso8601.toTime isoString of
                Ok time ->
                    Datum time amount

                _ ->
                    Datum (Time.millisToPosix 100000000) 0
    in
    List.map toDatum receivedData


datePickerSettings : DatePicker.DatePicker -> DatePicker.Settings
datePickerSettings datePicker =
    let
        isDisabledAfter : Date -> Date -> Bool
        isDisabledAfter laterDate date =
            Date.compare laterDate date == LT

        today =
            DatePicker.getInitialDate datePicker
    in
    { defaultSettings
        | inputClassList = [ ( "input", True ) ]
        , isDisabled = isDisabledAfter today
        , firstDayOfWeek = Time.Mon
        , placeholder = "Click here to pick a date"
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    let
        goodToGo =
            case formSubmittable model of
                True ->
                    "Yes"

                False ->
                    "No"
    in
    div [ class "container" ]
        [ div [ class "column" ] [ text ("total: " ++ String.fromFloat (totalPercentage model) ++ "\tOK: " ++ goodToGo ++ "\nMessageBody: " ++ Json.Encode.encode 4 (stocksEncoder model)) ]
        , div [ class "columns" ]
            [ div [ class "column is-one-third" ]
                [ h1 [ class "title has-text-primary is-size-1" ] [ text "ticker!" ]
                , Html.form [ class "form" ]
                    [ viewStartDate model
                    , viewStartAmount model
                    , viewAllocationAdder model
                    , viewAllocationLister model
                    , viewSubmitter model
                    ]
                ]
            , div [ class "column" ]
                [ viewChart model ]
            ]
        ]


viewStartDate : Model -> Html Msg
viewStartDate model =
    div [ class "field" ]
        [ Html.label [ class "label" ] [ text "Start Date" ]
        , div [ class "control" ]
            [ DatePicker.view model.startDate (datePickerSettings model.datePicker) model.datePicker
                |> Html.map SetDatePicker
            ]
        ]


viewStartAmount : Model -> Html Msg
viewStartAmount model =
    let
        initialBalance =
            case model.initialBalance of
                Just amount ->
                    String.fromInt amount

                Nothing ->
                    ""
    in
    div [ class "field" ]
        [ Html.label [ class "label" ] [ text "Initial Balance" ]
        , div [ class "control has-icons-left" ]
            [ input
                [ type_ "text"
                , value initialBalance
                , onBlurWithTargetValue SetStartAmount
                , class "input"
                , placeholder "Initial Balance"
                ]
                []
            , span [ class "icon is-left" ]
                [ i [ class "fas fa-dollar-sign" ] []
                ]
            ]
        ]


viewAllocationAdder : Model -> Html Msg
viewAllocationAdder model =
    let
        currentTicker =
            case model.currentTicker of
                Just string ->
                    string

                Nothing ->
                    ""

        currentPercentage =
            case model.currentPercentage of
                Just float ->
                    String.fromFloat float

                Nothing ->
                    ""
    in
    div [ class "field" ]
        [ Html.label [ class "label" ] [ text "Portfolio:" ]
        , div [ class "field is-grouped" ]
            [ div [ class "control" ]
                [ button
                    [ type_ "button"
                    , class "button is-primary"
                    , disabled (invalidAllocation model)
                    , onClick AddAllocation
                    ]
                    [ span [ class "icon" ]
                        [ i [ class "fas fa-plus" ] [] ]
                    ]
                ]
            , div [ class "control has-icons-right" ]
                [ input
                    [ type_ "text"
                    , value currentPercentage
                    , onBlurWithTargetValue SetCurrentPercentage
                    , class "input"
                    , id "percentage-input"
                    ]
                    []
                , span [ class "icon is-right" ]
                    [ i [ class "fas fa-percent" ] []
                    ]
                ]
            , div [ class "control" ]
                [ input
                    [ type_ "text"
                    , value currentTicker
                    , onInput SetCurrentTicker
                    , onEnter AddAllocation
                    , class "input"
                    , placeholder "ticker"
                    , id "ticker-input"
                    ]
                    []
                ]
            ]
        ]


viewAllocationLister : Model -> Html Msg
viewAllocationLister model =
    div [ class "content" ]
        [ ul []
            (model.portfolio
                |> List.sortBy ((*) -1 << .percentage)
                |> List.map
                    (\allocation ->
                        li []
                            [ text (String.fromFloat allocation.percentage ++ "%: " ++ allocation.ticker)
                            ]
                    )
            )
        ]


viewSubmitter : Model -> Html Msg
viewSubmitter model =
    button
        [ class "button is-primary stretch"
        , type_ "button"
        , disabled (not (formSubmittable model))
        , onClick Submit
        ]
        [ text "Get Chart" ]


viewChart : Model -> Html Msg
viewChart model =
    div []
        [ LineChart.viewCustom (chartConfig model)
            [ LineChart.line Colors.pink Dots.diamond "Nora" model.chartData.nora
            , LineChart.line Colors.cyan Dots.circle "Noah" model.chartData.noah
            , LineChart.line Colors.blue Dots.triangle "Nina" model.chartData.nina
            ]
        ]


chartConfig : Model -> LineChart.Config Datum Msg
chartConfig model =
    { y = Axis.default 450 "value" .value
    , x = Axis.time Time.utc 800 "time" (toFloat << Time.posixToMillis << .time)
    , container = containerConfig
    , interpolation = Interpolation.monotone
    , intersection = Intersection.default
    , legends = Legends.default
    , events = Events.default
    , junk = Junk.default
    , grid = Grid.dots 1 Colors.gray
    , area = Area.stacked 0.5
    , line = Line.default
    , dots = Dots.custom (Dots.empty 5 1)
    }


containerConfig : Container.Config Msg
containerConfig =
    Container.custom
        { attributesHtml = []
        , attributesSvg = []
        , size = Container.relative
        , margin = Container.Margin 30 100 30 70
        , id = "line-chart-area"
        }


pingBackend : Cmd Msg
pingBackend =
    Http.request
        { method = "GET"
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        , headers = []
        , url = "http://localhost:4000/api"
        , expect = Http.expectJson GotList myDecoder
        }


myDecoder : Decoder (List Int)
myDecoder =
    field "data" (field "draw" (Json.Decode.list Json.Decode.int))


onBlurWithTargetValue : (String -> msg) -> Attribute msg
onBlurWithTargetValue toMsg =
    on "blur" (Json.Decode.map toMsg targetValue)


currentAllocation : Model -> Maybe Allocation
currentAllocation model =
    case ( model.currentPercentage, model.currentTicker ) of
        ( Just percentage, Just ticker ) ->
            if percentage == 0 then
                Nothing

            else
                Just (Allocation percentage ticker)

        _ ->
            Nothing


totalPercentage : Model -> Float
totalPercentage model =
    model.portfolio
        |> List.map .percentage
        |> List.sum


formSubmittable : Model -> Bool
formSubmittable model =
    case ( totalPercentage model == 100, model.startDate, model.initialBalance ) of
        ( True, Just _, Just _ ) ->
            True

        _ ->
            False


onEnter : msg -> Html.Attribute msg
onEnter msg =
    let
        isEnterKey keyCode =
            if keyCode == 13 then
                Json.Decode.succeed msg

            else
                Json.Decode.fail "silent failure :)"
    in
    on "keyup" <|
        Json.Decode.andThen isEnterKey Html.Events.keyCode


allocationIncomplete : Model -> Bool
allocationIncomplete model =
    case currentAllocation model of
        Nothing ->
            True

        _ ->
            False


percentageOverHundred : Model -> Bool
percentageOverHundred model =
    case model.currentPercentage of
        Just float ->
            totalPercentage model + float > 100

        Nothing ->
            False


invalidAllocation : Model -> Bool
invalidAllocation model =
    case ( allocationIncomplete model, percentageOverHundred model ) of
        ( False, False ) ->
            False

        _ ->
            True


stocksEncoder : Model -> Json.Encode.Value
stocksEncoder model =
    case ( model.startDate, model.initialBalance ) of
        ( Just startDate, Just initialBalance ) ->
            Json.Encode.object
                [ ( "stocks", Json.Encode.list (stockEncoder << toStock startDate initialBalance) model.portfolio )
                ]

        _ ->
            Json.Encode.string "Form not submittable"


stockEncoder : Stock -> Json.Encode.Value
stockEncoder stock =
    Json.Encode.object
        [ ( "ticker", Json.Encode.string stock.ticker )
        , ( "startDate", Json.Encode.string (Date.toIsoString stock.startDate) )
        , ( "initialValue", Json.Encode.float stock.initialValue )
        ]


type alias Stock =
    { ticker : String
    , startDate : Date
    , initialValue : Float
    }


toStock : Date -> Int -> Allocation -> Stock
toStock startDate initialBalance allocation =
    { ticker = allocation.ticker
    , startDate = startDate
    , initialValue = allocation.percentage / 100 * toFloat initialBalance
    }


postToBackend : Model -> Cmd Msg
postToBackend model =
    Http.request
        { method = "POST"
        , body = Http.jsonBody (stocksEncoder model)
        , timeout = Nothing
        , tracker = Nothing
        , headers = []
        , url = "http://localhost:4000/api"
        , expect = Http.expectJson GotList myDecoder
        }


addCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
addCmd cmd model =
    ( model, cmd )
