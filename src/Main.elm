module Main exposing (main)

import Browser
import Data.Allocation as Allocation exposing (Allocation)
import Data.Chart as Chart exposing (Chart(..))
import Data.Stock as Stock
import Date exposing (Date)
import DatePicker exposing (DatePicker, defaultSettings)
import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import Helpers.LineChart as Helper
import Html exposing (Html, br, button, div, h1, i, input, li, span, text, ul)
import Html.Attributes exposing (class, disabled, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode as Encode exposing (object, string)
import LineChart as LineChart
import LineChart.Dots as Dots
import Maybe.Extra exposing (isNothing)
import Time
import Util exposing (addCmd, onBlurWithTargetValue, onEnter)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
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
    , chart : Chart
    , currentWorth : Maybe Float
    }


type BackendState
    = Failure
    | Loading
    | Success


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
            , chart = Chart [ { ticker = "", data = [] }, { ticker = "", data = [] }, { ticker = "", data = [] } ]
            , currentWorth = Nothing
            }

        commands =
            Cmd.map SetDate datePickerCmd
    in
    ( initialModel, commands )


type Msg
    = GotHistory (Result Http.Error Chart)
    | SetStartAmount String
    | SetDate DatePicker.Msg
    | SetPercentage String
    | SetTicker String
    | AddAllocation
    | Submit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotHistory response ->
            case response of
                Ok chart ->
                    { model | backendState = Success, chart = chart, currentWorth = Chart.worth chart }
                        |> addCmd Cmd.none

                Err _ ->
                    { model | backendState = Failure }
                        |> addCmd Cmd.none

        SetStartAmount amountString ->
            { model | initialBalance = String.toInt amountString }
                |> addCmd Cmd.none

        SetDate subMsg ->
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

        SetPercentage percentage ->
            { model | currentPercentage = String.toFloat percentage }
                |> addCmd Cmd.none

        SetTicker ticker ->
            if String.isEmpty ticker then
                { model | currentTicker = Nothing }
                    |> addCmd Cmd.none

            else
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
            case Allocation.current model.currentPercentage model.currentTicker of
                Just allocation ->
                    if percentageOverHundred model then
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


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "columns" ]
            [ div [ class "column is-one-third" ]
                [ br [] []
                , h1 [ class "title has-text-primary is-size-1" ] [ text "ticker!" ]
                , Html.form [ class "form" ]
                    [ viewStartDate model
                    , viewStartAmount model
                    , viewAllocationAdder model
                    , viewAllocationLister model
                    , viewSubmitter model
                    , viewCurrentWorth model
                    ]
                ]
            , div [ class "column" ]
                [ viewChart model.chart model.portfolio ]
            ]
        ]


viewStartDate : Model -> Html Msg
viewStartDate model =
    div [ class "field" ]
        [ Html.label [ class "label" ] [ text "Start Date" ]
        , div [ class "control" ]
            [ DatePicker.view model.startDate (datePickerSettings model.datePicker) model.datePicker
                |> Html.map SetDate
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
                    , disabled (adderDisabled model)
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
                    , onBlurWithTargetValue SetPercentage
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
                    , onInput SetTicker
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


viewCurrentWorth : Model -> Html Msg
viewCurrentWorth model =
    let
        message =
            case model.currentWorth of
                Just sum ->
                    "Your portfolio is now worth $ " ++ FormatNumber.format usLocale sum

                Nothing ->
                    ""
    in
    div []
        [ br [] []
        , h1 [ class "title has-text-primary is-size-5" ] [ text message ]
        ]


viewChart : Chart -> List Allocation -> Html Msg
viewChart (Chart chart) portfolio =
    let
        mapper =
            \item color -> LineChart.line color Dots.none item.ticker item.data
    in
    div []
        [ LineChart.viewCustom Helper.chartConfig (List.map2 mapper chart (Helper.colorOptions portfolio))
        ]


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


adderDisabled : Model -> Bool
adderDisabled model =
    isNothing (Allocation.current model.currentPercentage model.currentTicker) || percentageOverHundred model


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


percentageOverHundred : Model -> Bool
percentageOverHundred model =
    case model.currentPercentage of
        Just float ->
            totalPercentage model + float > 100

        Nothing ->
            False


requestEncoder : Model -> Encode.Value
requestEncoder model =
    case ( model.startDate, model.initialBalance ) of
        ( Just startDate, Just initialBalance ) ->
            Encode.object
                [ ( "stocks", Encode.list (Stock.encoder << Stock.build startDate initialBalance) model.portfolio )
                ]

        _ ->
            Encode.string "Form not submittable"


postToBackend : Model -> Cmd Msg
postToBackend model =
    Http.request
        { method = "POST"
        , body = Http.jsonBody (requestEncoder model)
        , timeout = Nothing
        , tracker = Nothing
        , headers = []
        , url = "http://localhost:4000/api"
        , expect = Http.expectJson GotHistory Chart.decoder
        }
