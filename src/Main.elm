module Main exposing (main)

import Browser
import Date exposing (Date)
import DatePicker exposing (DatePicker, defaultSettings)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, decodeString, field, int, list, string)
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
    }


type alias Allocation =
    { percentage : Float
    , ticker : String
    }


type BackendState
    = Failure
    | Loading
    | Success (List Int)


type Msg
    = GotList (Result Http.Error (List Int))
    | SetStartAmount String
    | SetDatePicker DatePicker.Msg
    | SetCurrentPercentage String
    | SetCurrentTicker String
    | AddAllocation


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
            }

        commands =
            Cmd.batch
                [ pingBackend
                , Cmd.map SetDatePicker datePickerCmd
                ]
    in
    ( initialModel, commands )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotList result ->
            case result of
                Ok myList ->
                    ( { model | backendState = Success myList }, Cmd.none )

                Err _ ->
                    ( { model | backendState = Failure }, Cmd.none )

        SetStartAmount amountString ->
            ( { model | initialBalance = String.toInt amountString }, Cmd.none )

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
            ( { model
                | startDate = startDate
                , datePicker = newDatePicker
              }
            , Cmd.none
            )

        SetCurrentPercentage percentage ->
            ( { model | currentPercentage = String.toFloat percentage }, Cmd.none )

        SetCurrentTicker ticker ->
            case String.isEmpty ticker of
                True ->
                    ( { model | currentTicker = Nothing }, Cmd.none )

                False ->
                    ( { model | currentTicker = Just ticker }, Cmd.none )

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
                        ( { model | currentPercentage = Nothing }, Cmd.none )

                    else
                        ( { model
                            | portfolio = allocation :: model.portfolio
                            , currentPercentage = Just remainingPercentage
                            , currentTicker = Nothing
                          }
                        , Cmd.none
                        )

                Nothing ->
                    ( model, Cmd.none )


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
        [ div [ class "columns" ]
            [ div [ class "column" ] [ text ("total: " ++ String.fromFloat (totalPercentage model) ++ "\tOK: " ++ goodToGo) ]
            , div [ class "column is-half" ]
                [ h1 [ class "is-size-1 has-text-centered" ] [ text "ticker!" ]
                , div
                    [ class "box has-background-primary" ]
                    [ viewStartDate model
                    , viewStartAmount model
                    , viewAllocationAdder model
                    , viewAllocationLister model
                    ]
                ]
            , div [ class "column" ] []
            ]
        ]


viewStartDate : Model -> Html Msg
viewStartDate model =
    div [ class "columns" ]
        [ label [ class "column label" ] [ text "Start Date:" ]
        , div [ class "column" ]
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
    div [ class "columns" ]
        [ label [ class "column label" ] [ text "Initial Balance:" ]
        , div [ class "column field has-addons" ]
            [ div [ class "control" ]
                [ a [ class "button is-static" ] [ text "$" ] ]
            , div [ class "control fullwidth" ]
                [ input
                    [ type_ "text"
                    , value initialBalance
                    , onBlurWithTargetValue SetStartAmount
                    , class "input"
                    , placeholder "Initial Balance"
                    ]
                    []
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
    div [ class "columns" ]
        [ div [ class "column" ]
            [ label [ class "label" ] [ text "Portfolio:" ] ]
        , div [ class "column field has-addons" ]
            [ div [ class "control" ]
                [ button
                    [ class "button"
                    , disabled (invalidAllocation model)
                    , onClick AddAllocation
                    ]
                    [ text "+"
                    ]
                ]
            , div [ class "control fullwidth" ]
                [ input
                    [ type_ "text"
                    , value currentPercentage
                    , onBlurWithTargetValue SetCurrentPercentage
                    , onEnter AddAllocation
                    , class "input"
                    , placeholder "percent"
                    ]
                    []
                ]
            , div [ class "control" ]
                [ a [ class "button is-static" ] [ text "%" ] ]
            , div [ class "control fullwidth" ]
                [ input
                    [ type_ "text"
                    , value currentTicker
                    , onInput SetCurrentTicker
                    , onEnter AddAllocation
                    , class "input"
                    , placeholder "ticker"
                    ]
                    [ text "foo" ]
                ]
            ]
        ]


viewAllocationLister : Model -> Html Msg
viewAllocationLister model =
    ul []
        (List.map
            (\allocation ->
                li []
                    [ text (String.fromFloat allocation.percentage ++ ":\t" ++ allocation.ticker)
                    ]
            )
            model.portfolio
        )


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
    field "data" (field "draw" (Json.Decode.list int))


onBlurWithTargetValue : (String -> msg) -> Attribute msg
onBlurWithTargetValue toMsg =
    on "blur" (Json.Decode.map toMsg targetValue)


currentAllocation : Model -> Maybe Allocation
currentAllocation model =
    case ( model.currentPercentage, model.currentTicker ) of
        ( Just percentage, Just ticker ) ->
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
