module Main exposing (main)

import Browser
import Date exposing (Date)
import DatePicker exposing (DatePicker, defaultSettings)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, decodeString, field, int, list, string)
import Json.Encode exposing (encode, int, object, string)
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
    | Submit


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
                    ( { model | currentTicker = Just (String.toUpper ticker) }, Cmd.none )

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

        Submit ->
            ( model, postToBackend model )


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
        [ div [ class "column" ] [ text ("total: " ++ String.fromFloat (totalPercentage model) ++ "\tOK: " ++ goodToGo ++ "\nMessageBody: " ++ Json.Encode.encode 4 (messageBody model)) ]
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
            , div [ class "column" ] []
            ]
        ]


viewStartDate : Model -> Html Msg
viewStartDate model =
    div [ class "field" ]
        [ label [ class "label" ] [ text "Start Date" ]
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
        [ label [ class "label" ] [ text "Initial Balance" ]
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
        [ label [ class "label" ] [ text "Portfolio:" ]
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


messageBody : Model -> Json.Encode.Value
messageBody model =
    case ( model.startDate, model.initialBalance ) of
        ( Just startDate, Just initialBalance ) ->
            Json.Encode.object
                [ ( "startDate", Json.Encode.string (Date.toIsoString startDate) )
                , ( "initialBalance", Json.Encode.int initialBalance )
                , ( "portfolio", Json.Encode.list allocationEncoder model.portfolio )
                ]

        _ ->
            Json.Encode.string "Form not submittable"


allocationEncoder : Allocation -> Json.Encode.Value
allocationEncoder allocation =
    Json.Encode.object
        [ ( "percentage", Json.Encode.float allocation.percentage )
        , ( "ticker", Json.Encode.string allocation.ticker )
        ]


postToBackend : Model -> Cmd Msg
postToBackend model =
    Http.request
        { method = "POST"
        , body = Http.jsonBody (messageBody model)
        , timeout = Nothing
        , tracker = Nothing
        , headers = []
        , url = "http://localhost:4000/api"
        , expect = Http.expectJson GotList myDecoder
        }
