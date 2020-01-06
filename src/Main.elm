module Main exposing (main)

import Browser
import Date exposing (Date)
import DatePicker exposing (DatePicker)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, list, string)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { backendState : BackendState
    , startAmount : Int
    , startDate : Maybe Date
    , datePicker : DatePicker
    }


type BackendState
    = Failure
    | Loading
    | Success (List Int)


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( datePicker, datePickerCmd ) =
            DatePicker.init

        initialModel =
            { backendState = Loading
            , startAmount = 0
            , startDate = Nothing
            , datePicker = datePicker
            }

        commands =
            Cmd.batch
                [ pingBackend
                , Cmd.map SetDatePicker datePickerCmd
                ]
    in
    ( initialModel, commands )


type Msg
    = GotList (Result Http.Error (List Int))
    | SetStartAmount String
    | SetDatePicker DatePicker.Msg


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
            let
                amount =
                    String.toInt amountString
            in
            case amount of
                Just amountInt ->
                    ( { model | startAmount = amountInt }, Cmd.none )

                Nothing ->
                    ( { model | startAmount = 0 }, Cmd.none )

        SetDatePicker subMsg ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update someSettings subMsg model.datePicker

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


someSettings : DatePicker.Settings
someSettings =
    DatePicker.defaultSettings


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Ticker!" ]
        , viewStartDate model
        , viewStartAmount model
        , viewData model
        ]


viewStartDate : Model -> Html Msg
viewStartDate model =
    div []
        [ label [] [ text "Start Date:" ]
        , DatePicker.view model.startDate someSettings model.datePicker
            |> Html.map SetDatePicker
        ]


viewStartAmount : Model -> Html Msg
viewStartAmount model =
    div []
        [ label [] [ text "Initial Balance: $" ]
        , input
            [ type_ "text"
            , value (String.fromInt model.startAmount)
            , onInput SetStartAmount
            ]
            []
        ]


viewData : Model -> Html Msg
viewData model =
    case model.backendState of
        Failure ->
            div []
                [ text "I could not load the data . "
                ]

        Loading ->
            text "Loading..."

        Success myList ->
            div []
                [ renderList myList
                ]


renderList : List Int -> Html msg
renderList lst =
    ul []
        (List.map (\l -> li [] [ text (String.fromInt l) ]) lst)


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
