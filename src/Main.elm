module Main exposing (main)

import Browser
import Date exposing (Date)
import DatePicker exposing (DatePicker, defaultSettings)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, list, string)
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
    , startAmount : Int
    , date : Maybe Date
    , datePicker : DatePicker
    }


type BackendState
    = Failure
    | Loading
    | Success (List Int)


type Msg
    = GotList (Result Http.Error (List Int))
    | SetStartAmount String
    | SetDatePicker DatePicker.Msg


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( datePicker, datePickerCmd ) =
            DatePicker.init

        initialModel =
            { backendState = Loading
            , startAmount = 0
            , date = Nothing
            , datePicker = datePicker
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
                    DatePicker.update (settings model.datePicker) subMsg model.datePicker

                date =
                    case dateEvent of
                        DatePicker.Picked newDate ->
                            Just newDate

                        _ ->
                            model.date
            in
            ( { model
                | date = date
                , datePicker = newDatePicker
              }
            , Cmd.none
            )


settings : DatePicker.DatePicker -> DatePicker.Settings
settings datePicker =
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
    div [ class "container" ]
        [ div [ class "columns" ]
            [ div [ class "column" ] []
            , div [ class "column" ]
                [ h1 [ class "is-size-1 has-text-centered" ] [ text "ticker!" ]
                , div
                    [ class "box has-background-primary" ]
                    [ viewStartDate model
                    , viewStartAmount model
                    ]
                ]
            , div [ class "column" ] []
            ]
        ]


viewStartDate : Model -> Html Msg
viewStartDate model =
    div [ class "columns" ]
        [ label [ class "column has-text-weight-semibold" ] [ text "Start Date:" ]
        , div [ class "column" ]
            [ DatePicker.view model.date (settings model.datePicker) model.datePicker
                |> Html.map SetDatePicker
            ]
        ]


viewStartAmount : Model -> Html Msg
viewStartAmount model =
    div [ class "columns" ]
        [ label [ class "column has-text-weight-semibold" ] [ text "Initial Balance:" ]
        , div [ class "column field has-addons" ]
            [ div [ class "control" ]
                [ a [ class "button is-static" ] [ text "$" ] ]
            , div [ class "control" ]
                [ input
                    [ type_ "text"
                    , value (String.fromInt model.startAmount)
                    , onInput SetStartAmount
                    , class "input"
                    ]
                    []
                ]
            ]
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
