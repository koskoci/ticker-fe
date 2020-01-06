module Main exposing (main)

import Browser
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
    , investment : Int
    }


type BackendState
    = Failure
    | Loading
    | Success String


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialModel =
            { backendState = Loading, investment = 0 }
    in
    ( initialModel, pingBackend )


type Msg
    = GotString (Result Http.Error String)
    | SetInvestment String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotString result ->
            case result of
                Ok myString ->
                    ( { model | backendState = Success myString }, Cmd.none )

                Err _ ->
                    ( { model | backendState = Failure }, Cmd.none )

        SetInvestment investmentString ->
            let
                investment =
                    String.toInt investmentString
            in
            case investment of
                Just integer ->
                    ( { model | investment = integer }, Cmd.none )

                Nothing ->
                    ( { model | investment = 0 }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Ticker!" ]
        , viewInvestment model
        , viewData model
        ]


viewInvestment : Model -> Html Msg
viewInvestment model =
    div []
        [ label [] [ text "Starting investment: $" ]
        , input
            [ type_ "text"
            , value (String.fromInt model.investment)
            , onInput SetInvestment
            ]
            []
        ]


viewData : Model -> Html Msg
viewData model =
    case model.backendState of
        Failure ->
            div []
                [ text "I could not load a random cat for some reason. "
                ]

        Loading ->
            text "Loading..."

        Success myString ->
            div []
                [ text myString
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
        , expect = Http.expectString GotString
        }


myDecoder : Decoder (List Int)
myDecoder =
    field "data" (field "draw" (Json.Decode.list int))
