module Util exposing (addCmd, onBlurWithTargetValue, onEnter)

import Html
import Html.Events exposing (on, targetValue)
import Json.Decode as Decode


onBlurWithTargetValue : (String -> msg) -> Html.Attribute msg
onBlurWithTargetValue toMsg =
    on "blur" (Decode.map toMsg targetValue)


onEnter : msg -> Html.Attribute msg
onEnter msg =
    let
        isEnterKey keyCode =
            if keyCode == 13 then
                Decode.succeed msg

            else
                Decode.fail "silent failure :)"
    in
    on "keyup" <|
        Decode.andThen isEnterKey Html.Events.keyCode


addCmd : Cmd msg -> model -> ( model, Cmd msg )
addCmd cmd model =
    ( model, cmd )
