module Utils.OverrideClick exposing (..)

import Html exposing (Attribute)
import Html.Events exposing (stopPropagationOn)
import Json.Decode

disableDefault : msg -> ( msg, Bool )
disableDefault msg =
    ( msg, True )


overrideOnClickWith : msg -> Attribute msg
overrideOnClickWith msg =
    stopPropagationOn "click" (Json.Decode.map disableDefault (Json.Decode.succeed msg))