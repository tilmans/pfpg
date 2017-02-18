module ModelLoader exposing (..)

import Html.Attributes exposing (attribute)
import Html exposing (Attribute, node, Html)


plymodel : String -> Attribute msg
plymodel value =
    attribute "ply-model" value


layout : String -> Attribute msg
layout value =
    attribute "layout" value


lookAt : String -> Attribute msg
lookAt value =
    attribute "look-at" value


text : List (Attribute msg) -> List (Html msg) -> Html msg
text =
    node "a-text"


anchor value =
    attribute "anchor" value


value value =
    attribute "value" value


align value =
    attribute "align" value
