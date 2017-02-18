module ModelLoader exposing (..)

import Html.Attributes exposing (attribute)
import Html exposing (Attribute)


plymodel : String -> Attribute msg
plymodel value =
    attribute "ply-model" value


layout : String -> Attribute msg
layout value =
    attribute "layout" value


lookAt : String -> Attribute msg
lookAt value =
    attribute "look-at" value
