module Geometry exposing (..)

import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (..)
import ModelLoader exposing (..)
import AFrame exposing (entity)
import AFrame.Primitives.Attributes as AA exposing (..)
import AFrame.Animations as Anim exposing (..)
import AFrame.Primitives.Camera exposing (..)
import AFrame.Primitives as AP exposing (..)
import AFrame.Primitives.Attributes as AA exposing (..)
import Color exposing (rgb)


scalefactor : Float
scalefactor =
    0.03


blankCard : Attribute msg
blankCard =
    plymodel "src: url(/models/back.ply)"


cardModel : Int -> Attribute msg
cardModel number =
    let
        modelurl =
            "src: url(/models/" ++ (toString number) ++ ".ply)"
    in
        plymodel modelurl


table : Html msg
table =
    entity
        [ plymodel "src: url(/models/table.ply)"
        , scale 0.2 0.2 0.2
        , rotation -70 0 0
        , position 0 -2.42 -9.28
        ]
        []


allPlayers : List String -> Html msg
allPlayers votes =
    let
        segments =
            List.length votes

        seg_length =
            pi / (toFloat segments)

        seg_offset =
            seg_length / 2

        radius =
            5

        coords =
            List.map
                (\i ->
                    ( cos ((toFloat i) * seg_length - seg_offset) * radius
                    , sin ((toFloat i) * seg_length - seg_offset) * radius * -1
                    )
                )
                (List.range 1 segments)
    in
        entity [ position 0 -0.19 -8.85, rotation 20 0 0 ]
            (List.map2 (\v pos -> player v pos) votes coords)


cardOnTable : Int -> ( Float, Float ) -> Bool -> Html msg
cardOnTable vote pos done =
    let
        ( x, z ) =
            pos

        s =
            0.04

        card =
            if done then
                cardModel vote
            else
                blankCard
    in
        if vote == -1 then
            entity [] []
        else
            entity
                [ card
                , rotation -90 0 0
                , scale s s s
                , position x 0 z
                ]
                []


allVotes : List Int -> Bool -> Html msg
allVotes votes done =
    let
        segments =
            List.length votes

        seg_length =
            pi / (toFloat segments)

        seg_offset =
            seg_length / 2

        radius =
            3.5

        coords =
            List.map
                (\i ->
                    ( cos ((toFloat i) * seg_length - seg_offset) * radius
                    , sin ((toFloat i) * seg_length - seg_offset) * radius * -1
                    )
                )
                (List.range 1 segments)
    in
        entity [ position 0 -0.28 -7.99, rotation 20 0 0 ]
            (List.map2 (\v pos -> cardOnTable v pos done) votes coords)


player : String -> ( Float, Float ) -> Html msg
player name pos =
    let
        ( x, z ) =
            pos
    in
        entity [ lookAt "[camera]", position x 0 z ]
            [ entity
                [ plymodel "src: url(/models/chr_headphones.ply)"
                , scale 0.2 0.2 0.2
                , rotation -90 0 0
                ]
                []
            , ModelLoader.text
                [ ModelLoader.value name
                , ModelLoader.align "center"
                , anchor "center"
                , position 0 3 0
                , scale 2 2 2
                ]
                []
            ]


cardSelection : Bool -> Maybe Int -> msg -> Html msg
cardSelection voted vote event =
    if voted then
        entity
            [ cardModel (Maybe.withDefault 0 vote)
            , scale 0.04 0.04 0.04
            , rotation -70 0 0
            , position -0.11 -1.06 -5.89
            ]
            []
    else
        entity
            [ rotation -70 0 0
            , scale 1 1.65 1
            , position 0 -1.03 -5.89
            , attribute "geometry" "primitive: plane"
            , attribute "material" "shader:flat; opacity:1; color:#f00"
            , onClick event
            ]
            (modelAnimation vote)


modelAnimation : Maybe a -> List (Html msg)
modelAnimation vote =
    case vote of
        Nothing ->
            []

        Just _ ->
            [ animation
                [ attribute_ "material.opacity"
                , dur 1000
                , to "0.5"
                , from "0.0"
                , attribute "direction" "alternate"
                , attribute "easing" "ease-out"
                , attribute "repeat" "indefinite"
                ]
                []
            ]


allCards : Maybe Int -> List Int -> (Int -> a) -> Html a
allCards vote voteValues event =
    entity [ position 0 -2.32 0 ]
        (List.indexedMap (cardImage vote event) voteValues)


cardImage : Maybe Int -> (Int -> a) -> Int -> Int -> Html a
cardImage selection message index number =
    let
        xoff =
            (toFloat index) * 0.6

        xpos =
            ((toFloat index) - 0.6 * 2) - xoff

        neutral =
            ( 1, -10 )

        selected =
            ( 1.3, -10 )

        ( ypos, xrot ) =
            case selection of
                Nothing ->
                    neutral

                Just sel ->
                    if number == sel then
                        selected
                    else
                        neutral

        distance =
            toFloat index - 3

        rotsteps =
            -9

        zrotation =
            floor (distance * 2 * rotsteps)

        yposStep =
            0.08

        yposOff =
            -1 * (abs distance) ^ 2 * yposStep

        cardpos =
            position xpos (ypos + yposOff) -3
    in
        entity
            [ cardModel number
            , cardpos
            , scale scalefactor scalefactor scalefactor
            , rotation xrot 20 zrotation
            , onClick (message number)
            ]
            []


cursor : Html msg
cursor =
    entity
        [ attribute "cursor" "fuse:true; fuseTimeout: 1"
        , attribute "geometry" "primitive: ring; radiusInner: 0.02; radiusOuter: 0.03"
        , position 0 0 -1
        , attribute "material" "color:red; shader:flat"
        ]
        [{--entity
        [ plymodel "src: url(/models/hand.ply)"
        , rotation -40 0 0
        , scale 0.007 0.007 0.007
        , position 0.05 -0.1 -0.3
        ]
        [] --}
        ]


sceneCamera : Html msg
sceneCamera =
    camera
        [ position 0 0 0
        , rotation -15 0 0
        ]
        [ cursor ]


sceneAssets : Html msg
sceneAssets =
    assets [] []


sceneSky : Html msg
sceneSky =
    sky [ color (rgb 3 10 28) ] []
