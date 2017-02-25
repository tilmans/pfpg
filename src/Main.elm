port module Main exposing (..)

import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (..)
import Navigation exposing (Location, modifyUrl)
import AFrame exposing (scene, entity)
import AFrame.Primitives as AP exposing (..)
import AFrame.Primitives.Attributes as AA exposing (..)
import AFrame.Primitives.Camera exposing (..)
import ModelLoader exposing (..)
import Color exposing (rgb)


port updateVotes : (List Vote -> msg) -> Sub msg


port setUser : String -> Cmd msg


port setVote : Int -> Cmd msg


type alias Model =
    { votes : List Vote
    , name : Maybe String
    , vote : Maybe Int
    , inputName : String
    , revealed : Bool
    }


type alias Vote =
    { id : String
    , user : String
    , vote : Int
    }


type Msg
    = VotesUpdated (List Vote)
    | SetVote Int
    | UrlChange Location
    | Name String
    | SetName


voteValues : List number
voteValues =
    [ 0, 1, 2, 3, 5, 8, 13 ]


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( name, id ) =
            getIdFrom location.search

        cmd =
            case name of
                Nothing ->
                    Cmd.none

                Just name ->
                    setUser name
    in
        ( Model [] name Nothing "" False, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        VotesUpdated votes ->
            let
                _ =
                    Debug.log "Votes" votes

                allVoted =
                    case model.vote of
                        Nothing ->
                            False

                        Just _ ->
                            (List.all (\v -> v.vote /= -1) votes)
            in
                { model | votes = votes, revealed = allVoted } ! []

        SetVote vote ->
            ( { model | vote = Just vote }, setVote vote )

        UrlChange location ->
            let
                ( name, id ) =
                    getIdFrom location.search

                cmd =
                    case name of
                        Nothing ->
                            Cmd.none

                        Just name ->
                            setUser name
            in
                { model | name = name } ! [ cmd ]

        Name name ->
            { model | inputName = name } ! []

        SetName ->
            model ! [ modifyUrl ("?name=" ++ model.inputName) ]


subscriptions : Model -> Sub Msg
subscriptions model =
    updateVotes VotesUpdated


displayVote : Vote -> Html msg
displayVote vote =
    div []
        [ div [ class "vote" ] [ Html.text (vote.user ++ ": " ++ (toString vote.vote)) ]
        ]


displayCard : Int -> Html Msg
displayCard value =
    span [ class "card", onClick (SetVote value) ] [ Html.text (toString value) ]


view : Model -> Html Msg
view model =
    case model.name of
        Nothing ->
            htmlView model

        Just _ ->
            aframeScene model


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


table : Html msg
table =
    entity
        [ plymodel "src: url(/models/table.ply)"
        , scale 0.2 0.2 0.2
        , rotation -70 0 0
        , position 0 -2.42 -9.28
        ]
        []


aframeScene : Model -> Html Msg
aframeScene model =
    scene
        []
        ((allCards model.vote)
            :: (allPlayers model.votes)
            :: (allVotes model.votes model.revealed)
            :: table
            :: [ camera [ position 0 0 0 ] [ cursor ]
               , assets [] []
               , sky [ color (rgb 3 10 28) ] []
               ]
        )


allCards : Maybe Int -> Html Msg
allCards vote =
    entity [ position 0 -2 0 ]
        (List.indexedMap (cardImage vote) voteValues)


htmlView : Model -> Html Msg
htmlView model =
    div [ id "wrap" ]
        [ div [ id "inputform" ]
            [ div [ id "container" ] [ img [ HA.src "login.png" ] [] ]
              --scene [] [] ]
            , div [ id "formcontainer" ]
                [ input [ HA.type_ "text", placeholder "Player Name", onInput Name ] []
                , button [ HA.disabled (model.inputName == ""), onClick SetName ] [ Html.text "Go!" ]
                ]
            ]
        ]


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


player : Vote -> ( Float, Float ) -> Html msg
player vote pos =
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
                [ ModelLoader.value vote.user
                , ModelLoader.align "center"
                , anchor "center"
                , position 0 3 0
                , scale 2 2 2
                ]
                []
            ]


allPlayers : List Vote -> Html msg
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


allVotes : List Vote -> Bool -> Html Msg
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


cardOnTable : Vote -> ( Float, Float ) -> Bool -> Html Msg
cardOnTable vote pos done =
    let
        ( x, z ) =
            pos

        s =
            0.04

        card =
            if done then
                cardModel vote.vote
            else
                blankCard
    in
        if vote.vote == -1 then
            entity [] []
        else
            entity
                [ card
                , rotation -90 0 0
                , scale s s s
                , position x 0 z
                ]
                []


getIdFrom : String -> ( Maybe String, Maybe String )
getIdFrom location =
    let
        string =
            String.dropLeft 1 location

        subs =
            String.split "&" string

        name =
            List.foldr (extract "name") Nothing subs

        id =
            List.foldr (extract "room") Nothing subs
    in
        ( name, id )


extract : String -> String -> Maybe String -> Maybe String
extract lookFor values accum =
    let
        subs =
            String.split "=" values

        key =
            Maybe.withDefault "" (List.head subs)
    in
        if key == lookFor then
            List.head (Maybe.withDefault [] (List.tail subs))
        else
            accum


scalefactor : Float
scalefactor =
    0.03


blankCard : Attribute Msg
blankCard =
    let
        modelurl =
            "src: url(/models/back.ply)"
    in
        plymodel modelurl


cardModel : Int -> Attribute msg
cardModel number =
    let
        modelurl =
            "src: url(/models/" ++ (toString number) ++ ".ply)"
    in
        plymodel modelurl


cardImage : Maybe Int -> Int -> Int -> Html Msg
cardImage selection index number =
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
            , onClick (SetVote number)
            ]
            []
