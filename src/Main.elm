port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation exposing (Location)
import AFrame exposing (scene, entity)
import AFrame.Primitives as AP exposing (..)
import AFrame.Primitives.Attributes as AA exposing (..)
import AFrame.Primitives.Camera exposing (..)
import AFrame.Primitives.Cursor exposing (..)
import ModelLoader exposing (..)


port updateVotes : (List Vote -> msg) -> Sub msg


port setUser : String -> Cmd msg


port setVote : Int -> Cmd msg


type alias Model =
    { votes : List Vote
    , name : String
    , vote : Maybe Int
    }


type alias Vote =
    { user : String
    , vote : Int
    }


type Msg
    = VotesUpdated (List Vote)
    | SetVote Int
    | UrlChange Location


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
        ( Model [] (Maybe.withDefault "Default" name) Nothing, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        VotesUpdated votes ->
            { model | votes = votes } ! []

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
                { model | name = (Maybe.withDefault "Default" name) } ! [ cmd ]


subscriptions : Model -> Sub Msg
subscriptions model =
    updateVotes VotesUpdated


displayVote : Vote -> Html msg
displayVote vote =
    div []
        [ div [ class "vote" ] [ text (vote.user ++ ": " ++ (toString vote.vote)) ]
        ]


displayCard : Int -> Html Msg
displayCard value =
    span [ class "card", onClick (SetVote value) ] [ text (toString value) ]


view : Model -> Html Msg
view model =
    aframeScene model


aframeScene : Model -> Html Msg
aframeScene model =
    scene
        []
        --AA.vrmodeui True ]
        ([ camera [ position 0 0 0 ] [ cursor [ fuse True ] [] ]
         , assets []
            [ img [ id "sky", AA.src "sky.jpg" ] []
            ]
         , sky [ AA.src "#sky" ] []
         ]
            ++ (List.indexedMap (cardImage model.vote) voteValues)
            ++ (allPlayers model.votes)
            ++ []
        )


htmlView : Model -> Html Msg
htmlView model =
    div []
        [ div [ class "vote-container" ] (List.map (\v -> displayVote v) model.votes)
        , div [ class "card-container" ] (List.map (\c -> displayCard c) voteValues)
        ]


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


allPlayers : List Vote -> List (Html msg)
allPlayers votes =
    [ box [] [] ]


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


cardImage : Maybe Int -> Int -> Int -> Html Msg
cardImage selection index number =
    let
        xpos =
            toFloat (index - 3)

        neutral =
            ( 1, -70 )

        selected =
            ( 1.5, -30 )

        ( ypos, xrot ) =
            case selection of
                Nothing ->
                    neutral

                Just sel ->
                    if number == sel then
                        selected
                    else
                        neutral

        cardpos =
            position xpos ypos -5

        modelurl =
            "src: url(/models/" ++ (toString number) ++ ".ply)"
    in
        entity
            [ plymodel modelurl
            , cardpos
            , scale scalefactor scalefactor scalefactor
            , rotation xrot 0 0
            , onClick (SetVote number)
            ]
            []
