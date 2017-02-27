port module Main exposing (..)

import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (..)
import Navigation exposing (Location, modifyUrl)
import AFrame exposing (scene)
import Geometry exposing (..)


port updateVotes : (List Vote -> msg) -> Sub msg


port setUser : String -> Cmd msg


port setVote : Int -> Cmd msg


type alias Model =
    { votes : List Vote
    , name : Maybe String
    , vote : Maybe Int
    , voted : Bool
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
    | SetVote
    | SelectCard Int
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
        ( Model [] name Nothing False "" False, cmd )


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

        SetVote ->
            let
                command =
                    case model.vote of
                        Nothing ->
                            Cmd.none

                        Just vote ->
                            setVote vote
            in
                ( { model | voted = True }, command )

        SelectCard vote ->
            ( { model | vote = Just vote, voted = False }, Cmd.none )

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


view : Model -> Html Msg
view model =
    case model.name of
        Nothing ->
            htmlView model

        Just _ ->
            aframeScene model


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


aframeScene : Model -> Html Msg
aframeScene model =
    scene
        []
        [ (allCards model.vote voteValues SelectCard)
        , (allPlayers (List.map (\v -> v.user) model.votes))
        , (allVotes (List.map (\v -> v.vote) model.votes) model.revealed)
        , Geometry.table
        , cardSelection model.voted model.vote SetVote
        , sceneCamera
        , sceneAssets
        , sceneSky
        ]


htmlView : Model -> Html Msg
htmlView model =
    div [ id "wrap" ]
        [ div [ id "inputform" ]
            [ div [ id "container" ] [ img [ HA.src "login.png" ] [] ]
            , div [ id "formcontainer" ]
                [ input [ HA.type_ "text", placeholder "Player Name", onInput Name ] []
                , button [ HA.disabled (model.inputName == ""), onClick SetName ] [ Html.text "Go!" ]
                ]
            ]
        ]


displayVote : Vote -> Html msg
displayVote vote =
    div []
        [ div [ class "vote" ] [ Html.text (vote.user ++ ": " ++ (toString vote.vote)) ]
        ]


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
