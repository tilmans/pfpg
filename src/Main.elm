port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation exposing (Location)


port updateVotes : (List Vote -> msg) -> Sub msg


port setVote : Vote -> Cmd msg


type alias Model =
    { votes : List Vote
    , name : String
    }


type alias Vote =
    { user : String
    , vote : Int
    }


type Msg
    = VotesUpdated (List Vote)
    | SetVote Int
    | UrlChange Location


voteValues =
    [ 0, 1, 2, 3, 5, 8, 13 ]


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( name, id ) =
            getIdFrom location.search
    in
        ( Model [] (Maybe.withDefault "Default" name), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        VotesUpdated votes ->
            { model | votes = votes } ! []

        SetVote vote ->
            ( model, setVote (Vote model.name vote) )

        UrlChange location ->
            let
                ( name, id ) =
                    getIdFrom location.search
            in
                { model | name = (Maybe.withDefault "Default" name) } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    updateVotes VotesUpdated


displayVote vote =
    div []
        [ div [ class "vote" ] [ text (vote.user ++ ": " ++ (toString vote.vote)) ]
        ]


displayCard value =
    span [ class "card", onClick (SetVote value) ] [ text (toString value) ]


view : Model -> Html Msg
view model =
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
