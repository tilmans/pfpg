port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


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


voteValues =
    [ 0, 1, 2, 3, 5, 8, 13 ]


init : ( Model, Cmd Msg )
init =
    ( Model [] "Jack", Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        VotesUpdated votes ->
            { model | votes = votes } ! []

        SetVote vote ->
            ( model, setVote (Vote model.name vote) )


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
    Html.program
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }
