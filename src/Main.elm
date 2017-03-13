module Main exposing (..)

import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (..)
import Navigation exposing (Location, modifyUrl)
import AFrame exposing (scene)
import Geometry exposing (..)
import Firebase
import Firebase.Database
import Firebase.Database.Types
import Firebase.Database.Reference
import Firebase.Database.Snapshot
import Firebase.Database.OnDisconnect
import Task
import Firebase.Errors exposing (Error)
import Firebase.Authentication
import Firebase.Authentication.User
import Firebase.Authentication.Types exposing (Auth, User)
import Json.Encode
import Json.Decode exposing (Decoder, field, map2, int, string, keyValuePairs)


type alias Model =
    { votes : List ( String, Vote )
    , name : Maybe String
    , vote : Maybe Int
    , voted : Bool
    , inputName : String
    , revealed : Bool
    , app : Firebase.App
    , db : Firebase.Database.Types.Database
    , myvote : Maybe Firebase.Database.Types.Reference
    , user : Maybe User
    }


type alias Vote =
    { name : String
    , vote : Int
    }


type Msg
    = SetVote
    | SelectCard Int
    | UrlChange Location
    | Name String
    | SetName
    | GameUpdate Firebase.Database.Types.Snapshot
    | SignedIn (Result Error User)
    | SetRoot (Result Error ())
    | DisconnectSet (Result Error ())


voteValues : List number
voteValues =
    [ 0, 1, 2, 3, 5, 8, 13 ]


type alias Config =
    { apiKey : String
    , databaseURL : String
    , authDomain : String
    , storageBucket : String
    , messagingSenderId : String
    }


init : Config -> Location -> ( Model, Cmd Msg )
init flags location =
    let
        ( name, id ) =
            getIdFrom location.search

        _ =
            Debug.log "Flags" flags

        app : Firebase.App
        app =
            Firebase.init
                { apiKey = flags.apiKey
                , databaseURL = flags.databaseURL
                , authDomain = flags.authDomain
                , storageBucket = flags.storageBucket
                , messagingSenderId = flags.messagingSenderId
                }

        {-
           It's not necessary to store the database, but it will make it easier
           since all your database interactions are going to either be in `update`
           or `subscriptions`, and both have access to your model.
        -}
        db : Firebase.Database.Types.Database
        db =
            Firebase.Database.init app

        model =
            Model [] name Nothing False "" False app db Nothing Nothing

        cmd =
            case name of
                Nothing ->
                    Cmd.none

                Just name ->
                    setUser model
    in
        ( model, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetVote ->
            let
                command =
                    case model.vote of
                        Nothing ->
                            Cmd.none

                        Just voteval ->
                            case model.myvote of
                                Nothing ->
                                    Cmd.none

                                Just ref ->
                                    let
                                        vote =
                                            Vote (Maybe.withDefault "" model.name) voteval
                                    in
                                        setVote vote ref
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
                            setUser model
            in
                { model | name = name } ! [ cmd ]

        Name name ->
            { model | inputName = name } ! []

        SetName ->
            model ! [ modifyUrl ("?name=" ++ model.inputName) ]

        GameUpdate snapshot ->
            let
                decoded =
                    Json.Decode.decodeValue jsonDecodeVoteList (Firebase.Database.Snapshot.value snapshot)

                votes =
                    case decoded of
                        Err err ->
                            []

                        Ok votes ->
                            votes

                _ =
                    Debug.log "All Votes" votes

                allVoted =
                    case model.vote of
                        Nothing ->
                            False

                        Just _ ->
                            (List.all (\( k, v ) -> v.vote /= -1) votes)
            in
                { model | votes = votes, revealed = allVoted } ! []

        SignedIn (Ok user) ->
            let
                userID =
                    Firebase.Authentication.User.uid user

                _ =
                    Debug.log "User" userID

                myvote =
                    model.db |> Firebase.Database.ref (Just ("votes/" ++ userID))

                onDis =
                    myvote
                        |> Firebase.Database.Reference.onDisconnect
                        |> Firebase.Database.OnDisconnect.set Json.Encode.null
                        |> Task.attempt DisconnectSet

                name =
                    Maybe.withDefault "Unknown" model.name

                vote =
                    Vote name -1

                cmd =
                    setVote vote myvote
            in
                { model | user = Just user, myvote = Just myvote } ! [ cmd, onDis ]

        SignedIn (Err err) ->
            model ! []

        SetRoot (Err err) ->
            let
                _ =
                    Debug.log "Error" err
            in
                model ! []

        SetRoot (Ok ()) ->
            let
                _ =
                    Debug.log "OK" ""
            in
                model ! []

        DisconnectSet (Err err) ->
            let
                _ =
                    Debug.log "Error" err
            in
                model ! []

        DisconnectSet (Ok _) ->
            model ! []


jsonEncode : Vote -> Json.Encode.Value
jsonEncode vote =
    Json.Encode.object [ ( "user", Json.Encode.string vote.name ), ( "vote", Json.Encode.int vote.vote ) ]


jsonDecodeVote : Decoder Vote
jsonDecodeVote =
    map2 Vote
        (field "user" string)
        (field "vote" int)


jsonDecodeVoteList : Decoder (List ( String, Vote ))
jsonDecodeVoteList =
    keyValuePairs jsonDecodeVote


setVote : Vote -> Firebase.Database.Types.Reference -> Cmd Msg
setVote vote ref =
    let
        _ =
            Debug.log "Set Vote" vote
    in
        Task.attempt SetRoot (Firebase.Database.Reference.set (jsonEncode vote) ref)


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        ref =
            model.db |> Firebase.Database.ref (Just "votes")
    in
        Firebase.Database.Reference.on "value" ref GameUpdate


view : Model -> Html Msg
view model =
    case model.name of
        Nothing ->
            htmlView model

        Just _ ->
            aframeScene model


main : Program Config Model Msg
main =
    Navigation.programWithFlags UrlChange
        { init = init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


setUser : Model -> Cmd Msg
setUser model =
    let
        auth : Auth
        auth =
            model.app |> Firebase.Authentication.init
    in
        Task.attempt SignedIn (Firebase.Authentication.signInAnonymously auth)


aframeScene : Model -> Html Msg
aframeScene model =
    let
        filteredPlayers =
            case model.user of
                Nothing ->
                    model.votes

                Just user ->
                    List.filter (\( k, v ) -> k /= (Firebase.Authentication.User.uid user)) model.votes
    in
        scene
            []
            [ (allCards model.vote voteValues SelectCard)
            , (allPlayers (List.map (\( k, v ) -> v.name) filteredPlayers))
            , (allVotes (List.map (\( k, v ) -> v.vote) filteredPlayers) model.revealed)
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
        [ div [ class "vote" ] [ Html.text (vote.name ++ ": " ++ (toString vote.vote)) ]
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
