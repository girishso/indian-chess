port module Welcome exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    {}


type Msg
    = NewSharedGame


port createNewGame : () -> Cmd msg


init str =
    ( Model, Cmd.none )


update msg model =
    case msg of
        NewSharedGame ->
            ( model, createNewGame () )


view model =
    section [ class "hero is-fullheight is-default is-bold" ]
        [ div [ class "hero-body" ]
            [ div [ class "container has-text-centered" ]
                [ div [ class "columns is-vcentered" ]
                    [ div [ class "column is-3" ]
                        [ figure []
                            [ img [ class "game_img", alt "game image", src "/game.png" ]
                                []
                            ]
                        ]
                    , div [ class "column is-6 is-offset-1" ]
                        [ h1 [ class "title is-2" ]
                            [ text "Indian Chess" ]
                        , h2 [ class "subtitle is-4" ]
                            [ text "" ]
                        , p [] [ text "This is a battlefield game where two players with eight coins each literally battle it out to gain control of enemy territory." ]
                        , br []
                            []
                        , p [ class "has-text-centered" ]
                            [ -- a [ class "button is-medium is-info is-outlined", rel "noreferrer" ]
                              --     [ text "Play on the same device" ]
                              br [] []
                            , br [] []
                            , a [ class "button is-medium is-info is-outlined", onClick NewSharedGame ]
                                [ text "Play on two devices" ]
                            ]
                        ]
                    ]
                ]
            ]
        , hr [] []
        , div [ class "hero-foot" ]
            [ div
                [ class "container" ]
                [ div [ class "content" ]
                    [ h3 []
                        [ text "Rules:"
                        ]
                    , p [] [ text "The game is played by 2 players with 8 coins each. Objective is to kill all the opponents' coins." ]
                    , ol []
                        [ li [] [ text "Players take alternate turns. White player starts the game." ]
                        , li [] [ text "Player can move her coin one place; left, right, up or down. Provided the place is empty." ]
                        , li [] [ text "Opponents' coin can be killed by jumping over it. Only for killing moving two places is allowed." ]
                        , li [] [ text "Red places are no kill zones." ]
                        , li [] [ text "Player wins when all the opponents' coins are killed or when opponent has no valid moves left." ]
                        ]
                    , hr [] []
                    , h3 [] [ text "About the game" ]
                    , p []
                        [ text "This game is also known as"
                        , strong [] [ text " Chathurvimshathi Koshtaka" ]
                        ]
                    , p [] [ text "This is a game drawn from an old book written in Sanskrit by Harikrishna, son of Venkatram in the late nineteenth century. " ]
                    , p [] [ text "Chathurvimshathi Koshtaka means 24 boxes or squares in Sanskrit. This is a battlefield game where two players with eight coins each literally battle it out to gain control of enemy territory." ]
                    , hr [ class "space" ] []
                    ]
                ]
            ]
        , div [ class "hero-foot" ]
            [ div [ class "container" ]
                [ div [ class "tabs is-centered" ]
                    [ ul []
                        [ li []
                            [ text "Source code:" ]
                        , li []
                            [ a [ href "https://github.com/girishso/indian-chess" ]
                                [ span [] [ text "Github " ]
                                , span [ class "icon" ]
                                    [ i [ class "fa fa-github" ] []
                                    ]
                                ]
                            ]
                        , li []
                            [ text "Brought you by:" ]
                        , li []
                            [ a [ href "http://cuberoot.in", target "_blank" ] [ text " Cube Root Software" ]
                            ]
                        ]
                    ]
                ]
            ]
        , hr [ class "space" ] []
        ]


main : Program String Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
