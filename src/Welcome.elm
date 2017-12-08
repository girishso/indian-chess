port module Welcome exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { showGameUrl : Bool, gameUrl : String }


type Msg
    = NewSharedGame
    | NewGameCreated String


port newSharedGameCreated : (String -> msg) -> Sub msg


port createNewGame : () -> Cmd msg


init str =
    ( Model False "", Cmd.none )


update msg model =
    case msg of
        NewSharedGame ->
            ( model, createNewGame () )

        NewGameCreated url ->
            let
                _ =
                    Debug.log "NewGameCreatedxxx" url
            in
                ( { model | showGameUrl = True, gameUrl = url }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    newSharedGameCreated NewGameCreated


view model =
    section [ class "hero is-fullheight is-default is-bold" ]
        [ div [ class "hero-head" ]
            [ nav [ class "navbar" ]
                [ div [ class "container" ]
                    [ div [ class "navbar-brand" ]
                        [ a [ class "navbar-item", href "../", rel "noreferrer" ]
                            [ img [ alt "Indian Chess" ]
                                []
                            ]
                        , span [ class "navbar-burger burger", attribute "data-target" "navbarMenu" ]
                            [ span []
                                []
                            , span []
                                []
                            , span []
                                []
                            ]
                        ]
                    , div [ class "navbar-menu", id "navbarMenu" ]
                        [ div [ class "navbar-end" ]
                            []
                        ]
                    ]
                ]
            ]
        , div [ class "hero-body" ]
            [ div [ class "container has-text-centered" ]
                [ div [ class "columns is-vcentered" ]
                    [ div [ class "column is-5" ]
                        [ figure [ class "image is-4by3" ]
                            [ img [ alt "Description", src "http://placehold.it/800x600" ]
                                []
                            ]
                        ]
                    , div [ class "column is-6 is-offset-1" ]
                        [ h1 [ class "title is-2" ]
                            [ text "Indian Chess" ]
                        , h2 [ class "subtitle is-4" ]
                            [ text "Chathurvimshathi Koshtaka" ]
                        , p [] [ text "This is a battlefield game where two players with eight coins each literally battle it out to gain control of enemy territory." ]
                        , br []
                            []
                        , p [ class "has-text-centered" ]
                            [ a [ class "button is-medium is-info is-outlined", rel "noreferrer" ]
                                [ text "Play on the same device" ]
                            , br [] []
                            , br [] []
                            , a [ class "button is-medium is-info is-outlined", onClick NewSharedGame ]
                                [ text "Play on two devices" ]
                            ]
                        ]
                    ]
                ]
            ]
        , div [ class "hero-foot" ]
            [ div [ class "container" ]
                [ div [ class "tabs is-centered" ]
                    [ ul []
                        [ li []
                            [ a [ rel "noreferrer" ]
                                [ text "And this at the bottom" ]
                            ]
                        ]
                    ]
                ]
            ]
        , gameUrlPopup model
        ]


gameUrlPopup : Model -> Html Msg
gameUrlPopup model =
    if not model.showGameUrl then
        Html.text ""
    else
        div [ class "modal is-active" ]
            [ div [ class "modal-background" ]
                []
            , div [ class "modal-card" ]
                [ header [ class "modal-card-head" ]
                    [ p [ class "modal-card-title" ]
                        [ text "Game url to share..." ]
                    ]
                , section [ class "modal-card-body" ]
                    [ div [ class "content" ]
                        [ a [ href model.gameUrl, target "_blank" ] [ text model.gameUrl ]
                        ]
                    ]
                , footer [ class "modal-card-foot" ]
                    [ div []
                        [ text "Brought you by:"
                        , a [ href "http://cuberoot.in", target "_blank" ] [ text " Cube Root Software" ]
                        ]
                    ]
                ]
            ]


main : Program String Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
