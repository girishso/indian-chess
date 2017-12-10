module View exposing (view)

import Array
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import String exposing (dropRight)
import Utils exposing (..)


view : Model -> Html Msg
view model =
    section [ class "hero is-fullheight is-default is-bold main" ]
        [ div [ class "" ]
            [ div [ class "container has-text-centered" ]
                [ div [ class "columns" ]
                    [ div [ class "column  is-12-mobile is-2-tablet is-2-desktop" ]
                        [ div [ class "" ]
                            [ div [ class "is-size-5" ]
                                [ text "You are: "
                                , model.thisPlayer |> toString |> dropRight 6 |> text
                                , div [ class "column", classIfCurrentPlayer model WhitePlayer "tdu" ] [ currentPlayerIcon model WhitePlayer, text " White player" ]
                                , div [ class "column", classIfCurrentPlayer model BlackPlayer "tdu" ] [ currentPlayerIcon model BlackPlayer, text " Black player" ]
                                ]
                            ]
                        ]
                    , div [ class "column" ]
                        [ div []
                            [ model.gameState.board |> drawBoard ]
                        , if isWin model then
                            h3 [ class "winner is-size-3" ]
                                [ (case model.gameState.currentPlayer of
                                    WhitePlayer ->
                                        "Black is the Winner!"

                                    BlackPlayer ->
                                        "White is the Winner!"
                                  )
                                    |> text
                                ]
                          else
                            Html.text ""
                        ]
                    ]
                ]
            ]
        , div [ class "h100" ] []
        , div [ class "hero-foot" ]
            [ div [ class "container" ]
                [ div [ class "tabs is-centered" ]
                    [ ul []
                        [ li []
                            [ a [ href "http://cuberoot.in", target "_blank" ]
                                [ text " Cube Root Software" ]
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
                        [ text "Share this URL with your opponent:" ]
                    ]
                , section [ class "modal-card-body" ]
                    [ div [ class "content" ]
                        [ div [ class "field has-addons" ]
                            [ p [ class "control" ]
                                [ input
                                    [ value model.gameUrl
                                    , id "url_input"
                                    , class "input"
                                    , onClick SelectGameUrlInput
                                    ]
                                    []
                                ]
                            , p [ class "control" ]
                                [ a
                                    [ class "button is-light"
                                    , id "copy_url_btn"
                                    , attribute "data-clipboard-target" "#url_input"
                                    ]
                                    [ span [ class "icon is-small" ]
                                        [ i [ class "fa fa-clipboard" ] []
                                        ]
                                    , span [] [ text "Copy" ]
                                    ]
                                ]
                            ]
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



-- howToPlayPopup : Html Msg
-- howToPlayPopup =
--     div [ class "modal is-active" ]
--         [ div [ class "modal-background", onClick ToggleHowToPlay ]
--             []
--         , div [ class "modal-card" ]
--             [ header [ class "modal-card-head" ]
--                 [ p [ class "modal-card-title" ]
--                     [ text "How to play" ]
--                 , button [ attribute "aria-label" "close", class "delete", onClick ToggleHowToPlay ]
--                     []
--                 ]
--             , section [ class "modal-card-body" ]
--                 [ div [ class "content" ]
--                     [ p [] [ text "The game is played by 2 players with 8 coins each. Objective is to kill all the opponents' coins." ]
--                     , h3 []
--                         [ text "Rules:"
--                         ]
--                     , ol []
--                         [ li [] [ text "Players take alternate turns. White player starts the game." ]
--                         , li [] [ text "Player can move her coin one place; left, right, up or down. Provided the place is empty." ]
--                         , li [] [ text "Opponents' coin can be killed by jumping over it. Only for killing moving two places is allowed." ]
--                         , li [] [ text "Red places are no kill zones." ]
--                         , li [] [ text "Player wins when all the opponents' coins are killed or when opponent has no valid moves left." ]
--                         ]
--                     , h3 [] [ text "About the game" ]
--                     , p []
--                         [ text "This game is also known as"
--                         , strong [] [ text " Chathurvimshathi Koshtaka" ]
--                         ]
--                     , p [] [ text "This is a game drawn from an old book written in Sanskrit by Harikrishna, son of Venkatram in the late nineteenth century. " ]
--                     , p [] [ text "Chathurvimshathi Koshtaka means 24 boxes or squares in Sanskrit. This is a battlefield game where two players with eight coins each literally battle it out to gain control of enemy territory." ]
--                     ]
--                 ]
--             , footer [ class "modal-card-foot" ]
--                 [ div []
--                     [ text "Brought you by:"
--                     , a [ href "http://cuberoot.in", target "_blank" ] [ text " Cube Root Software" ]
--                     ]
--                 , div [ class "ml2" ]
--                     [ button [ class "button  is-success ", onClick ToggleHowToPlay ]
--                         [ text "OK" ]
--                     ]
--                 ]
--             ]
--         ]


currentPlayerIcon : Model -> Player -> Html msg
currentPlayerIcon model player =
    div [ class "w30" ]
        [ i [ classIfCurrentPlayer model player "fa fa-angle-double-right" ] []
        ]


classIfCurrentPlayer : Model -> Player -> String -> Attribute msg
classIfCurrentPlayer model player className =
    if model.gameState.currentPlayer == player then
        class className
    else
        class ""


drawBoard : Dict Position Cell -> Html Msg
drawBoard board =
    let
        drawrow row_num =
            div
                [ class "row" ]
                (List.range 0 7
                    |> List.map
                        (\x ->
                            Dict.get ( x, row_num ) board
                                |> Maybe.withDefault emptyCell
                                |> drawCell x row_num
                        )
                )

        height =
            2
    in
        List.range 0 height
            |> List.map drawrow
            |> div []


drawCell : Int -> Int -> Cell -> Html Msg
drawCell x y cell =
    div
        [ class "cell-container"
        , class (toString cell.state |> String.toLower)
        , onClick (OnCellClick x y cell)
        , Html.Attributes.style
            [ if cell.noKill then
                ( "background-color", "#f77171" )
              else
                ( "background-color", "#fff" )
            , ( "display", "inline-block" )
            ]
        ]
        [ Html.div
            []
            [ case cell.pebble of
                Just pebble ->
                    drawPebble pebble

                Nothing ->
                    Html.text ""

            -- , div [ class "debug-pos" ]
            --     [ text <|
            --         String.join " "
            --             [ "("
            --             , (toString x)
            --             , ", "
            --             , (toString y)
            --             , ")"
            --             ]
            --     ]
            ]
        ]


drawPebble : Pebble -> Html msg
drawPebble pebble =
    case pebble of
        Black ->
            div [ class "pebble center" ] [ i [ class "fa fa-circle fa-2x" ] [] ]

        White ->
            div [ class "pebble center" ] [ i [ class "fa fa-circle-o fa-2x" ] [] ]


isWin : Model -> Bool
isWin model =
    model.gameState.board
        |> filterValues (isCurrentPlayersCell model)
        |> Dict.isEmpty
