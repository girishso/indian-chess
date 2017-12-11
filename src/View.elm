module View exposing (view)

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
        [ div [ class "container has-text-centered" ]
            [ div [ id "status-wrapper" ]
                [ ul [ class "is-size-5", id "status-bar" ]
                    [ li [ class "you-are" ]
                        [ text "You are: "
                        , model.thisPlayer |> toString |> dropRight 6 |> text
                        ]
                    , li [ class "", classIfCurrentPlayer model WhitePlayer "tdu" ] [ currentPlayerIcon model WhitePlayer, text " White player" ]
                    , li [ class "", classIfCurrentPlayer model BlackPlayer "tdu" ] [ currentPlayerIcon model BlackPlayer, text " Black player" ]
                    ]
                ]
            , div []
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
        , div [ class "h100" ] []
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
                        [ text "Brought to you by:"
                        , a [ href "http://cuberoot.in", target "_blank" ] [ text " Cube Root Software" ]
                        ]
                    ]
                ]
            ]


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
        , class
            (if cell.noKill then
                "noKill"
             else
                ""
            )
        , onClick (OnCellClick x y cell)
        , Html.Attributes.style
            [ ( "display", "inline-block" )
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
        |> filterValues (isCurrentPlayersCell model.gameState)
        |> Dict.isEmpty
