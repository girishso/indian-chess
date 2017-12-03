module View exposing (view)

import Array
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Dict exposing (..)
import Utils exposing (..)


view : Model -> Html Msg
view model =
    section [ class "hero is-fullheight is-default is-bold" ]
        [ div [ class "hero-head" ]
            [ nav [ class "navbar" ]
                [ div [ class "container" ]
                    [ div [ class "navbar-brand" ]
                        [ a [ class "navbar-item", href "../", rel "noreferrer" ]
                            [ img [ alt "Logo", src "../images/bulma.png" ]
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
                            [ a [ class "navbar-item is-active", rel "noreferrer" ]
                                [ text "Home            " ]
                            , a [ class "navbar-item", rel "noreferrer" ]
                                [ text "Examples            " ]
                            , a [ class "navbar-item", rel "noreferrer" ]
                                [ text "Features            " ]
                            , a [ class "navbar-item", rel "noreferrer" ]
                                [ text "Team            " ]
                            , a [ class "navbar-item", rel "noreferrer" ]
                                [ text "Help            " ]
                            ]
                        ]
                    ]
                ]
            ]
        , div [ class "" ]
            [ div [ class "container has-text-centered" ]
                [ div [ class "columns" ]
                    [ div [ class "column" ]
                        [ div [ class "is-size-5" ] [ currentPlayerIcon model WhitePlayer, text " White player" ]
                        , div [ class "is-size-5" ] [ currentPlayerIcon model BlackPlayer, text " Black player" ]
                        , div []
                            [ model.board |> drawBoard ]
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
        ]



-- div [ class "container is-fluid" ]
--     [ h1 [ class "is-size-1" ] [ text "Chathurvimshathi Koshtaka" ]
--     , div [ class "columns" ]
--         []
--     , div [ class "columns is-desktop is-centered " ]
--         [ div [ class "column tablet-only" ] [ text "tablet-only" ]
--         , div [ class "column" ]
--             [ div [ class "is-size-5" ] [ currentPlayerIcon model WhitePlayer, text " White player" ]
--             , div [ class "is-size-5" ] [ currentPlayerIcon model BlackPlayer, text " Black player" ]
--             ]
--         , div [ class "column is-9 " ]
--             [ div []
--                 [ model.board |> drawBoard ]
--             ]
--         ]
--     , if isWin model then
--         h3 [ class "winner" ]
--             [ (case model.currentPlayer of
--                 WhitePlayer ->
--                     "Black is the Winner!"
--
--                 BlackPlayer ->
--                     "White is the Winner!"
--               )
--                 |> text
--             ]
--       else
--         Html.text ""
--     , div [ class "h100" ] []
--     ]


currentPlayerIcon : Model -> Player -> Html msg
currentPlayerIcon model player =
    div [ class "w30" ]
        [ i [ classIfCurrentPlayer model player "fa fa-angle-double-right" ] []
        ]


classIfCurrentPlayer : Model -> Player -> String -> Attribute msg
classIfCurrentPlayer model player className =
    if model.currentPlayer == player then
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
    model.board
        |> filterValues (isCurrentPlayersCell model)
        |> Dict.isEmpty
