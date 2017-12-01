module View exposing (view)

import Array
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Matrix
import Matrix.Extra exposing (prettyPrint)
import Model exposing (..)


view : Model -> Html Msg
view model =
    div [ class "container is-fluid" ]
        [ h1 [] [ text "Chathurvimshathi Koshtaka" ]
        , div [ class "columns" ]
            [ div [ class "column" ] []
            , div [ class "column", classIfCurrentPlayer model WhitePlayer "tdu" ] [ currentPlayerIcon model WhitePlayer, text " White player" ]
            , div [ class "column", classIfCurrentPlayer model BlackPlayer "tdu" ] [ currentPlayerIcon model BlackPlayer, text " Black player" ]
            , div [ class "column" ] []
            ]
        , div []
            [ model.board |> drawBoard ]
        , if isWin model then
            h3 [ class "winner" ]
                [ (case model.currentPlayer of
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


drawBoard : Matrix.Matrix Cell -> Html Msg
drawBoard board =
    let
        drawrow row_num =
            div
                [ class "row" ]
                (Matrix.getRow row_num board
                    |> Maybe.withDefault Array.empty
                    |> Array.indexedMap (drawCell row_num)
                    |> Array.toList
                )

        height =
            Matrix.height board
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
                    div [] []

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
        |> Matrix.filter (isCurrentPlayersCell model)
        |> Array.isEmpty
