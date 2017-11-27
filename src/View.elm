module View exposing (view)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Matrix.Extra exposing (prettyPrint)
import Model exposing (..)
import Array
import Matrix


view : Model -> Html Msg
view model =
    div [ class "container is-fluid" ]
        [ h2 [] [ text "Chathurvimshanthi Koshtakam" ]
        , h3 []
            [ (case model.currentPlayer of
                WhitePlayer ->
                    "White"

                BlackPlayer ->
                    "Black"
              )
                |> text
            ]
        , div []
            [ model.board |> drawBoard ]
        , div
            [ class "white-pebble", Html.Attributes.style [ ( "width", "500px" ), ( "height", "500px" ) ] ]
            []
        ]


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
            , ( "width", "80px" )
            , ( "height", "80px" )
            , ( "margin", "0px" )
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
            div [ class "center black-pebble", Html.Attributes.style [ ( "width", "80px" ), ( "height", "80px" ) ] ] []

        White ->
            div [ class "center white-pebble", Html.Attributes.style [ ( "width", "80px" ), ( "height", "80px" ) ] ] []
