module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Matrix
import Matrix.Extra exposing (prettyPrint)
import Maybe exposing (withDefault)
import Array


---- MODEL ----


type Pebble
    = Black
    | White


type alias Cell =
    { pebble : Maybe Pebble
    , noKill : Bool
    }


type alias Model =
    { board : Matrix.Matrix Cell
    }


init : String -> ( Model, Cmd Msg )
init path =
    let
        emptyCell =
            { pebble = Nothing, noKill = False }

        noKillEmptyCell =
            { emptyCell | noKill = True }

        middleRow =
            List.concat
                [ [ emptyCell ]
                , [ noKillEmptyCell ]
                , List.repeat 5 emptyCell
                , [ noKillEmptyCell ]
                , [ emptyCell ]
                ]

        _ =
            Debug.log "middleRow" middleRow
    in
        ( { board =
                Matrix.fromList
                    [ List.repeat 9 { pebble = Just Black, noKill = False }
                    , middleRow
                    , List.repeat 9 { pebble = Just White, noKill = False }
                    ]
                    |> withDefault Matrix.empty
          }
        , Cmd.none
        )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ model.board |> drawBoard ]
        , hr [] []
        , div
            []
            [ prettyPrint model.board ]
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
        , Html.Attributes.style
            [ if cell.noKill then
                ( "background-color", "#f77171" )
              else
                ( "background-color", "#fff" )
            , ( "width", "80px" )
            , ( "height", "80px" )
            , ( "border", "4px solid #000" )
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
            , div [ class "debug-pos" ]
                [ text <|
                    String.join " "
                        [ "("
                        , (toString x)
                        , ", "
                        , (toString y)
                        , ")"
                        ]
                ]
            ]
        ]


drawPebble pebble =
    case pebble of
        Black ->
            div [ class " center" ] [ i [ class "fa fa-circle fa-2x" ] [] ]

        White ->
            div [ class " center" ] [ i [ class "fa fa-circle-o fa-2x " ] [] ]



---- PROGRAM ----


main : Program String Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
