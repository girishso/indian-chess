module App exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Matrix
import Matrix.Extra exposing (prettyPrint)
import Maybe exposing (withDefault)
import Array


---- MODEL ----


type Pebble
    = Black
    | White


type State
    = Normal
    | Selected
    | ValidMove


type alias Cell =
    { pebble : Maybe Pebble
    , noKill : Bool
    , state : State
    }


type alias Model =
    { board : Matrix.Matrix Cell
    }


emptyCell =
    { pebble = Nothing, noKill = False, state = Normal }


noKillEmptyCell =
    { emptyCell | noKill = True }


whiteCell =
    { emptyCell | pebble = Just White }


blackCell =
    { emptyCell | pebble = Just Black }


init : String -> ( Model, Cmd Msg )
init path =
    let
        middleRow =
            List.concat
                [ [ emptyCell ]
                , [ noKillEmptyCell ]
                , List.repeat 5 emptyCell
                , [ noKillEmptyCell ]
                , [ emptyCell ]
                ]
    in
        ( { board =
                Matrix.fromList
                    [ List.repeat 9 blackCell
                    , middleRow
                    , List.repeat 9 whiteCell
                    ]
                    |> withDefault Matrix.empty
          }
        , Cmd.none
        )



---- UPDATE ----


type Msg
    = OnCellClick Int Int Cell


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnCellClick x y cell ->
            let
                newBoard =
                    Matrix.set y x ({ cell | state = Selected }) model.board
            in
                ( { model | board = newBoard }, Cmd.none )



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
        , class (toString cell.state |> String.toLower)
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
            [ onClick (OnCellClick x y cell) ]
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
