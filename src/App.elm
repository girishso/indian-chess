module App exposing (..)

import Array
import Html exposing (programWithFlags)
import Model exposing (..)
import Ports exposing (..)
import View exposing (view)
import Dict exposing (..)
import Dict.Extra exposing (find)
import Utils exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnCellClick x y currentCell ->
            let
                ( newBoard, nextPlayer ) =
                    if isCurrentPlayersCell model currentCell then
                        -- select cell and calculate valid moves
                        ( calculateValidMoves model x y currentCell, model.currentPlayer )
                    else if
                        isAnyPebbleSelected model
                            && (currentCell.pebble == Nothing)
                            && (currentCell.state == ValidMove)
                    then
                        -- move/delete pebble
                        let
                            ( ( i, j ), selectedCell ) =
                                (getPositionCellIfExists ( x, y ) (\key cell -> cell.state == Selected) model.board)
                        in
                            ( movePebble x y i j selectedCell currentCell model, togglePlayer model )
                    else
                        ( model.board, model.currentPlayer )
            in
                ( { model | board = newBoard, currentPlayer = nextPlayer }, Cmd.none )


getPositionCellIfExists k f dict =
    dict
        |> Dict.Extra.find f
        |> Maybe.withDefault
            ( ( -1, -1 ), emptyCell )


calculateValidMoves : Model -> Int -> Int -> Cell -> Dict Position Cell
calculateValidMoves model x y currentCell =
    model.board
        |> Dict.map (\_ cell -> { cell | state = Normal })
        |> updateIfExists ( x, y ) (\cell -> { cell | state = Selected })
        |> Dict.map
            -- valid moves in immediate neighbours
            (\( i, j ) cell ->
                if isNeighbour i j x y && cell.pebble == Nothing then
                    { cell | state = ValidMove }
                else
                    cell
            )
        |> Dict.map
            -- possible kill positions
            (\( i, j ) cell ->
                if (cell.pebble == Nothing) then
                    -- kill position empty?
                    if
                        -- is neighbour enemy and not in noKill cell?
                        ((i == x && j == y - 2) && isEnemyIsKillable (Dict.get ( x, (y - 1) ) model.board) model.currentPlayer)
                            || ((i == x && j == y + 2) && isEnemyIsKillable (Dict.get ( x, (y + 1) ) model.board) model.currentPlayer)
                            || ((i == x - 2 && j == y) && isEnemyIsKillable (Dict.get ( (x - 1), y ) model.board) model.currentPlayer)
                            || ((i == x + 2 && j == y) && isEnemyIsKillable (Dict.get ( (x + 1), y ) model.board) model.currentPlayer)
                    then
                        { cell | state = ValidMove }
                    else
                        cell
                else
                    cell
            )


isEnemyIsKillable : Maybe Cell -> Player -> Bool
isEnemyIsKillable cell currentPlayer =
    case cell of
        Just c ->
            if c.noKill then
                False
            else
                case currentPlayer of
                    WhitePlayer ->
                        case c.pebble of
                            Just pebble ->
                                pebble == Black

                            Nothing ->
                                False

                    BlackPlayer ->
                        case c.pebble of
                            Just pebble ->
                                pebble == White

                            Nothing ->
                                False

        Nothing ->
            False


togglePlayer model =
    if model.currentPlayer == WhitePlayer then
        BlackPlayer
    else
        WhitePlayer


movePebble : Int -> Int -> Int -> Int -> Cell -> Cell -> Model -> Dict Position Cell
movePebble toI toJ fromX fromY fromCell toCell model =
    let
        newBoard =
            if abs (fromX - toI) == 2 then
                killPebble ((fromX + toI) // 2) fromY model
            else if abs (fromY - toJ) == 2 then
                killPebble fromX ((fromY + toJ) // 2) model
            else
                model.board
    in
        newBoard
            |> updateIfExists ( toI, toJ ) (\cell -> { toCell | pebble = fromCell.pebble })
            |> updateIfExists ( fromX, fromY ) (\cell -> { fromCell | pebble = Nothing })
            |> Dict.map (\_ cell -> { cell | state = Normal })


killPebble : Int -> Int -> Model -> Dict Position Cell
killPebble x y model =
    let
        cell =
            Dict.get ( x, y ) model.board
    in
        case cell of
            Just cell ->
                if cell.noKill then
                    model.board
                else
                    updateIfExists ( x, y ) (\cell -> { cell | pebble = Nothing }) model.board

            Nothing ->
                model.board


isAnyPebbleSelected model =
    model.board
        |> filterValues (\c -> c.state == Selected)
        |> Dict.isEmpty
        |> not


isNeighbour i j x y =
    -- top
    (i == x - 1 && j == y)
        -- bottom
        || (i == x + 1 && j == y)
        -- left
        || (i == x && j == y - 1)
        -- right
        || (i == x && j == y + 1)



---- PROGRAM ----


main : Program String Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
