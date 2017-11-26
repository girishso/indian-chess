module App exposing (..)

import Array
import Html exposing (programWithFlags)
import Matrix
import Matrix.Extra
import Model exposing (..)
import View exposing (view)


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
                                getSelectedPebbleXY model.board
                        in
                            ( movePebble x y i j selectedCell currentCell model, togglePlayer model )
                    else
                        ( model.board, model.currentPlayer )
            in
                ( { model | board = newBoard, currentPlayer = nextPlayer }, Cmd.none )


calculateValidMoves : Model -> Int -> Int -> Cell -> Matrix.Matrix Cell
calculateValidMoves model x y currentCell =
    model.board
        |> Matrix.map (\cell -> { cell | state = Normal })
        |> Matrix.set y x ({ currentCell | state = Selected })
        |> Matrix.indexedMap
            -- valid moves in immediate neighbours
            (\j i cell ->
                if isNeighbour i j x y && cell.pebble == Nothing then
                    { cell | state = ValidMove }
                else
                    cell
            )
        |> Matrix.indexedMap
            -- possible kill positions
            (\j i cell ->
                if (cell.pebble == Nothing) then
                    -- kill position empty?
                    if
                        -- is neighbour enemy and not in noKill cell?
                        ((i == x && j == y - 2) && isEnemyIsKillable (Matrix.get (y - 1) x model.board) model.currentPlayer)
                            || ((i == x && j == y + 2) && isEnemyIsKillable (Matrix.get (y + 1) x model.board) model.currentPlayer)
                            || ((i == x - 2 && j == y) && isEnemyIsKillable (Matrix.get y (x - 1) model.board) model.currentPlayer)
                            || ((i == x + 2 && j == y) && isEnemyIsKillable (Matrix.get y (x + 1) model.board) model.currentPlayer)
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


getSelectedPebbleXY : Matrix.Matrix Cell -> ( ( Int, Int ), Cell )
getSelectedPebbleXY board =
    let
        selectedCells =
            Matrix.toIndexedArray board
                |> Array.filter
                    (\( ( y, x ), cell ) ->
                        cell.state == Selected
                    )
    in
        case Array.get 0 selectedCells of
            Just ( ( y, x ), cell ) ->
                ( ( y, x ), cell )

            Nothing ->
                ( ( -1, -1 ), emptyCell )


movePebble : Int -> Int -> Int -> Int -> Cell -> Cell -> Model -> Matrix.Matrix Cell
movePebble toI toJ fromX fromY fromCell toCell model =
    let
        newBoard =
            if abs (fromY - toI) == 2 then
                killPebble ((fromY + toI) // 2) fromX model
            else if abs (fromX - toJ) == 2 then
                killPebble fromY ((fromX + toJ) // 2) model
            else
                model.board
    in
        newBoard
            |> Matrix.set toJ toI ({ toCell | pebble = fromCell.pebble })
            |> Matrix.set fromX fromY ({ fromCell | pebble = Nothing })
            |> Matrix.map (\cell -> { cell | state = Normal })


killPebble : Int -> Int -> Model -> Matrix.Matrix Cell
killPebble x y model =
    let
        cell =
            Matrix.get y x model.board
    in
        case cell of
            Just cell ->
                if cell.noKill then
                    model.board
                else
                    Matrix.set y x ({ cell | pebble = Nothing }) model.board

            Nothing ->
                model.board


isAnyPebbleSelected model =
    model.board
        |> Matrix.filter (\c -> c.state == Selected)
        |> Array.isEmpty
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


isCurrentPlayersCell : Model -> Cell -> Bool
isCurrentPlayersCell model cell =
    case model.currentPlayer of
        WhitePlayer ->
            case cell.pebble of
                Just pebble ->
                    pebble == White

                Nothing ->
                    False

        BlackPlayer ->
            case cell.pebble of
                Just pebble ->
                    pebble == Black

                Nothing ->
                    False



---- PROGRAM ----


main : Program String Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
