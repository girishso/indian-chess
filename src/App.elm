module App exposing (..)

import Html exposing (programWithFlags)
import Matrix
import Matrix.Extra
import View exposing (view)
import Model exposing (..)
import Array


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnCellClick x y currentCell ->
            let
                ( newBoard, nextPlayer ) =
                    if isCurrentPlayersCell model currentCell then
                        ( model.board
                            |> Matrix.map (\cell -> { cell | state = Normal })
                            |> Matrix.set y x ({ currentCell | state = Selected })
                            |> Matrix.indexedMap
                                (\j i cell ->
                                    if isNeighbour i j x y && cell.pebble == Nothing then
                                        { cell | state = ValidMove }
                                    else
                                        cell
                                )
                            |> Matrix.indexedMap
                                (\j i cell ->
                                    if (cell.pebble == Nothing) then
                                        -- kill position empty?
                                        if
                                            -- is neighbour enemy?
                                            ((i == x && j == y - 2) && isEnemy (Matrix.get (y - 1) x model.board) model.currentPlayer)
                                                || ((i == x && j == y + 2) && isEnemy (Matrix.get (y + 1) x model.board) model.currentPlayer)
                                                || ((i == x - 2 && j == y) && isEnemy (Matrix.get y (x - 1) model.board) model.currentPlayer)
                                                || ((i == x + 2 && j == y) && isEnemy (Matrix.get y (x + 1) model.board) model.currentPlayer)
                                        then
                                            { cell | state = ValidMove }
                                        else
                                            cell
                                    else
                                        cell
                                )
                        , model.currentPlayer
                        )
                    else if
                        isAnyPebbleSelected model
                            && (currentCell.pebble == Nothing)
                            && (currentCell.state == ValidMove)
                    then
                        let
                            -- _ =
                            --     Debug.log "ix array: " (toString (Matrix.toIndexedArray model.board))
                            ( ( i, j ), selectedCell ) =
                                getSelectedPebbleXY model.board
                        in
                            ( movePebble x y i j selectedCell currentCell model, togglePlayer model )
                    else
                        ( model.board, model.currentPlayer )
            in
                ( { model | board = newBoard, currentPlayer = nextPlayer }, Cmd.none )


isEnemy : Maybe Cell -> Player -> Bool
isEnemy cell currentPlayer =
    case cell of
        Just c ->
            case currentPlayer of
                WhitePlayer ->
                    case c.pebble of
                        Just p ->
                            p == Black

                        Nothing ->
                            False

                BlackPlayer ->
                    case c.pebble of
                        Just p ->
                            p == White

                        Nothing ->
                            False

        Nothing ->
            False


togglePlayer model =
    if model.currentPlayer == WhitePlayer then
        BlackPlayer
    else
        WhitePlayer


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


movePebble to_i to_j from_x from_y from_cell to_cell model =
    let
        _ =
            Debug.log "movePebble: " ( to_i, to_j, from_y, from_x )
    in
        model.board
            |> Matrix.set to_j to_i ({ to_cell | pebble = from_cell.pebble })
            |> Matrix.set from_x from_y ({ from_cell | pebble = Nothing })
            |> Matrix.map (\cell -> { cell | state = Normal })


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
