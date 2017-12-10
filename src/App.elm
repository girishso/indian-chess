module App exposing (..)

import Array
import Dict exposing (..)
import Dict.Extra exposing (find)
import Html exposing (programWithFlags)
import Html.Attributes exposing (value)
import Json.Decode as Decode exposing (field)
import Json.Encode as Encode exposing (..)
import Model exposing (..)
import Ports exposing (..)
import Utils exposing (..)
import View exposing (view)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnCellClick x y currentCell ->
            let
                ( newBoard, nextPlayer ) =
                    if (model.thisPlayer == model.gameState.currentPlayer) then
                        if isCurrentPlayersCell model currentCell then
                            -- select cell and calculate valid moves
                            ( calculateValidMoves model x y currentCell, model.gameState.currentPlayer )
                        else if
                            isAnyPebbleSelected model
                                && (currentCell.pebble == Nothing)
                                && (currentCell.state == ValidMove)
                        then
                            -- move/delete pebble
                            let
                                ( ( i, j ), selectedCell ) =
                                    (getPositionCellIfExists ( x, y ) (\key cell -> cell.state == Selected) model.gameState.board)
                            in
                                ( movePebble x y i j selectedCell currentCell model, togglePlayer model.gameState )
                        else
                            ( model.gameState.board, model.gameState.currentPlayer )
                    else
                        ( model.gameState.board, model.gameState.currentPlayer )

                gameState =
                    model.gameState

                newGameState =
                    { gameState | board = newBoard, currentPlayer = nextPlayer }

                newModel =
                    { model | gameState = newGameState }
            in
                ( newModel
                , Encode.encode 1 (modelEncoder newGameState) |> sendGameState
                )

        GameStateChanged json ->
            ( { model
                | gameState =
                    case (json) of
                        Ok value ->
                            value

                        Err error ->
                            let
                                _ =
                                    Debug.log "GameStateChanged err" error
                            in
                                model.gameState
              }
            , Cmd.none
            )

        NewGameCreated url ->
            ( { model | showGameUrl = False, gameUrl = url }, Cmd.none )

        SetThisPlayer player ->
            ( { model | thisPlayer = strToPlayer player, showGameUrl = False }, Cmd.none )

        CopyUrl ->
            ( model, copyUrl "url_input" )

        SelectGameUrlInput ->
            ( model, focus "url_input" )


getPositionCellIfExists k f dict =
    dict
        |> Dict.Extra.find f
        |> Maybe.withDefault
            ( ( -1, -1 ), emptyCell )


calculateValidMoves : Model -> Int -> Int -> Cell -> Dict Position Cell
calculateValidMoves model x y currentCell =
    model.gameState.board
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
                        ((i == x && j == y - 2) && isEnemyIsKillable (Dict.get ( x, (y - 1) ) model.gameState.board) model.gameState.currentPlayer)
                            || ((i == x && j == y + 2) && isEnemyIsKillable (Dict.get ( x, (y + 1) ) model.gameState.board) model.gameState.currentPlayer)
                            || ((i == x - 2 && j == y) && isEnemyIsKillable (Dict.get ( (x - 1), y ) model.gameState.board) model.gameState.currentPlayer)
                            || ((i == x + 2 && j == y) && isEnemyIsKillable (Dict.get ( (x + 1), y ) model.gameState.board) model.gameState.currentPlayer)
                    then
                        { cell | state = ValidMove }
                    else
                        cell
                else
                    cell
            )


isEnemyIsKillable : Maybe Cell -> Player -> Bool
isEnemyIsKillable cell currentPlayer =
    cell
        |> Maybe.map
            (\c ->
                if c.noKill then
                    False
                else
                    case currentPlayer of
                        WhitePlayer ->
                            c.pebble |> Maybe.map (\pebble -> pebble == Black) |> Maybe.withDefault False

                        BlackPlayer ->
                            c.pebble |> Maybe.map (\pebble -> pebble == White) |> Maybe.withDefault False
            )
        |> (Maybe.withDefault False)


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
                model.gameState.board
    in
        newBoard
            |> updateIfExists ( toI, toJ ) (\cell -> { toCell | pebble = fromCell.pebble })
            |> updateIfExists ( fromX, fromY ) (\cell -> { fromCell | pebble = Nothing })
            |> Dict.map (\_ cell -> { cell | state = Normal })


killPebble : Int -> Int -> Model -> Dict Position Cell
killPebble x y model =
    let
        cell =
            Dict.get ( x, y ) model.gameState.board
    in
        case cell of
            Just cell ->
                if cell.noKill then
                    model.gameState.board
                else
                    updateIfExists ( x, y ) (\cell -> { cell | pebble = Nothing }) model.gameState.board

            Nothing ->
                model.gameState.board


isAnyPebbleSelected model =
    model.gameState.board
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



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ gameStateChanged (GameStateChanged << Decode.decodeValue modelDecoder)
        , newSharedGameCreated NewGameCreated
        , setThisPlayer SetThisPlayer
        ]



---- PROGRAM ----


main : Program ( String, String ) Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
