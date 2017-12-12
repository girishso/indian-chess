module App exposing (..)

import Dict exposing (..)
import Dict.Extra exposing (find)
import Html exposing (programWithFlags)
import Json.Decode as Decode exposing (field)
import Json.Encode as Encode exposing (..)
import Model exposing (..)
import Ports exposing (..)
import Utils exposing (..)
import View exposing (view)
import Window exposing (height, width)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnCellClick x y currentCell ->
            let
                gameState =
                    model.gameState

                ( newBoard, nextPlayer ) =
                    if (model.thisPlayer == gameState.currentPlayer) then
                        if isCurrentPlayersCell gameState currentCell then
                            -- select cell and calculate valid moves
                            ( calculateValidMoves gameState x y currentCell, gameState.currentPlayer )
                        else if
                            isAnyPebbleSelected gameState
                                && (currentCell.pebble == Zilch)
                                && (currentCell.state == ValidMove)
                        then
                            -- move/delete pebble
                            let
                                ( ( i, j ), selectedCell ) =
                                    (getPositionCellIfExists ( x, y ) (\key cell -> cell.state == Selected) gameState.board)
                            in
                                ( movePebble x y i j selectedCell currentCell gameState, togglePlayer gameState )
                        else
                            ( gameState.board, gameState.currentPlayer )
                    else
                        ( gameState.board, gameState.currentPlayer )

                newGameState =
                    { gameState | board = newBoard, currentPlayer = nextPlayer }
            in
                ( { model | gameState = newGameState }
                , Encode.encode 1 (gameStateEncoder newGameState) |> sendGameState
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

        SelectGameUrlInput ->
            ( model, focus "url_input" )

        ScreenSize { width, height } ->
            let
                _ =
                    Debug.log "ScreenSize: " ( width, height )
            in
                { model | screen = ( width, height ) } ! []


calculateValidMoves : GameState -> Int -> Int -> Cell -> Dict Position Cell
calculateValidMoves gameState x y currentCell =
    gameState.board
        |> Dict.map
            (\_ cell -> { cell | state = Normal })
        |> updateIfExists ( x, y ) (\cell -> { cell | state = Selected })
        |> Dict.map
            -- valid moves in immediate neighbours
            (\( i, j ) cell ->
                if isNeighbour i j x y && cell.pebble == Zilch then
                    { cell | state = ValidMove }
                else
                    cell
            )
        |> Dict.map
            -- possible kill positions
            (\( i, j ) cell ->
                if (cell.pebble == Zilch) && isNeighbourEnemyAndKillable i j x y gameState then
                    -- kill position empty? And is neighbour enemy and not in noKill cell?
                    { cell | state = ValidMove }
                else
                    cell
            )


isNeighbourEnemyAndKillable : Int -> Int -> Int -> Int -> GameState -> Bool
isNeighbourEnemyAndKillable i j x y gameState =
    ((i == x && j == y - 2) && isEnemyIsKillable (Dict.get ( x, (y - 1) ) gameState.board) gameState.currentPlayer)
        || ((i == x && j == y + 2) && isEnemyIsKillable (Dict.get ( x, (y + 1) ) gameState.board) gameState.currentPlayer)
        || ((i == x - 2 && j == y) && isEnemyIsKillable (Dict.get ( (x - 1), y ) gameState.board) gameState.currentPlayer)
        || ((i == x + 2 && j == y) && isEnemyIsKillable (Dict.get ( (x + 1), y ) gameState.board) gameState.currentPlayer)


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
                            c.pebble == Black

                        BlackPlayer ->
                            c.pebble == White
            )
        |> Maybe.withDefault False


togglePlayer model =
    if model.currentPlayer == WhitePlayer then
        BlackPlayer
    else
        WhitePlayer


movePebble : Int -> Int -> Int -> Int -> Cell -> Cell -> GameState -> Dict Position Cell
movePebble toX toY fromX fromY fromCell toCell gameState =
    let
        newBoard =
            if abs (fromX - toX) == 2 then
                killPebble ((fromX + toX) // 2) fromY gameState
            else if abs (fromY - toY) == 2 then
                killPebble fromX ((fromY + toY) // 2) gameState
            else
                gameState.board
    in
        newBoard
            |> updateIfExists ( toX, toY ) (\cell -> { toCell | pebble = fromCell.pebble, state = LastMoved })
            |> updateIfExists ( fromX, fromY ) (\cell -> { fromCell | pebble = Zilch, state = LastMoved })
            |> Dict.map
                (\_ cell ->
                    if cell.state == LastMoved then
                        cell
                    else
                        { cell | state = Normal }
                )


killPebble : Int -> Int -> GameState -> Dict Position Cell
killPebble x y gameState =
    case Dict.get ( x, y ) gameState.board of
        Just cell ->
            if cell.noKill then
                gameState.board
            else
                updateIfExists ( x, y ) (\cell -> { cell | pebble = Zilch }) gameState.board

        Nothing ->
            gameState.board


isAnyPebbleSelected : GameState -> Bool
isAnyPebbleSelected gameState =
    gameState.board
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


getPositionCellIfExists k f dict =
    dict
        |> Dict.Extra.find f
        |> Maybe.withDefault
            ( ( -1, -1 ), emptyCell )



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ gameStateChanged (GameStateChanged << Decode.decodeValue gameStateDecoder)
        , Window.resizes ScreenSize
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
