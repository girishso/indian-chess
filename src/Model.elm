module Model exposing (..)

import Dict exposing (..)
import Json.Decode as Decode exposing (field)
import Json.Encode as Encode exposing (..)
import Window
import Task


type Pebble
    = Black
    | White


type CellState
    = Normal
    | Selected
    | ValidMove
    | LastMoved


type alias Cell =
    { pebble : Maybe Pebble
    , noKill : Bool
    , state : CellState
    }


type Player
    = WhitePlayer
    | BlackPlayer


type alias Position =
    ( Int, Int )


type alias Model =
    { gameState : GameState
    , showGameUrl : Bool
    , gameUrl : String
    , thisPlayer : Player
    , screen : ( Int, Int )
    }


type alias GameState =
    { board : Dict.Dict Position Cell
    , currentPlayer : Player
    }


type Msg
    = OnCellClick Int Int Cell
    | GameStateChanged (Result String GameState)
    | NewGameCreated String
    | SetThisPlayer String
    | SelectGameUrlInput
    | ScreenSize Window.Size



-- required for encode/decode to json


type alias CellWrapper =
    { pos : Position, cell : Cell }


emptyCell : Cell
emptyCell =
    { pebble = Nothing, noKill = False, state = Normal }


noKillEmptyCell : Cell
noKillEmptyCell =
    { emptyCell | noKill = True }


whiteCell : Cell
whiteCell =
    { emptyCell | pebble = Just White }


blackCell : Cell
blackCell =
    { emptyCell | pebble = Just Black }


init : ( String, String ) -> ( Model, Cmd Msg )
init ( gameUrl, player ) =
    let
        middleRow =
            List.concat
                [ repeatDict 0 0 1 emptyCell
                , repeatDict 1 1 1 noKillEmptyCell
                , repeatDict 2 5 1 emptyCell
                , repeatDict 6 6 1 noKillEmptyCell
                , repeatDict 7 7 1 emptyCell
                ]

        board =
            [ repeatDict 0 7 0 blackCell
            , middleRow
            , repeatDict 0 7 2 whiteCell
            ]
                |> List.concat
                |> Dict.fromList
    in
        ( { gameState =
                { board = board
                , currentPlayer = WhitePlayer
                }
          , showGameUrl =
                if String.isEmpty gameUrl then
                    -- same device game
                    False
                else
                    True
          , gameUrl = gameUrl
          , thisPlayer = strToPlayer player
          , screen = ( 800, 600 )
          }
        , getScreenSize
        )


getScreenSize : Cmd Msg
getScreenSize =
    Task.perform ScreenSize (Window.size)


strToPlayer : String -> Player
strToPlayer player =
    if player == "WhitePlayer" then
        WhitePlayer
    else
        BlackPlayer


repeatDict : Int -> Int -> Int -> Cell -> List ( Position, Cell )
repeatDict startX uptoX y cell =
    List.range startX uptoX
        |> List.map (\x -> ( ( x, y ), cell ))


isCurrentPlayersCell : GameState -> Cell -> Bool
isCurrentPlayersCell gameState cell =
    case gameState.currentPlayer of
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



-- Encoders and Decoders


gameStateDecoder : Decode.Decoder GameState
gameStateDecoder =
    Decode.map2 GameState
        (field "board" boardDecoder)
        (field "currentPlayer" playerDecoder)


gameStateEncoder : GameState -> Encode.Value
gameStateEncoder v =
    Encode.object
        [ ( "board", boardEncoder v.board )
        , ( "currentPlayer", playerEncoder v.currentPlayer )
        ]


playerEncoder : Player -> Encode.Value
playerEncoder v =
    case v of
        WhitePlayer ->
            Encode.string "WhitePlayer"

        BlackPlayer ->
            Encode.string "BlackPlayer"


playerDecoder : Decode.Decoder Player
playerDecoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "WhitePlayer" ->
                        Decode.succeed WhitePlayer

                    "BlackPlayer" ->
                        Decode.succeed BlackPlayer

                    _ ->
                        Decode.fail "Invalid Player"
            )


boardEncoder : Dict.Dict Position Cell -> Encode.Value
boardEncoder board =
    board
        |> Dict.toList
        |> List.map (\( pos, cell ) -> { pos = pos, cell = cell })
        |> List.map cellWrapperEncoder
        |> Encode.list


boardDecoder : Decode.Decoder (Dict Position Cell)
boardDecoder =
    let
        asTuple : CellWrapper -> ( Position, Cell )
        asTuple cw =
            ( cw.pos, cw.cell )

        toDict : List CellWrapper -> Dict Position Cell
        toDict wrappers =
            wrappers |> List.map asTuple |> Dict.fromList
    in
        (Decode.list decodeCellWrapper)
            |> Decode.map toDict


decodeCellWrapper : Decode.Decoder CellWrapper
decodeCellWrapper =
    Decode.map2 CellWrapper
        (field "pos" positionDecoder)
        (field "cell" cellDecoder)


cellWrapperEncoder : CellWrapper -> Encode.Value
cellWrapperEncoder v =
    Encode.object
        [ ( "pos", positionEncoder v.pos )
        , ( "cell", cellEncoder v.cell )
        ]


positionDecoder : Decode.Decoder Position
positionDecoder =
    Decode.map2 (,) (Decode.index 0 Decode.int) (Decode.index 1 Decode.int)


positionEncoder : Position -> Encode.Value
positionEncoder ( x, y ) =
    Encode.list [ Encode.int x, Encode.int y ]


cellDecoder : Decode.Decoder Cell
cellDecoder =
    Decode.map3 Cell
        (Decode.maybe <| field "pebble" pebbleDecoder)
        (field "noKill" Decode.bool)
        (field "state" cellStateDecoder)


cellEncoder : Cell -> Encode.Value
cellEncoder cell =
    Encode.object
        [ ( "pebble", pebbleEncoder cell.pebble )
        , ( "noKill", Encode.bool cell.noKill )
        , ( "state", cellStateEncoder cell.state )
        ]


pebbleEncoder : Maybe Pebble -> Encode.Value
pebbleEncoder v =
    case v of
        Just pebble ->
            case pebble of
                Black ->
                    Encode.string "Black"

                White ->
                    Encode.string "White"

        Nothing ->
            Encode.null


pebbleDecoder : Decode.Decoder Pebble
pebbleDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "White" ->
                        Decode.succeed White

                    "Black" ->
                        Decode.succeed Black

                    somethingElse ->
                        Decode.fail <| "Unknown pebble: " ++ somethingElse
            )


cellStateEncoder : CellState -> Encode.Value
cellStateEncoder v =
    case v of
        Normal ->
            Encode.string "Normal"

        Selected ->
            Encode.string "Selected"

        ValidMove ->
            Encode.string "ValidMove"

        LastMoved ->
            Encode.string "LastMoved"


cellStateDecoder : Decode.Decoder CellState
cellStateDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "Normal" ->
                        Decode.succeed Normal

                    "Selected" ->
                        Decode.succeed Selected

                    "ValidMove" ->
                        Decode.succeed ValidMove

                    "LastMoved" ->
                        Decode.succeed LastMoved

                    somethingElse ->
                        Decode.fail <| "Unknown state: " ++ somethingElse
            )
