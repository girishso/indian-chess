module Model exposing (..)

import Dict exposing (..)


type Pebble
    = Black
    | White


type CellState
    = Normal
    | Selected
    | ValidMove


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
    { board : Dict.Dict Position Cell
    , currentPlayer : Player
    , isHowToPlayPopupActive : Bool
    , isAboutPopupActive : Bool
    }


type Msg
    = OnCellClick Int Int Cell
    | ToggleHowToPlay


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


init : String -> ( Model, Cmd Msg )
init path =
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
        ( { board = board
          , currentPlayer = WhitePlayer
          , isHowToPlayPopupActive = False
          , isAboutPopupActive = False
          }
        , Cmd.none
        )


repeatDict : Int -> Int -> Int -> Cell -> List ( Position, Cell )
repeatDict startX uptoX y cell =
    List.range startX uptoX
        |> List.map (\i -> ( ( i, y ), cell ))


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
