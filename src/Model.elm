module Model exposing (..)

import Matrix


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


type alias Model =
    { board : Matrix.Matrix Cell
    , currentPlayer : Player
    }


type Msg
    = OnCellClick Int Int Cell


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
                    |> Maybe.withDefault Matrix.empty
          , currentPlayer = WhitePlayer
          }
        , Cmd.none
        )
