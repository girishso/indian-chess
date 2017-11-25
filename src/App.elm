module App exposing (..)

import Html exposing (programWithFlags)
import Matrix
import Matrix.Extra
import Maybe exposing (withDefault)
import View exposing (view)
import Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnCellClick x y cell ->
            let
                newBoard =
                    if isCurrentPlayersCell model cell then
                        model.board
                            |> Matrix.map (\c -> { c | state = Normal })
                            |> Matrix.set y x ({ cell | state = Selected })
                            |> Matrix.indexedMap
                                (\j i c ->
                                    if isNeighbour i j x y && c.pebble == Nothing then
                                        { c | state = ValidMove }
                                    else
                                        c
                                )
                    else
                        model.board
            in
                ( { model | board = newBoard }, Cmd.none )


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
