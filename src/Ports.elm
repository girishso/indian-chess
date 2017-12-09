port module Ports exposing (..)

import Json.Decode as Decode


port alert : String -> Cmd msg


port sendGameState : String -> Cmd msg


port gameStateChanged : (Decode.Value -> msg) -> Sub msg


port newSharedGameCreated : (String -> msg) -> Sub msg


port setThisPlayer : (String -> msg) -> Sub msg
