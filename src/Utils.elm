module Utils exposing (..)

import Dict exposing (..)


mapValues : (b -> b) -> Dict comparable b -> Dict comparable b
mapValues f dict =
    Dict.map (\_ val -> f val) dict


filterValues : (b -> Bool) -> Dict comparable b -> Dict comparable b
filterValues f dict =
    Dict.filter (\_ val -> f val) dict


updateIfExists : comparable -> (v -> v) -> Dict comparable v -> Dict comparable v
updateIfExists k f dict =
    Dict.update k (Maybe.map f) dict
