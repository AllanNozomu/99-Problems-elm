module P04 exposing (solve)

import Html exposing (a)

solve : List a -> Int
solve list =
    case list of
       [] -> 0
       _ :: r -> 1 + (solve r)