module P03 exposing (solve)

import Html exposing (a)


solve : List a -> Int -> Maybe a
solve list k =
    if k <= 0 then
        Nothing

    else
        solveAux list k


solveAux : List a -> Int -> Maybe a
solveAux list k =
    case ( list, k ) of
        ( a :: _, 1 ) ->
            Just a

        ( [], _ ) ->
            Nothing

        _ :: 1 ->
            Nothing

        ( _ :: r, n ) ->
            solveAux r (n - 1)
