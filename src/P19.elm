module P19 exposing (solve)

import List


solve : List a -> Int -> List a
solve list n =
    case list of
        [] ->
            []

        _ ->
            let
                ll =
                    List.length list

                index =
                    Basics.remainderBy ll n
            in
            if index < 0 then
                solveAux list (index + ll) []

            else
                solveAux list index []


solveAux : List a -> Int -> List a -> List a
solveAux list n acc =
    case ( list, n ) of
        ( [], _ ) ->
            acc

        ( _, 0 ) ->
            list ++ acc

        ( x :: r, _ ) ->
            solveAux r (n - 1) (acc ++ [ x ])
