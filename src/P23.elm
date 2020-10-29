module P23 exposing (solve)

import Random


solve : List Int -> Int -> List Int
solve l n =
    if n <= 0 then
        []

    else if n >= List.length l then
        l

    else
        solveAux l n (Random.initialSeed (List.length l))


solveAux : List Int -> Int -> Random.Seed -> List Int
solveAux l n seed =
    case ( l, n ) of
        ( [], _ ) ->
            []

        ( _, 0 ) ->
            []

        ( x :: xs, _ ) ->
            if List.length l == n then
                l

            else
                case Random.step (Random.int 0 1) seed of
                    ( 0, newSeed ) ->
                        solveAux xs n newSeed

                    ( _, newSeed ) ->
                        x :: solveAux xs (n - 1) newSeed
