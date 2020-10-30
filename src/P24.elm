module P24 exposing (solve)

import Random



-- TODO: this solve the problem, but will have preference with the first elements (or last ones) depending in the input values


solve : Int -> Int -> List Int
solve n e =
    if e <= 0 || n <= 0 || n > e then
        []

    else
        solveAux 1 (e + 1) (e - n) (Random.initialSeed (n * e))


solveAux : Int -> Int -> Int -> Random.Seed -> List Int
solveAux i e removes seed =
    if i == e then
        []

    else if removes == 0 then
        i :: solveAux (i + 1) e 0 seed

    else if removes == (e - i) then
        solveAux (i + 1) e (removes - 1) seed

    else
        case Random.step (Random.int 0 1) seed of
            ( 0, newSeed ) ->
                solveAux (i + 1) e (removes - 1) newSeed

            ( _, newSeed ) ->
                i :: solveAux (i + 1) e removes newSeed
