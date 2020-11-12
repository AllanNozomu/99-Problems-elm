module P35 exposing (primeFactors, solve)

import P31


primeFactors : Int -> List Int
primeFactors =
    solve


solveAux : Int -> Int -> List Int
solveAux n candidate =
    if P31.isPrime n then
        [ n ]

    else if Basics.remainderBy candidate n == 0 then
        candidate :: solveAux (n // candidate) candidate

    else
        solveAux n (candidate + 1)


solve : Int -> List Int
solve a =
    if a < 2 then
        []

    else
        solveAux a 2
