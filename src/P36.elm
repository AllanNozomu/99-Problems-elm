module P36 exposing (primeFactorMult, solve)

import P10
import P35


primeFactorMult : Int -> List ( Int, Int )
primeFactorMult =
    solve


step : Int -> Int
step n =
    case n of
        2 ->
            n + 1

        _ ->
            n + 2


solveAux : Int -> Int -> Int -> List ( Int, Int )
solveAux n candidate times =
    if n == 1 then
        [ ( candidate, times ) ]

    else if Basics.remainderBy candidate n == 0 then
        solveAux (n // candidate) candidate (times + 1)

    else if times > 0 then
        ( candidate, times ) :: solveAux n (step candidate) 0

    else
        solveAux n (step candidate) 0


solve : Int -> List ( Int, Int )
solve a =
    if a < 2 then
        []

    else
        solveAux a 2 0
