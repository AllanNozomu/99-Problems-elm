module P25 exposing (solve)

import Array exposing (Array)
import Html exposing (a)
import Random


solve : List comparable -> List comparable
solve l =
    Array.fromList l
        |> solveAux (List.length l - 1) (Random.initialSeed (List.length l))
        |> Array.toList


solveAux : Int -> Random.Seed -> Array comparable -> Array comparable
solveAux i seed arr =
    if i <= 1 then
        arr

    else
        case Random.step (Random.int 0 i) seed of
            ( v, newSeed ) ->
                solveAux (i - 1) newSeed (arrSwap v i arr)


arrSwap : Int -> Int -> Array comparable -> Array comparable
arrSwap i j arr =
    if i == j then
        arr

    else
        case ( Array.get i arr, Array.get j arr ) of
            ( Just ii, Just jj ) ->
                Array.set i jj arr |> Array.set j ii

            _ ->
                arr
