module P34 exposing (solve, totientPhi)

import P33


totientPhi : Int -> Int
totientPhi =
    solve


solve : Int -> Int
solve a =
    if a < 0 then
        0

    else
        List.range 1 a
            |> List.map (P33.isCoprime a)
            |> List.filter Basics.identity
            |> List.length
