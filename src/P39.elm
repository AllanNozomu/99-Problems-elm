module P39 exposing (solve)

import P31


solve : Int -> Int -> List Int
solve s e =
    List.range s e
        |> List.filter P31.isPrime
