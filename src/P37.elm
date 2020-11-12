module P37 exposing (solve)

import P36


solve : Int -> Int
solve a =
    P36.primeFactorMult a
        |> List.map (\( q, n ) -> (q - 1) * q ^ (n - 1))
        |> List.foldl (*) 1
