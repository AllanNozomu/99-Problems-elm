module P33 exposing (isCoprime, solve)

import P32


isCoprime : Int -> Int -> Bool
isCoprime =
    solve


solve : Int -> Int -> Bool
solve a b =
    P32.gcd a b == 1
