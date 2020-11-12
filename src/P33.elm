module P33 exposing (solve)


gcd : Int -> Int -> Int
gcd a b =
    case b of
        0 ->
            a

        _ ->
            gcd b (Basics.remainderBy b a)


solve : Int -> Int -> Bool
solve a b =
    gcd a b == 1
