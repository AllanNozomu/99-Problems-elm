module P32 exposing (solve)


solve : Int -> Int -> Int
solve a b =
    case b of
        0 ->
            a

        _ ->
            solve b (Basics.remainderBy b a)
