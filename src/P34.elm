module P34 exposing (solve)


gcd : Int -> Int -> Int
gcd a b =
    case b of
        0 ->
            a

        _ ->
            gcd b (Basics.remainderBy b a)


coprime : Int -> Int -> Bool
coprime a b =
    gcd a b == 1


solve : Int -> Int
solve a =
    if a < 0 then
        0

    else
        List.range 1 a
            |> List.map (coprime a)
            |> List.filter Basics.identity
            |> List.length
