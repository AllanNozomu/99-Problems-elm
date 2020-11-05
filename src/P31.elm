module P31 exposing (solve)


solve : Int -> Bool
solve n =
    if n <= 1 then
        False

    else if n == 2 then
        True

    else
        let
            myRemainderBy x y =
                Basics.remainderBy y x
        in
        List.range 2 (Basics.floor (Basics.sqrt (toFloat n)))
            |> List.map (myRemainderBy n)
            |> List.map ((==) 0)
            |> List.foldl (\curr acc -> acc || curr) False
            |> Basics.not
