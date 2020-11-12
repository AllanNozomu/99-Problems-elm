module P31 exposing (isPrime, solve)


isPrime : Int -> Bool
isPrime =
    solve


solveAux : Int -> List Int -> Bool
solveAux n candidates =
    case candidates of
        [] ->
            True

        candidate :: r ->
            if Basics.remainderBy candidate n == 0 then
                False

            else
                solveAux n r


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
            |> solveAux n
