module P16 exposing (solve)


solve : List a -> Int -> List a
solve list n =
    case ( list, n ) of
        ( [], _ ) ->
            []

        ( _, 0 ) ->
            []

        _ ->
            if n < 0 then
                []

            else
                solveAux list n n


solveAux : List a -> Int -> Int -> List a
solveAux list n count =
    case ( list, count ) of
        ( [], _ ) ->
            []

        ( _ :: r, 1 ) ->
            solveAux r n n

        ( x :: r, _ ) ->
            x :: solveAux r n (count - 1)
