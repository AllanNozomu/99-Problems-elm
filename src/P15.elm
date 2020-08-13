module P15 exposing (solve)


solve : List a -> Int -> List a
solve list n =
    case ( list, n ) of
        ( [], _ ) ->
            []

        ( _, 0 ) ->
            []

        _ ->
            solveAux list n n


solveAux : List a -> Int -> Int -> List a
solveAux list n count =
    case ( list, count ) of
        ( [], _ ) ->
            []

        ( _ :: r, 0 ) ->
            solveAux r n n

        ( x :: _, _ ) ->
            x :: solveAux list n (count - 1)
