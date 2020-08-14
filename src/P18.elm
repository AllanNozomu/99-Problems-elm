module P18 exposing (solve)


solve : List a -> Int -> Int -> List a
solve list i j =
    case ( list, i, j ) of
        ( [], _, _ ) ->
            []

        ( _, 0, _ ) ->
            []

        ( _, _, 0 ) ->
            []

        _ ->
            if i < 0 || j < 0 || i > j then
                []

            else
                solveAux list i j


solveAux : List a -> Int -> Int -> List a
solveAux list i j =
    case ( list, i, j ) of
        ( [], _, _ ) ->
            []

        ( _, _, 0 ) ->
            []

        ( x :: r, 1, _ ) ->
            x :: solveAux r 1 (j - 1)

        ( _ :: r, _, _ ) ->
            solveAux r (i - 1) (j - 1)
