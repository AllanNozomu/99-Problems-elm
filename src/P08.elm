module P08 exposing (solve)


solve : List a -> List a
solve list =
    case list of
        [] ->
            []

        x :: r ->
            x :: solveAux r x


solveAux : List a -> a -> List a
solveAux list previous =
    case list of
        [] ->
            []

        x :: r ->
            if x == previous then
                solveAux r x

            else
                x :: solveAux r x
