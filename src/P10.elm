module P10 exposing (solve)

solve : List a -> List (Int, a)
solve list =
    case list of
        [] ->
            []

        x :: r ->
            solveAux r (1, x) []

solveAux : List a -> (Int, a) -> List (Int, a) -> List (Int, a)
solveAux list curr acc =
    case (list, curr) of
        
        (x :: r, (count, previous)) ->
            if x == previous then
                solveAux r (count + 1, x) acc
            else
                solveAux r (1, x) (acc ++ [curr])

        _ ->
             acc ++ [curr]
