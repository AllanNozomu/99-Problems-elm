module P09 exposing (solve)


solve : List a -> List (List a)
solve list =
    case list of
        [] ->
            []

        x :: r ->
            solveAux r [x] []

solveAux : List a -> List a -> List (List a) -> List (List a)
solveAux list curr acc =
    case (list, curr) of
        
        (x :: r, previous :: _) ->
            if x == previous then
                solveAux r (x :: curr) acc
            else
                solveAux r [x] (acc ++ [curr])

        _ ->
             acc ++ [curr]
