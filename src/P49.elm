module P49 exposing (solve)


concatenate : String -> List String -> List String
concatenate s =
    List.map (\ss -> s ++ ss)


solve : Int -> List String
solve n =
    if n == 0 then
        []

    else
        solveAux (n - 1) [ "0", "1" ]


solveAux : Int -> List String -> List String
solveAux n acc =
    if n == 0 then
        acc

    else
        solveAux (n - 1) (concatenate "0" acc ++ (concatenate "1" <| List.reverse acc))
