module P26 exposing (solve)


solve : Int -> List a -> List (List a)
solve n l =
    if n == 0 then
        []

    else
        solveAux n l []


solveAux : Int -> List a -> List a -> List (List a)
solveAux n l acc =
    if n == 0 then
        [ acc ]

    else
        case l of
            [] ->
                []

            x :: xs ->
                solveAux (n - 1) xs (x :: acc) ++ solveAux n xs acc
