module P22 exposing (solve)


solve : Int -> Int -> List Int
solve begin end =
    if begin > end then
        []

    else
        solveAux begin end


solveAux : Int -> Int -> List Int
solveAux begin end =
    if begin == end then
        [ end ]

    else
        begin :: solveAux (begin + 1) end
