module P62 exposing (solve, solveB)

import P55 exposing (Tree(..))


solve : Tree comparable -> List comparable
solve tree =
    case tree of
        Empty ->
            []

        Branch x l r ->
            case ( l, r ) of
                ( Empty, Empty ) ->
                    []

                ( _, Empty ) ->
                    x :: solve l

                ( Empty, _ ) ->
                    x :: solve r

                _ ->
                    x :: solve l ++ solve r


solveB : Int -> Tree comparable -> List comparable
solveB level tree =
    let
        solveBAux : Int -> Tree comparable -> List comparable
        solveBAux curr t =
            case t of
                Empty ->
                    []

                Branch x l r ->
                    if curr == level then
                        [ x ]

                    else
                        solveBAux (curr + 1) l ++ solveBAux (curr + 1) r
    in
    solveBAux 1 tree
