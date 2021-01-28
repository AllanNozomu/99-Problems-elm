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
                    [ x ]

                ( _, Empty ) ->
                    solve l

                ( Empty, _ ) ->
                    solve r

                _ ->
                    solve l ++ solve r


solveB : Tree comparable -> List comparable
solveB tree =
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
