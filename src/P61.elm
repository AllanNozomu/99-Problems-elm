module P61 exposing (solve)

import P55 exposing (Tree(..))


solve : Tree comparable -> Int
solve tree =
    case tree of
        Empty ->
            0

        Branch _ l r ->
            case ( l, r ) of
                ( Empty, Empty ) ->
                    1

                ( _, Empty ) ->
                    solve l

                ( Empty, _ ) ->
                    solve r

                _ ->
                    solve l + solve r


solveB : Tree comparable -> List comparable
solveB tree =
    case tree of
        Empty ->
            []

        Branch x l r ->
            case ( l, r ) of
                ( Empty, Empty ) ->
                    [ x ]

                ( _, Empty ) ->
                    solveB l

                ( Empty, _ ) ->
                    solveB r

                _ ->
                    solveB l ++ solveB r
