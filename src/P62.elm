module P62 exposing (solve)

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
