module P56 exposing (solve)

import P55 exposing (Tree(..))


mirror : Tree comparable -> Tree comparable -> Bool
mirror t1 t2 =
    case ( t1, t2 ) of
        ( Empty, Empty ) ->
            True

        ( Branch _ l1 r1, Branch _ l2 r2 ) ->
            mirror l1 r2 && mirror r1 l2

        _ ->
            False


solve : Tree comparable -> Bool
solve t =
    mirror t t
