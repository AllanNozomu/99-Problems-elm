module P56 exposing (solve)


type Tree a
    = Empty
    | Branch a (Tree a) (Tree a)


mirror : Tree a -> Tree a -> Bool
mirror t1 t2 =
    case ( t1, t2 ) of
        ( Empty, Empty ) ->
            True

        ( Branch _ l1 r1, Branch _ l2 r2 ) ->
            mirror l1 r2 && mirror r1 l2

        _ ->
            False


solve : Tree a -> Bool
solve t =
    mirror t t
