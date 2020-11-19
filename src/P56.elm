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
            case ( ( l1, r1 ), ( l2, r2 ) ) of
                ( ( Branch _ _ _, Branch _ _ _ ), ( Branch _ _ _, Branch _ _ _ ) ) ->
                    mirror l1 r2 && mirror r1 l2

                ( ( Branch _ _ _, Empty ), ( Empty, Branch _ _ _ ) ) ->
                    mirror l1 r2

                ( ( Empty, Branch _ _ _ ), ( Branch _ _ _, Empty ) ) ->
                    mirror r1 l2

                ( ( Empty, Empty ), ( Empty, Empty ) ) ->
                    True

                _ ->
                    False

        _ ->
            False


solve : Tree a -> Bool
solve t =
    mirror t t
