module P55 exposing (Tree(..), solve, tree0, tree1, tree2, tree3, tree4)


type Tree a
    = Empty
    | Branch a (Tree a) (Tree a)


leaf : a -> Tree a
leaf x =
    Branch x Empty Empty


tree0 : Tree Int
tree0 =
    Empty


tree1 : Tree Int
tree1 =
    leaf 1


tree2 : Tree Int
tree2 =
    Branch 1 (leaf 2) Empty


tree3 : Tree Int
tree3 =
    Branch 1 (leaf 2) (leaf 3)


tree4 : Tree Int
tree4 =
    Branch 1 (Branch 2 (leaf 4) Empty) (leaf 3)


decapsulate : List (List (Tree a)) -> List (Tree a)
decapsulate =
    List.map
        (\ll ->
            case ll of
                [] ->
                    Empty

                lll :: _ ->
                    lll
        )


solve : a -> Int -> List (Tree a)
solve val n =
    case n of
        0 ->
            []

        _ ->
            let
                level =
                    lastLevel n 0

                allLastLevels =
                    generateLastLevel val (n - (2 ^ level - 1)) (2 ^ level) []
            in
            List.map (\ll -> generateLevel val 0 n ll) allLastLevels
                |> decapsulate


lastLevel : Int -> Int -> Int
lastLevel n level =
    if n <= 2 ^ level then
        level

    else
        lastLevel (n - (2 ^ level)) (level + 1)


generateLastLevel : a -> Int -> Int -> List (Tree a) -> List (List (Tree a))
generateLastLevel val remaining size acc =
    if size == 0 then
        if remaining == 0 then
            [ acc ]

        else
            []

    else if remaining == 0 then
        generateLastLevel val 0 (size - 1) (Empty :: acc)

    else
        generateLastLevel val remaining (size - 1) (Empty :: acc)
            ++ generateLastLevel val (remaining - 1) (size - 1) (leaf val :: acc)


mergeLevel : List (Tree a) -> List (Tree a) -> List (Tree a)
mergeLevel parents children =
    case ( parents, children ) of
        ( p :: ps, c1 :: c2 :: cs ) ->
            case p of
                Branch val _ _ ->
                    Branch val c1 c2 :: mergeLevel ps cs

                _ ->
                    []

        _ ->
            []


generateLevel : a -> Int -> Int -> List (Tree a) -> List (Tree a)
generateLevel val level remaining ll =
    if 2 ^ level < remaining then
        let
            curr =
                List.repeat (2 ^ level) (leaf val)

            children =
                generateLevel val (level + 1) (remaining - 2 ^ level) ll
        in
        mergeLevel curr children

    else
        ll
