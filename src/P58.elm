module P58 exposing (solve)

import P55 exposing (Tree(..))

reverse : Tree comparable -> Tree comparable
reverse tree =
    case tree of
    Empty -> Empty
    Branch x l r -> Branch x (reverse r) (reverse l)

addReverse : comparable -> Tree comparable -> Tree comparable
addReverse val tree =
    Branch val (reverse tree) tree

solve : comparable -> Int -> List (Tree comparable)
solve val n =
    if Basics.modBy 2 n == 0 then
        []
    else
        P55.solve val (n // 2) |>
        List.map (addReverse val)
