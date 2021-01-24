module P59 exposing (solve)

import P55 exposing (Tree(..))


solve : comparable -> Int -> List (Tree comparable)
solve val n =
    List.range (2 ^ (n - 1)) (2 ^ n - 1)
        |> List.map (P55.solve val)
        |> List.concat
