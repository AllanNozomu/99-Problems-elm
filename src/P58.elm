module P58 exposing (solve)

import P55 exposing (Tree(..))
import P56


solve : comparable -> Int -> List (Tree comparable)
solve val n =
    P55.solve val n
        |> List.filter P56.solve
