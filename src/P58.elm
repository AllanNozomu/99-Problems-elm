module P58 exposing (solve)

import P55 exposing (Tree(..))
import P56


solve : Int -> comparable -> List (Tree comparable)
solve n val =
    P55.solve n val
        |> List.filter P56.solve
