module P41 exposing (solve)

import P40


isEven : Int -> Bool
isEven n =
    remainderBy 2 n == 0


solve : Int -> Int -> Int -> List ( Int, Int )
solve s e l =
    List.range s e
        |> List.filter isEven
        |> List.map P40.goldbach
        |> List.filter (\( left, right ) -> left >= l && right >= l)
