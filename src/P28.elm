module P28 exposing (solve, solveB)

import Dict exposing (Dict)


compareListLength : List comparable -> List comparable -> Basics.Order
compareListLength a b =
    compare (List.length a) (List.length b)


solve : List (List comparable) -> List (List comparable)
solve =
    List.sortWith compareListLength


solveB : List (List comparable) -> List (List comparable)
solveB l =
    solve l
        |> List.foldr
            (\curr acc ->
                case acc of
                    (x :: xs) :: r ->
                        if List.length x == List.length curr then
                            (curr :: x :: xs) :: r

                        else
                            [ curr ] :: acc

                    _ ->
                        [ [ curr ] ]
            )
            []
        |> solve
        |> List.concat
