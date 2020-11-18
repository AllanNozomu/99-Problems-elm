module P40 exposing (goldbach, solve)

import P31


goldbach : Int -> ( Int, Int )
goldbach =
    solve


solve : Int -> ( Int, Int )
solve n =
    case
        List.range 2 (n // 2)
            |> List.map (\x -> ( x, n - x ))
            |> List.filter
                (\( left, right ) ->
                    P31.isPrime left && P31.isPrime right
                )
            |> List.head
    of
        Just res ->
            res

        Nothing ->
            ( -1, -1 )
