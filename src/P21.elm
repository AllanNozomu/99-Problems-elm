module P21 exposing (solve)


solve : a -> List a -> Int -> List a
solve data list n =
    case ( list, n ) of
        ( _, 1 ) ->
            data :: list

        ( [], _ ) ->
            []

        ( a :: x, _ ) ->
            a :: solve data x (n - 1)
