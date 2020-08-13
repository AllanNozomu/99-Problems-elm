module P06 exposing (solve)


solve : List a -> Bool
solve list =
    list == reverse list


reverse : List a -> List a
reverse list =
    case list of
        [] ->
            []

        a :: r ->
            reverse r ++ [ a ]
