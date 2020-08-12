module P05 exposing (solve)


solve : List a -> List a
solve list =
    case list of
        [] ->
            []

        a :: r ->
            solve r ++ [ a ]
