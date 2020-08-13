module P14 exposing (solve)


solve : List a -> List a
solve list =
    case list of
        [] ->
            []

        x :: r ->
            x :: x :: solve r
