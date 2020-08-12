module P02 exposing (solve)


solve : List a -> Maybe a
solve list =
    case list of
        [] ->
            Nothing

        [ _ ] ->
            Nothing

        x :: [ _ ] ->
            Just x

        _ :: _ :: r ->
            solve r
