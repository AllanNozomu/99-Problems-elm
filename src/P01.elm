module P01 exposing (solve)


solve : List a -> Maybe a
solve list =
    case list of
        [] ->
            Nothing

        [ x ] ->
            Just x

        _ :: r ->
            solve r
