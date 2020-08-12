module P06 exposing (solve)
import Html exposing (a)


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
