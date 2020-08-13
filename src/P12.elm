module P12 exposing (Element(..), solve)


type Element a
    = Single a
    | Multiple Int a


solve : List (Element a) -> List a
solve list =
    case list of
        [] ->
            []

        x :: r ->
            elementToList x ++ solve r


elementToList : Element a -> List a
elementToList element =
    case element of
        Single x ->
            [ x ]

        Multiple n x ->
            List.repeat n x
