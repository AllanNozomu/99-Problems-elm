module P13 exposing (Element(..), solve)


type Element a
    = Single a
    | Multiple Int a


solve : List a -> List (Element a)
solve list =
    case list of
        [] ->
            []

        x :: r ->
            solveAux r ( 1, x ) []


solveAux : List a -> ( Int, a ) -> List (Element a) -> List (Element a)
solveAux list curr acc =
    case ( list, curr ) of
        ( x :: r, ( count, previous ) ) ->
            if x == previous then
                solveAux r ( count + 1, x ) acc

            else
                solveAux r ( 1, x ) (acc ++ [ makeElement curr ])

        _ ->
            acc ++ [ makeElement curr ]


makeElement : ( Int, a ) -> Element a
makeElement element =
    case element of
        ( 1, a ) ->
            Single a

        ( x, a ) ->
            Multiple x a
