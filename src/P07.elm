module P07 exposing (NestedList(..), solve)


type NestedList a
    = Elem a
    | SubList (List (NestedList a))


solve : NestedList a -> List a
solve nestedList =
    case nestedList of
        Elem a ->
            [ a ]

        SubList [] ->
            []

        SubList x ->
            concatNestedList x


concatNestedList : List (NestedList a) -> List a
concatNestedList list =
    case list of
        [] ->
            []

        a :: r ->
            solve a ++ concatNestedList r
