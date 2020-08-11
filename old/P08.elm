module P08 exposing (noDupes)

import Html
import List
import Maybe


noDupes : List a -> List a
noDupes xs =
    List.foldr
        (\elem list ->
            case List.head list of
                Just first ->
                    if first == elem then
                        list
                    else
                        elem :: list

                Nothing ->
                    elem :: []
        )
        []
        xs


main : Html.Html a
main =
    Html.text <|
        case test of
            0 ->
                "Your implementation passed all tests."

            1 ->
                "Your implementation failed one test."

            x ->
                "Your implementation failed " ++ (toString x) ++ " tests."


test : Int
test =
    List.length <|
        List.filter ((==) False)
            [ noDupes [ 1, 1, 1, 1, 2, 5, 5, 2, 1 ] == [ 1, 2, 5, 2, 1 ]
            , noDupes [ 2, 1, 1, 1 ] == [ 2, 1 ]
            , noDupes [ 2, 2, 2, 1, 1, 1 ] == [ 2, 1 ]
            , noDupes [ 1 ] == [ 1 ]
            , noDupes [] == []
            , noDupes [ "aa", "aa", "aa" ] == [ "aa" ]
            , noDupes [ "aab", "b", "b", "aa" ] == [ "aab", "b", "aa" ]
            ]
