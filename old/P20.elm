module P20 exposing (dropAt)

import Html
import List


dropAt : Int -> List a -> List a
dropAt n list =
    List.append (List.take (n - 1) list) (List.drop n list)


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
            [ dropAt 2 [ 1, 2, 5, 5, 2, 1 ] == [ 1, 5, 5, 2, 1 ]
            , dropAt 3 (List.range 1 14) == [ 1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 ]
            , dropAt 6 (List.range 1 5) == [ 1, 2, 3, 4, 5 ]
            , dropAt 0 (List.range 1 5) == [ 1, 2, 3, 4, 5 ]
            , dropAt -1 (List.range 1 5) == [ 1, 2, 3, 4, 5 ]
            , dropAt 1 (List.range 1 5) == [ 2, 3, 4, 5 ]
            , dropAt 2 [ "1", "2", "3", "4", "5" ] == [ "1", "3", "4", "5" ]
            ]
