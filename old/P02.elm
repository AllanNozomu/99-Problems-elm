module P02 exposing (penultimate)

import Html
import List
import Maybe


penultimate : List a -> Maybe a
penultimate list =
    case List.tail list of
        Just [ _ ] ->
            List.head list

        Just rest ->
            penultimate rest

        Nothing ->
            Nothing


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
            [ penultimate [ 1, 2, 3, 4 ] == Just 3
            , penultimate [ 1, 2 ] == Just 1
            , penultimate [ 1 ] == Nothing
            , penultimate [] == Nothing
            , penultimate [ "a", "b", "c" ] == Just "b"
            , penultimate [ "a" ] == Nothing
            ]


(...) : Int -> Int -> List Int
(...) start end =
    List.range start end
