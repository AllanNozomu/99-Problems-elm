module P09 exposing (pack, packOther)

import P07E1
import P07E2
import Html
import List
import Maybe


pack : List a -> List (List a)
pack xs =
    List.foldr
        (\elem list ->
            case list of
                (f :: r) :: rest ->
                    if elem == f then
                        List.append [ f, f ] r :: rest
                    else
                        List.append [ [ elem ], f :: r ] rest

                _ ->
                    [ [ elem ] ]
        )
        []
        xs


packOther : List a -> List (List a)
packOther xs =
    case xs of
        [] ->
            []

        a :: r ->
            List.append
                [ (P07E2.takeWhile ((==) a) xs) ]
                (packOther <| P07E1.dropWhile ((==) a) xs)


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
            [ pack [ 1, 1, 1, 1, 2, 5, 5, 2, 1 ] == [ [ 1, 1, 1, 1 ], [ 2 ], [ 5, 5 ], [ 2 ], [ 1 ] ]
            , pack [ 2, 1, 1, 1 ] == [ [ 2 ], [ 1, 1, 1 ] ]
            , pack [ 2, 2, 2, 1, 1, 1 ] == [ [ 2, 2, 2 ], [ 1, 1, 1 ] ]
            , pack [ 1 ] == [ [ 1 ] ]
            , pack [] == []
            , pack [ "aa", "aa", "aa" ] == [ [ "aa", "aa", "aa" ] ]
            , pack [ "aab", "b", "b", "aa" ] == [ [ "aab" ], [ "b", "b" ], [ "aa" ] ]
            ]
