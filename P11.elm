module P11 exposing (..)

import P09
import Html
import List
import Maybe


type RleCode a
    = Run Int a
    | Single a


rleEncode : List a -> List (RleCode a)
rleEncode list =
    List.concat <|
        List.map
            (\elem ->
                case List.head elem of
                    Just val ->
                        let
                            len =
                                List.length elem
                        in
                            if len > 1 then
                                [ Run len val ]
                            else
                                [ Single val ]

                    _ ->
                        []
            )
            (P09.pack list)


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
            [ rleEncode [ 1, 1, 1, 1, 2, 5, 5, 2, 1 ]
                == [ Run 4 1, Single 2, Run 2 5, Single 2, Single 1 ]
            , rleEncode [ 2, 1, 1, 1 ] == [ Single 2, Run 3 1 ]
            , rleEncode [ 2, 2, 2, 1, 1, 1 ] == [ Run 3 2, Run 3 1 ]
            , rleEncode [ 1 ] == [ Single 1 ]
            , rleEncode [] == []
            , rleEncode [ "aa", "aa", "aa" ] == [ Run 3 "aa" ]
            , rleEncode [ "aab", "b", "b", "aa" ]
                == [ Single "aab", Run 2 "b", Single "aa" ]
            ]
