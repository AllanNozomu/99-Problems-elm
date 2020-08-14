module R19 exposing (suite)

import Expect
import Fuzz exposing (int, list, string, tuple)
import P19 exposing (solve)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "P19"
        [ test "Empty list" <|
            \_ ->
                Expect.equal (solve [] 1) []
        , test "Rotate 0" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3 ] 0) [ 1, 2, 3 ]
        , test "Rotate negative index" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3 ] -1) [ 3, 1, 2 ]
        , test "Rotate positive index" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3 ] 1) [ 2, 3, 1 ]
        , test "Rotate index larger than list" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3 ] 5) [ 3, 1, 2 ]
        , test "Rotate index larger than list (negative)" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3 ] -11) [ 2, 3, 1 ]
        , fuzz (tuple ( list int, int )) "Rotate random list random index " <|
            \( randomList, i ) ->
                let
                    ll =
                        List.length randomList

                    index =
                        if Basics.remainderBy ll i < 0 then
                            Basics.remainderBy ll i + ll

                        else
                            Basics.remainderBy ll i
                in
                Expect.equal (solve randomList index) (List.drop index randomList ++ List.take index randomList)
        ]
