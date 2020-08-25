module R21 exposing (suite)

import Array
import Expect
import Fuzz exposing (array, int, intRange, list, string, tuple)
import P21 exposing (solve)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "P21"
        [ test "Empty list" <|
            \_ ->
                Expect.equal (solve 1 [] 1) [ 1 ]
        , test "Add begin" <|
            \_ ->
                Expect.equal (solve 1 [ 2, 3 ] 1) [ 1, 2, 3 ]
        , test "Add end" <|
            \_ ->
                Expect.equal (solve 3 [ 1, 2 ] 3) [ 1, 2, 3 ]
        , test "Add middle" <|
            \_ ->
                Expect.equal (solve 2 [ 1, 3 ] 2) [ 1, 2, 3 ]
        , test "Bigger index" <|
            \_ ->
                Expect.equal (solve 10 [ 1, 2, 3, 4 ] 10) [ 1, 2, 3, 4 ]
        , test "Negative index" <|
            \_ ->
                Expect.equal (solve -10 [ 1, 2, 3, 4 ] -10) [ 1, 2, 3, 4 ]
        , fuzz (tuple ( list int, intRange 1 100 )) "Random List and index" <|
            \( randomList, k ) ->
                if k - 1 <= List.length randomList then
                    Expect.equal (solve k randomList k) (List.take (k - 1) randomList ++ (k :: List.drop (k - 1) randomList))

                else
                    Expect.equal (solve k randomList k) randomList
        ]
