module R17 exposing (suite)

import Expect
import Fuzz exposing (int, intRange, list, string, tuple)
import P17 exposing (solve)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "P17"
        [ test "Empty list" <|
            \_ ->
                Expect.equal (solve [] 1) ( [], [] )
        , test "Split 0" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3 ] 0) ( [], [ 1, 2, 3 ] )
        , test "Split equal than list" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3, 4 ] 4) ( [ 1, 2, 3, 4 ], [] )
        , test "Split bigger than list" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3, 4 ] 100) ( [ 1, 2, 3, 4 ], [] )
        , test "Negative index" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3 ] -10) ( [], [] )
        , fuzz (tuple ( list int, intRange 0 1000 )) "Random list and index" <|
            \( randomList, k ) -> Expect.equal (solve randomList k) ( List.take k randomList, List.drop k randomList )
        ]
