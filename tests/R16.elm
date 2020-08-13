module R16 exposing (suite)

import Expect
import Fuzz exposing (int, list, string)
import P16 exposing (solve)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "P16"
        [ test "Empty list" <|
            \_ ->
                Expect.equal (solve [] 1) []
        , test "Drop 0" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3 ] 0) []
        , test "Drop negative index" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3 ] -100) []
        , test "Drop 1th element" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3 ] 1) []
        , test "Drop index larger than list" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3 ] 4) [ 1, 2, 3 ]
        , test "Drop even elements" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3, 1, 2, 3, 1 ] 2) [ 1, 3, 2, 1 ]
        ]
