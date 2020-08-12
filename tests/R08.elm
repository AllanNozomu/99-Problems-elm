module R08 exposing (suite)

import Expect
import Fuzz exposing (int, list, string)
import P08 exposing (solve)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "P08"
        [ test "Empty list" <|
            \_ ->
                Expect.equal (solve []) []
        , test "One element" <|
            \_ ->
                Expect.equal (solve [ 1 ]) [ 1 ]
        , test "Equal elements" <|
            \_ ->
                Expect.equal (solve [ 1, 1, 1, 1, 1]) [ 1 ]
        , test "Many Single elements" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3, 4, 5 ]) [ 1, 2, 3, 4, 5 ]
        , test "Repeated elements" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 2, 3, 1, 1, 1 ]) [ 1, 2, 3, 1 ]
        ]
