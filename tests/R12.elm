module R12 exposing (suite)

import Expect
import Fuzz exposing (int, list, string)
import P12 exposing (Element(..), solve)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "P12"
        [ test "Empty list" <|
            \_ ->
                Expect.equal (solve []) []
        , test "One element" <|
            \_ ->
                Expect.equal (solve [ Single 1 ]) [ 1 ]
        , test "Equal elements" <|
            \_ ->
                Expect.equal (solve [ Multiple 5 1 ]) [ 1, 1, 1, 1, 1 ]
        , test "Many Single elements" <|
            \_ ->
                Expect.equal (solve [ Single 1, Single 2, Single 3, Single 4, Single 5 ]) [ 1, 2, 3, 4, 5 ]
        , test "Repeated elements" <|
            \_ ->
                Expect.equal (solve [ Multiple 2 1, Multiple 2 2, Single 3, Multiple 3 1 ]) [ 1, 1, 2, 2, 3, 1, 1, 1 ]
        ]
