module R02 exposing (suite)

import Expect
import Fuzz exposing (int, list, string)
import P02 exposing (solve)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "P02"
        [ test "Empty list" <|
            \_ ->
                Expect.equal (solve []) Nothing
        , test "One element" <|
            \_ ->
                Expect.equal (solve [ 1 ]) Nothing
        , test "Two elements" <|
            \_ ->
                Expect.equal (solve [ 1, 2 ]) (Just 1)
        , test "Many elements" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3, 4, 5, 9 ]) (Just 5)
        ]
