module R01 exposing (suite)

import Expect
import Fuzz exposing (int, list, string)
import Test exposing (Test, describe, fuzz, test)

import P01 exposing (solve)

suite : Test
suite =
    describe "P01"
        [ test "Empty list" <|
            \_ ->
                Expect.equal (solve []) Nothing

        , test "One element" <|
            \_ ->
                Expect.equal (solve [1]) (Just 1)

        , test "Many elements" <|
            \_ ->
                Expect.equal (solve [1,2,3,4,5,9]) (Just 9)
        ]
