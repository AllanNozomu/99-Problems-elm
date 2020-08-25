module R22 exposing (suite)

import Array
import Expect
import Fuzz exposing (array, int, intRange, list, string, tuple)
import P22 exposing (solve)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "P22"
        [ test "Equal numbers" <|
            \_ ->
                Expect.equal (solve 0 0) (List.range 0 0)
        , test "Invalid range" <|
            \_ ->
                Expect.equal (solve 10 0) (List.range 10 0)
        , test "Simple range" <|
            \_ ->
                Expect.equal (solve 1 5) (List.range 1 5)
        , test "Negative range" <|
            \_ ->
                Expect.equal (solve -3 0) (List.range -3 0)
        , fuzz (tuple ( intRange -1000 1000, intRange -1000 1000 )) "Random range" <|
            \( begin, end ) ->
                Expect.equal (solve begin end) (List.range begin end)
        ]
