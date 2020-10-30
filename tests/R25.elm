module R25 exposing (suite)

import Array
import Expect
import Fuzz exposing (array, bool, float, floatRange, int, intRange, list, percentage, string, tuple, tuple3)
import P25 exposing (solve)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "P25"
        [ test "Empty list" <|
            \_ ->
                Expect.equal (solve []) []
        , test "One element" <|
            \_ ->
                Expect.equal (solve [ 1 ]) [ 1 ]
        , fuzz (list int) "Random List" <|
            \l ->
                Expect.equal (List.sort <| solve l) (List.sort l)
        ]
