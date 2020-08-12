module R05 exposing (suite)

import Expect
import Fuzz exposing (int, list, string)
import List
import P05 exposing (solve)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "P05"
        [ test "Empty list" <|
            \_ ->
                Expect.equal (solve []) []
        , test "One element" <|
            \_ ->
                Expect.equal (solve [ 1 ]) [1]
        , fuzz (list int) "Random list" <|
            \randomList ->
                Expect.equal (solve randomList) (List.reverse randomList)
        ]
