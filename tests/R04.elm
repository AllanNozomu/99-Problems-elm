module R04 exposing (suite)

import Expect
import Fuzz exposing (int, list, string)
import List
import P04 exposing (solve)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "P02"
        [ test "Empty list" <|
            \_ ->
                Expect.equal (solve []) 0
        , test "One element" <|
            \_ ->
                Expect.equal (solve [ 1 ]) 1
        , fuzz (list int) "Random list" <|
            \randomList ->
                Expect.equal (solve randomList) (List.length randomList)
        ]
