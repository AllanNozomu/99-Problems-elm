module R14 exposing (suite)

import Expect
import Fuzz exposing (int, list, string)
import P14 exposing (solve)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "P14"
        [ test "Empty list" <|
            \_ ->
                Expect.equal (solve []) []
        , test "One element" <|
            \_ ->
                Expect.equal (solve [ 1 ]) [ 1, 1 ]
        , test "Two elements" <|
            \_ ->
                Expect.equal (solve [ 1, 2 ]) [ 1, 1, 2, 2 ]
        , fuzz (list int) "Random elements" <|
            \randomList ->
                Expect.equal (solve (List.sort randomList)) (List.sort (randomList ++ randomList))
        ]
