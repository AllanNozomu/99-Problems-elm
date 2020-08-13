module R15 exposing (suite)

import Expect
import Fuzz exposing (int, list, string)
import P15 exposing (solve)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "P15"
        [ test "Empty list" <|
            \_ ->
                Expect.equal (solve [] 1) []
        , test "Multiply by 0" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3 ] 0) []
        , test "Multiply by negative" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3 ] -10) []
        , test "One element" <|
            \_ ->
                Expect.equal (solve [ 1 ] 1) [ 1 ]
        , test "One elemnt times 2" <|
            \_ ->
                Expect.equal (solve [ 1 ] 2) [ 1, 1 ]
        , fuzz (list int) "Random elements multiply by zero" <|
            \randomList ->
                Expect.equal (solve randomList 0) []
        , fuzz (list int) "Random elements" <|
            \randomList ->
                Expect.equal (solve randomList 1) randomList
        , fuzz (list int) "Random elements times 2" <|
            \randomList ->
                Expect.equal (solve (List.sort randomList) 2) (List.sort (randomList ++ randomList))
        , fuzz (list int) "Random elements times 5" <|
            \randomList ->
                Expect.equal (solve (List.sort randomList) 5) (List.sort (randomList ++ randomList ++ randomList ++ randomList ++ randomList))
        ]
