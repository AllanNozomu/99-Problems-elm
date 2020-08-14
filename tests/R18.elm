module R18 exposing (suite)

import Array
import Expect
import Fuzz exposing (array, int, intRange, list, string, tuple3)
import P18 exposing (solve)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "P18"
        [ test "Empty list" <|
            \_ ->
                Expect.equal (solve [] 1 10) []
        , test "Zero index" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3 ] 0 100) []
        , test "Negative index 1" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3 ] -1 100) []
        , test "Negative index 2" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3 ] 1 -100) []
        , test "Simple Slice" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3, 4 ] 2 3) [ 2, 3 ]
        , test "Same number Slice" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3, 4 ] 2 2) [ 2 ]
        , test "Slice equal than list" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3, 4 ] 1 4) [ 1, 2, 3, 4 ]
        , test "Slice bigger than list" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3, 4 ] 1 100) [ 1, 2, 3, 4 ]
        , test "Slice begin bigger than list" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3, 4 ] 100 200) []
        , test "End bigger than begin" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3, 4 ] 100 99) []
        , fuzz (tuple3 ( array int, intRange 1 50, intRange 51 100 )) "Random list and index" <|
            \( randomArray, i, j ) -> Expect.equal (solve (Array.toList randomArray) i j) (Array.slice (i - 1) j randomArray |> Array.toList)
        ]
