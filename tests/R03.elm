module R03 exposing (suite)

import Expect
import Fuzz exposing (int, list, string)
import P03 exposing (solve)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "P02"
        [ test "Empty list" <|
            \_ ->
                Expect.equal (solve [] 1) Nothing
        , test "One element" <|
            \_ ->
                Expect.equal (solve [ 1 ] 1) (Just 1)
        , test "Zero Index" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3, 4, 5, 9 ] 0) Nothing
        , test "Negative Index" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3, 4, 5, 9 ] -2) Nothing
        , test "Overflow Index" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3, 4, 5, 9 ] 150) Nothing
        , test "First Index" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3, 4, 5, 9 ] 1) (Just 1)
        , test "Last Index" <|
            \_ ->
                Expect.equal (solve [ 1, -10, 200, 100 ] 4) (Just 100)
        , test "Middle Index" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3, 10, 500, 6, 7, 8 ] 5) (Just 500)
        ]
