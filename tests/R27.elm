module R27 exposing (suite)

import Array
import Expect
import Fuzz exposing (array, bool, float, floatRange, int, intRange, list, percentage, string, tuple, tuple3)
import P27 exposing (solve)
import Test exposing (Test, describe, fuzz, test)


factorial : Int -> Int
factorial n =
    List.product (List.range 1 n)


suite : Test
suite =
    describe "P27"
        [ test "Empty List" <|
            \_ ->
                Expect.equal (solve [] [ 0 ]) []
        , test "Different values" <|
            \_ ->
                Expect.equal (solve (List.range 1 10) [ 5, 4, 3 ]) []
        , test "Statement test 1" <|
            \_ ->
                let
                    n =
                        9

                    groups =
                        [ 2, 3, 4 ]
                in
                Expect.equal (solve (List.range 1 n) groups |> List.length) 1260
        , test "Statement test 2" <|
            \_ ->
                let
                    n =
                        9

                    groups =
                        [ 2, 2, 5 ]
                in
                Expect.equal (solve (List.range 1 n) groups |> List.length) 756
        , fuzz (intRange 8 16) "Random range with two groups" <|
            \n ->
                let
                    groups =
                        [ n // 2, n // 2 + Basics.remainderBy 2 n ]

                    res =
                        solve (List.range 1 n) groups
                in
                Expect.equal (res |> List.length) (factorial n // (List.map factorial groups |> List.product))
        , fuzz (intRange 8 12) "Random range with three groups" <|
            \n ->
                let
                    groups =
                        [ n // 3, n // 3, n - (2 * (n // 3)) ]

                    res =
                        solve (List.range 1 n) groups
                in
                Expect.equal (res |> List.length) (factorial n // (List.map factorial groups |> List.product))
        ]
