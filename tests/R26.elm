module R26 exposing (suite)

import Array
import Expect
import Fuzz exposing (array, bool, float, floatRange, int, intRange, list, percentage, string, tuple, tuple3)
import P26 exposing (solve)
import Test exposing (Test, describe, fuzz, test)


factorial : Int -> Int
factorial n =
    List.product (List.range 1 n)


suite : Test
suite =
    describe "P26"
        [ test "Size 0" <|
            \_ ->
                Expect.equal (solve 0 [ 123 ]) []
        , test "Empty list" <|
            \_ ->
                Expect.equal (solve 10 []) []
        , fuzz (tuple ( intRange 1 20, intRange 1 20 )) "Random number and committe size" <|
            \( n, l ) ->
                let
                    res =
                        solve n (List.range 1 l)
                in
                if n <= l then
                    Expect.equal (res |> List.length) (factorial l // (factorial n * factorial (l - n)))

                else
                    Expect.equal res []
        ]
