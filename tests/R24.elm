module R24 exposing (suite)

import Array
import Expect
import Fuzz exposing (array, bool, float, floatRange, int, intRange, list, percentage, string, tuple, tuple3)
import P24 exposing (solve)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "P24"
        [ test "Zero end" <|
            \_ ->
                Expect.equal (solve 0 0) []
        , test "Zero elements" <|
            \_ ->
                Expect.equal (solve 0 100) []
        , fuzz (intRange 1 100) "Given number equal" <|
            \i ->
                Expect.equal (solve i i) (List.range 1 i)
        , fuzz (intRange 1 100) "Given number bigger" <|
            \i ->
                Expect.equal (solve (i + 1) i) []
        , fuzz (tuple ( intRange 1 100, intRange 1 100 )) "Random List" <|
            \( n, e ) ->
                let
                    res =
                        solve n e

                    isMember a =
                        List.member a (List.range 1 e)
                in
                if n > e then
                    Expect.equal res []

                else if n == e then
                    Expect.equal res (List.range 1 e)

                else
                    List.all isMember res
                        |> (&&) (List.length res == n)
                        |> Expect.true "Expected set inside other"
        ]
