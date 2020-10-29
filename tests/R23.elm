module R23 exposing (suite)

import P23 exposing (solve)
import Array
import Expect
import Fuzz exposing (array, bool, float, floatRange, int, intRange, list, percentage, string, tuple, tuple3)
import Test exposing (Test, describe, fuzz, test)
import Bitwise exposing (and)


suite : Test
suite =
    describe "P23"
        [ test "Empty list" <|
            \_ ->
                Expect.equal (solve [] 1) []
        , test "Zero elements" <|
            \_ ->
                Expect.equal (solve [1,2,3] 0) []
        , fuzz (list int) "Given number equal" <|
            \l ->
                Expect.equal (solve l (List.length l)) l
        , fuzz (list int) "Given number bigger" <|
            \l ->
                Expect.equal (solve l (List.length l * 2)) l
        , fuzz (tuple ( list int, intRange 0 100 )) "Random List" <|
            \( l, n ) ->
                let
                    res = solve l n
                    isMember a = 
                        List.member a l
                in
                (List.all isMember) res   
                |> (&&) (List.length res == Basics.min (List.length l) n)
                |> Expect.true "Expected set inside other"
        ]
