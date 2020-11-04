module R28 exposing (suite)

import Array
import Expect
import Fuzz exposing (array, bool, float, floatRange, int, intRange, list, percentage, string, tuple, tuple3)
import P28 exposing (solve)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "P28"
        [ test "Empty List" <|
            \_ ->
                Expect.equal (solve []) []
        , test "One element" <|
            \_ ->
                Expect.equal (solve [ [ 1 ] ]) [ [ 1 ] ]
        , fuzz (list (intRange 0 20)) "Random number in range" <|
            \ll ->
                let
                    l =
                        List.map (\n -> List.repeat n n) ll
                in
                Expect.equal (solve l |> List.map List.length) (List.map List.length l |> List.sort)
        ]
