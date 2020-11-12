module R33 exposing (suite)

import Array
import Expect
import Fuzz exposing (array, bool, float, floatRange, int, intRange, list, percentage, string, tuple, tuple3)
import P33 exposing (solve)
import Test exposing (Test, describe, fuzz, test)


testCases : List ( Int, Int, Bool )
testCases =
    [ ( 2, 4, False )
    , ( 1, 1, True )
    , ( 10, 5, False )
    , ( 21, 22, True )
    , ( 21, 24, False )
    , ( 500, 499, True )
    , ( 500, 9, True )
    , ( 500, 349, True )
    , ( 35, 64, True )
    , ( 1024, 512, False )
    ]


boolToString : Bool -> String
boolToString b =
    case b of
        True ->
            "True"

        _ ->
            "False"


suite : Test
suite =
    describe "P33"
        (List.map
            (\( i1, i2, out ) ->
                test ("Coprime " ++ String.fromInt i1 ++ " " ++ String.fromInt i2 ++ " = " ++ boolToString out) <|
                    \_ -> Expect.equal (solve i1 i2) out
            )
            testCases
        )
