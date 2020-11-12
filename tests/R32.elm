module R32 exposing (suite)

import Array
import Expect
import Fuzz exposing (array, bool, float, floatRange, int, intRange, list, percentage, string, tuple, tuple3)
import P32 exposing (solve)
import Test exposing (Test, describe, fuzz, test)


testCases : List ( Int, Int, Int )
testCases =
    [ ( 1, 9, 1 )
    , ( 0, 0, 0 )
    , ( 36, 63, 9 )
    , ( 0, 12, 12 )
    , ( 19, 0, 19 )
    , ( 6, 8, 2 )
    , ( 9, 27, 9 )
    , ( 13, 13, 13 )
    , ( 2, 15, 1 )
    , ( 37, 600, 1 )
    , ( 42, 56, 14 )
    , ( 461952, 116298, 18 )
    , ( 897221562, 63423576, 6 )
    , ( 7966496, 314080416, 32 )
    , ( 24826148, 45296490, 526 )
    ]


suite : Test
suite =
    describe "P32"
        (List.map
            (\( i1, i2, out ) ->
                test ("GCD " ++ String.fromInt i1 ++ " " ++ String.fromInt i2 ++ " = " ++ String.fromInt out) <|
                    \_ -> Expect.equal (solve i1 i2) out
            )
            testCases
        )
