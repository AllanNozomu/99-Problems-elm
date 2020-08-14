module R20 exposing (suite)

import Array
import Expect
import Fuzz exposing (array, int, intRange, list, string, tuple)
import P20 exposing (solve)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "P20"
        [ test "Empty list" <|
            \_ ->
                Expect.equal (solve [] 1) ( [], Nothing )
        , test "Remove 0" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3 ] 0) ( [ 1, 2, 3 ], Nothing )
        , test "Remove negative index" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3 ] -100) ( [ 1, 2, 3 ], Nothing )
        , test "Remove idnex larger than list" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3 ] 1000) ( [ 1, 2, 3 ], Nothing )
        , test "Remove first element" <|
            \_ ->
                Expect.equal (solve [ 1, 2, 3, 4 ] 1) ( [ 2, 3, 4 ], Just 1 )
        , test "Remove last element" <|
            \_ ->
                Expect.equal (solve [ 1, 4, 3, 2 ] 4) ( [ 1, 4, 3 ], Just 2 )
        , fuzz (tuple ( array int, intRange 1 100 )) "Random List and index" <|
            \( randomArray, k ) ->
                let
                    ( resList, res ) =
                        if Array.length randomArray < k then
                            ( Array.toList randomArray, Nothing )

                        else
                            ( Array.toList (Array.slice 0 (k - 1) randomArray) ++ Array.toList (Array.slice k (Array.length randomArray + 1) randomArray)
                            , Array.get (k - 1) randomArray
                            )
                in
                Expect.equal (solve (Array.toList randomArray) k) ( resList, res )
        ]
