module R07 exposing (suite)

import Expect
import Fuzz exposing (int, list, string)
import P07 exposing (NestedList(..), solve)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "P07"
        [ test "Empty nestedList" <|
            \_ ->
                let
                    a =
                        SubList []
                in
                Expect.equal (solve a) []
        , test "One element" <|
            \_ ->
                let
                    a =
                        Elem 1
                in
                Expect.equal (solve a) [ 1 ]
        , test "Two elements" <|
            \_ ->
                let
                    a =
                        SubList [ Elem 1, Elem 2 ]
                in
                Expect.equal (solve a) [ 1, 2 ]
        , test "Many elements" <|
            \_ ->
                let
                    a =
                        SubList [ Elem 1, Elem 2, SubList [ SubList [ Elem 3, SubList [ Elem 4 ], Elem 5, SubList [ SubList [], Elem 6, SubList [ Elem 7 ] ] ] ] ]
                in
                Expect.equal (solve a) [ 1, 2, 3, 4, 5, 6, 7 ]
        ]
