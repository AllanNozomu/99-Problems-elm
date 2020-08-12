module R06 exposing (suite)

import Expect
import Fuzz exposing (int, list, string)
import P06 exposing (solve)
import Test exposing (Test, describe, fuzz, test)


suite : Test
suite =
    describe "P06"
        [ test "Empty list" <|
            \_ ->
                Expect.equal (solve []) True
        , test "One char" <|
            \_ ->
                Expect.equal (solve [1]) True
        , test "Simple palindrome" <|
            \_ ->
                Expect.equal (solve [1,2,1]) True
        , test "Simple not palindrome" <|
            \_ ->
                Expect.equal (solve [1,2,3,4,5]) False
        , fuzz (list int) "Random Lists" <|
            \randomList ->
                Expect.equal (solve randomList) (List.reverse randomList == randomList )
        , fuzz (list int) "Random Palindrome Lists" <|
            \randomList ->
                Expect.equal (solve (randomList ++ List.reverse randomList)) True
        ]
