module P67 exposing (solveA, solveB)

import P55 exposing (Tree(..))
import P64 exposing (TreeNodePosition(..))


tree67 : Tree Char
tree67 =
    Branch 'a' (Branch 'b' (Branch 'd' Empty Empty) (Branch 'e' Empty Empty)) (Branch 'c' Empty (Branch 'f' (Branch 'g' Empty Empty) Empty))


solveA : Tree a -> (a -> String) -> String
solveA tree f =
    case tree of
        Branch x Empty Empty ->
            f x

        Branch x l r ->
            let
                ll =
                    solveA l f

                rr =
                    solveA r f
            in
            f x ++ "(" ++ ll ++ "," ++ rr ++ ")"

        Empty ->
            ""


separateLeft : List Char -> Int -> List Char
separateLeft s n =
    case ( s, n ) of
        ( '(' :: l, nn ) ->
            '(' :: separateLeft l (nn + 1)

        ( ')' :: l, nn ) ->
            ')' :: separateLeft l (nn - 1)

        ( ',' :: l, 0 ) ->
            []

        ( x :: l, nn ) ->
            x :: separateLeft l nn

        ( [], _ ) ->
            []


separateLeftRight : List Char -> ( List Char, List Char )
separateLeftRight s =
    let
        left =
            separateLeft s 0
    in
    ( left, List.take (List.length s - 1) s |> List.drop (List.length left + 1) )


finishesWithClosing : List Char -> Bool
finishesWithClosing s =
    case List.reverse s of
        ')' :: _ ->
            True

        _ ->
            False


solveB : String -> Maybe (Tree Char)
solveB s =
    let
        solveBAux res ls =
            case ls of
                [] ->
                    ( res && True, Empty )

                [ a ] ->
                    ( res && True, Branch a Empty Empty )

                a :: '(' :: ss ->
                    let
                        ( l, r ) =
                            separateLeftRight ss

                        ( resl, ll ) =
                            solveBAux res l

                        ( resr, rr ) =
                            solveBAux res r

                        fres =
                            resl && resr && finishesWithClosing ss
                    in
                    ( fres, Branch a ll rr )

                _ ->
                    ( False, Empty )
    in
    case String.toList s |> solveBAux True of
        ( True, res ) ->
            Just res

        ( False, _ ) ->
            Nothing
