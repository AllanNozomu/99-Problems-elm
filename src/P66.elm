module P66 exposing (solve, tree66)

import Html exposing (i)
import P55 exposing (Tree(..))
import P57
import P64 exposing (TreeNodePosition(..))



-- tree66 : Tree Char
-- tree66 =
--     P57.solve [ 'n', 'k', 'u', 'c', 'm', 'p', 'a', 'e', 'q', 'd', 'g' ]
-- tree66 : Tree Char
-- tree66 =
--     P57.solve [  'k',  'c', 'm',  'a', 'e',  'd', 'g' ]
-- tree66 : Tree Int
-- tree66 =
--     P57.solve [ 5, 3, 7, 4, 6 ]
-- tree66 : Tree Int
-- tree66 =
--     P57.solve [ 5, 3, 1, 7, 8 ]
-- tree66 : Tree Int
-- tree66 =
--     P57.solve [ 5, 1,2,3,9,8,7 ]


tree66 : Tree Int
tree66 =
    P57.solve [ 50, 25, 0, 10, 40, 30, 45, 75, 100, 90, 60, 55, 70 ]


solve : Tree a -> Tree (TreeNodePosition a)
solve tree =
    let
        unshiftedTree =
            solveTreePath 0 tree |> Tuple.second |> solveTree 0 0

        index =
            -(leftMostIndice unshiftedTree) + 1
    in
    shiftRightTree index unshiftedTree


maxDist : Int -> List ( Int, Int ) -> List ( Int, Int ) -> Int
maxDist i la lb =
    case ( la, lb ) of
        ( ( _, a ) :: aa, ( b, _ ) :: bb ) ->
            let
                now =
                    Basics.toFloat (a - b) / 2 |> Basics.floor |> (+) 1
            in
            max now (maxDist i aa bb)

        _ ->
            0


newListLeftRight : Int -> List ( Int, Int ) -> List ( Int, Int ) -> List ( Int, Int )
newListLeftRight i la lb =
    case ( la, lb ) of
        ( [], [] ) ->
            []

        ( a :: aa, b :: bb ) ->
            ( Tuple.first a - i, Tuple.second b + i ) :: newListLeftRight i aa bb

        ( ( a1, a2 ) :: aa, [] ) ->
            ( a1 - i, a2 + i ) :: newListLeftRight i aa []

        ( [], ( b1, b2 ) :: bb ) ->
            ( b1 - i, b2 + i ) :: newListLeftRight i [] bb


solveTreePath : Int -> Tree a -> ( List ( Int, Int ), Tree (TreeNodePosition a) )
solveTreePath i tree =
    case tree of
        Empty ->
            ( [], Empty )

        Branch x l r ->
            case ( l, r ) of
                ( Empty, Empty ) ->
                    ( [ ( i, i ) ], Branch (NodePosition x 0 0) Empty Empty )

                _ ->
                    let
                        ( lll, ll ) =
                            solveTreePath (i - 1) l

                        ( rrr, rr ) =
                            solveTreePath (i + 1) r

                        newI =
                            maxDist i lll rrr

                        newLR =
                            newListLeftRight newI lll rrr
                    in
                    ( ( i, i ) :: newLR, Branch (NodePosition x newI 0) ll rr )


solveTree : Int -> Int -> Tree (TreeNodePosition a) -> Tree (TreeNodePosition a)
solveTree i lv tree =
    case tree of
        Empty ->
            Empty

        Branch (NodePosition x c _) l r ->
            let
                ll =
                    solveTree (i - 1 - c) (lv + 1) l

                rr =
                    solveTree (i + 1 + c) (lv + 1) r
            in
            Branch (NodePosition x i (lv + 1)) ll rr


leftMostIndice : Tree (TreeNodePosition a) -> Int
leftMostIndice tree =
    case tree of
        Empty ->
            0

        Branch (NodePosition _ i _) ll _ ->
            min i (leftMostIndice ll)


shiftRightTree : Int -> Tree (TreeNodePosition a) -> Tree (TreeNodePosition a)
shiftRightTree i tree =
    case tree of
        Empty ->
            Empty

        Branch (NodePosition x c lv) l r ->
            let
                ll =
                    shiftRightTree i l

                rr =
                    shiftRightTree i r
            in
            Branch (NodePosition x (c + i) lv) ll rr
