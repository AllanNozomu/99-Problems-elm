module P64 exposing (TreeNodePosition(..), solve, tree64)

import P55 exposing (Tree(..))
import P57


type TreeNodePosition x
    = NodePosition x Int Int


tree64 : Tree Char
tree64 =
    P57.solve [ 'n', 'k', 'u', 'c', 'm', 'p', 'a', 'h', 's', 'g', 'q', 'e' ]


solve : Tree a -> Tree (TreeNodePosition a)
solve tree =
    solveLevel 1 tree |> solveInOrder 1 |> Tuple.second


solveLevel : Int -> Tree a -> Tree (TreeNodePosition a)
solveLevel level tree =
    case tree of
        Empty ->
            Empty

        Branch x l r ->
            let
                ll =
                    solveLevel (level + 1) l

                rr =
                    solveLevel (level + 1) r
            in
            Branch (NodePosition x 0 level) ll rr


solveInOrder : Int -> Tree (TreeNodePosition a) -> ( Int, Tree (TreeNodePosition a) )
solveInOrder n tree =
    case tree of
        Empty ->
            ( n, Empty )

        Branch (NodePosition x _ lv) l r ->
            let
                ( nl, ll ) =
                    solveInOrder n l

                ( nr, rr ) =
                    solveInOrder (nl + 1) r
            in
            ( nr, Branch (NodePosition x nl lv) ll rr )
