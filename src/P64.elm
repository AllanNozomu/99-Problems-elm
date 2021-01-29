module P64 exposing (solve, tree64)

import P55 exposing (Tree(..))
import P57


type TreeNodePosition x
    = NodePosition x Int Int


tree64 : Tree Char
tree64 =
    P57.solve [ 'n', 'k', 'u', 'c', 'm', 'p', 'a', 'h', 's', 'g', 'q', 'e' ]


solve : Tree a -> Tree (TreeNodePosition a)
solve tree =
    solveInOrder 0 tree |> Tuple.second |> solveLevel 1


solveInOrder : Int -> Tree a -> ( Int, Tree (TreeNodePosition a) )
solveInOrder n tree =
    case tree of
        Empty ->
            ( n, Empty )

        Branch x l r ->
            let
                ( nl, ll ) =
                    solveInOrder n l

                ( nr, rr ) =
                    solveInOrder (nl + 1) r
            in
            ( nr, Branch (NodePosition x (nl + 1) 0) ll rr )


solveLevel : Int -> Tree (TreeNodePosition a) -> Tree (TreeNodePosition a)
solveLevel level tree =
    case tree of
        Empty ->
            Empty

        Branch (NodePosition x a _) l r ->
            let
                ll =
                    solveLevel (level + 1) l

                rr =
                    solveLevel (level + 1) r
            in
            Branch (NodePosition x a level) ll rr
