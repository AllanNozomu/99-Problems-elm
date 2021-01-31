module P65 exposing (solve, tree65, treeHeight)

import P55 exposing (Tree(..))
import P57
import P64 exposing (TreeNodePosition(..))


tree65 : Tree Char
tree65 =
    P57.solve [ 'n', 'k', 'u', 'c', 'm', 'p', 'a', 'e', 'q', 'd', 'g' ]


treeHeight : Tree a -> Int
treeHeight tree =
    case tree of
        Empty ->
            0

        Branch _ l r ->
            1 + max (treeHeight l) (treeHeight r)


solve : Tree a -> Tree (TreeNodePosition a)
solve tree =
    let
        unshiftedTree =
            solveLevel 1 tree |> solveInOrder (2 ^ (treeHeight tree - 1)) (treeHeight tree - 1)

        amount =
            getAmount unshiftedTree
    in
    shiftLeft amount unshiftedTree


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


solveInOrder : Int -> Int -> Tree (TreeNodePosition a) -> Tree (TreeNodePosition a)
solveInOrder n invLv tree =
    case tree of
        Empty ->
            Empty

        Branch (NodePosition x _ lv) l r ->
            let
                currw =
                    2 ^ (invLv - 1)

                ll =
                    solveInOrder (n - currw) (invLv - 1) l

                rr =
                    solveInOrder (n + currw) (invLv - 1) r
            in
            Branch (NodePosition x n lv) ll rr


getAmount : Tree (TreeNodePosition a) -> Int
getAmount tree =
    case tree of
        Empty ->
            -1

        Branch (NodePosition _ n _) l _ ->
            case l of
                Empty ->
                    n - 1

                _ ->
                    getAmount l


shiftLeft : Int -> Tree (TreeNodePosition a) -> Tree (TreeNodePosition a)
shiftLeft amount tree =
    case tree of
        Empty ->
            Empty

        Branch (NodePosition x n lv) l r ->
            let
                ll =
                    shiftLeft amount l

                rr =
                    shiftLeft amount r
            in
            Branch (NodePosition x (n - amount) lv) ll rr
