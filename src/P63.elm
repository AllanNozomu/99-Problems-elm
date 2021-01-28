module P63 exposing (isCompleteBinaryTree, solve)

import P55 exposing (Tree(..))


solve : Int -> Tree Int
solve n =
    let
        solveAux : Int -> Tree Int
        solveAux curr =
            if curr > n then
                Empty

            else
                let
                    l =
                        solveAux (2 * curr)

                    r =
                        solveAux (2 * curr + 1)
                in
                Branch curr l r
    in
    if n <= 0 then
        Empty

    else
        solveAux 1


isCompleteBinaryTree : Tree comparable -> Bool
isCompleteBinaryTree tree =
    case tree of
        Empty ->
            True

        Branch x l r ->
            case ( l, r ) of
                ( Empty, Branch _ _ _ ) ->
                    False

                ( Empty, Empty ) ->
                    True

                _ ->
                    isCompleteBinaryTree l && isCompleteBinaryTree r
