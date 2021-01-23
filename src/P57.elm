module P57 exposing (solve)

import P55 exposing (Tree(..))


solve : List comparable -> Tree comparable
solve =
    construct Empty


addNode : comparable -> Tree comparable -> Tree comparable
addNode n tree =
    case tree of
        Empty ->
            Branch n Empty Empty

        Branch x l r ->
            if n < x then
                Branch x (addNode n l) r

            else
                Branch x l (addNode n r)


construct : Tree comparable -> List comparable -> Tree comparable
construct tree l =
    case l of
        [] ->
            tree

        x :: s ->
            construct (addNode x tree) s
