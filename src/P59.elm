module P59 exposing (solve, zipLists)

import Dict exposing (Dict)
import P55 exposing (Tree(..))


zipLists : List a -> List a -> List ( a, a )
zipLists la lb =
    case la of
        a :: aa ->
            List.foldl (\b acc -> ( a, b ) :: acc) [] lb ++ zipLists aa lb

        _ ->
            []


solve : comparable -> Int -> List (Tree comparable)
solve val n =
    solveWithMemo val n Dict.empty
        |> Tuple.second


solveWithMemo : comparable -> Int -> Dict Int (List (Tree comparable)) -> ( Dict Int (List (Tree comparable)), List (Tree comparable) )
solveWithMemo val h memo =
    case h of
        0 ->
            ( Dict.singleton 0 [ Empty ], [ Empty ] )

        1 ->
            ( Dict.singleton 0 [ Branch val Empty Empty ], [ Branch val Empty Empty ] )

        _ ->
            case Dict.get h memo of
                Just l ->
                    ( memo, l )

                Nothing ->
                    let
                        ( memo1, lh1 ) =
                            solveWithMemo val (h - 1) memo

                        ( memo2, lh2 ) =
                            solveWithMemo val (h - 2) memo1

                        res =
                            List.map (\( l, r ) -> Branch val l r) (zipLists lh2 lh1 ++ zipLists lh1 lh1 ++ zipLists lh1 lh2)
                    in
                    ( Dict.insert h res memo2, res )
