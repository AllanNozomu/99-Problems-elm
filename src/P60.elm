module P60 exposing (solve)

import P55 exposing (Tree(..))


minNodes : Int -> Int
minNodes h =
    case h of
        0 ->
            0

        1 ->
            1

        _ ->
            minNodes (h - 1) + minNodes (h - 2) + 1


maxNodes : Int -> Int
maxNodes h =
    2 ^ h - 1


minHeight : Int -> Int
minHeight n =
    n
        + 1
        |> Basics.toFloat
        |> Basics.logBase 2
        |> Basics.ceiling


maxHeight : Int -> Int
maxHeight n =
    let
        maxHeigthAux : Int -> Int -> Int
        maxHeigthAux nn res =
            if minNodes res > nn then
                res - 1

            else
                maxHeigthAux nn (res + 1)
    in
    maxHeigthAux n 0


zipLists : List a -> List a -> List ( a, a )
zipLists la lb =
    case la of
        a :: aa ->
            List.foldl (\b acc -> ( a, b ) :: acc) [] lb ++ zipLists aa lb

        _ ->
            []


solve : comparable -> Int -> List (Tree comparable)
solve val n =
    List.range (minHeight n) (maxHeight n)
        |> List.map (solveAux val n)
        |> List.concat


solveAux : comparable -> Int -> Int -> List (Tree comparable)
solveAux val n h =
    case h of
        0 ->
            [ Empty ]

        1 ->
            [ Branch val Empty Empty ]

        _ ->
            [ ( h - 2, h - 1 ), ( h - 1, h - 1 ), ( h - 1, h - 2 ) ]
                |> List.foldl
                    (\( hl, hr ) hAcc ->
                        let
                            minl =
                                max (minNodes hl) (n - 1 - maxNodes hr)

                            maxl =
                                min (maxNodes hl) (n - 1 - minNodes hr)
                        in
                        List.range minl maxl
                            |> List.foldl
                                (\nl nlAcc ->
                                    let
                                        nr =
                                            n - 1 - nl

                                        ll =
                                            solveAux val nl hl

                                        rr =
                                            solveAux val nr hr
                                    in
                                    List.foldl (\( l, r ) acc -> Branch val l r :: acc) [] (zipLists ll rr)
                                        |> (++) nlAcc
                                )
                                []
                            |> (++) hAcc
                    )
                    []
