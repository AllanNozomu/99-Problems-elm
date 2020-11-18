module P50 exposing (solve)

import Array exposing (Array)
import DataStructures.MinHeap as MinHeap


type Node
    = Node HuffmanData
    | None


type alias HuffmanData =
    { c : String
    , qty : Int
    , nodes : List ( String, String )
    }


cmpFunction : Int -> Int -> Basics.Order
cmpFunction =
    Basics.compare


createNode : ( String, Int ) -> HuffmanData
createNode ( s, i ) =
    HuffmanData s i []


cmpHuffman : HuffmanData -> HuffmanData -> Basics.Order
cmpHuffman a b =
    case Basics.compare a.qty b.qty of
        Basics.EQ ->
            Basics.compare (String.length a.c) (String.length b.c)

        Basics.GT ->
            Basics.GT

        Basics.LT ->
            Basics.LT


solve : List ( String, Int ) -> List ( String, String )
solve l =
    let
        heap =
            List.map createNode l |> List.foldl (\n acc -> MinHeap.insert n acc) (MinHeap.create cmpHuffman) |> solveAux
    in
    case MinHeap.peek heap of
        Just hd ->
            hd.nodes

        Nothing ->
            []


solveAux : MinHeap.MinHeap HuffmanData -> MinHeap.MinHeap HuffmanData
solveAux heap =
    case ( MinHeap.peek heap, MinHeap.remove heap |> MinHeap.peek ) of
        ( Just newLeft, Just newRight ) ->
            let
                newNodes =
                    case ( newLeft.nodes, newRight.nodes ) of
                        ( [], [] ) ->
                            [ ( newLeft.c, "0" ), ( newRight.c, "1" ) ]

                        ( [], _ ) ->
                            ( newLeft.c, "0" ) :: List.map (\( c, s ) -> ( c, "1" ++ s )) newRight.nodes

                        ( _, [] ) ->
                            ( newRight.c, "0" ) :: List.map (\( c, s ) -> ( c, "0" ++ s )) newLeft.nodes

                        _ ->
                            List.map (\( c, s ) -> ( c, "0" ++ s )) newLeft.nodes ++ List.map (\( c, s ) -> ( c, "1" ++ s )) newRight.nodes

                newNode =
                    createNode ( newLeft.c ++ newRight.c, newLeft.qty + newRight.qty )
            in
            solveAux (MinHeap.remove heap |> MinHeap.remove |> MinHeap.insert { newNode | nodes = newNodes })

        _ ->
            heap
