module P50 exposing (..)

import Array exposing (Array)
import Basics
import Html exposing (a)


type alias CmpFuntion a =
    a -> a -> Basics.Order


type alias MinHeap a =
    { size : Int
    , arr : Array a
    , cmpFunction : CmpFuntion a
    }


minHeapParent : Int -> Int
minHeapParent n =
    (n - 1) // 2


minHeapLeft : Int -> Int
minHeapLeft n =
    n * 2 + 1


minHeapRight : Int -> Int
minHeapRight n =
    n * 2 + 2


heapCompare : Int -> Int -> CmpFuntion a -> Array a -> Bool
heapCompare i j f arr =
    case ( Array.get i arr, Array.get j arr ) of
        ( Just ii, Just jj ) ->
            f ii jj == Basics.LT

        _ ->
            False


arrSwap : Int -> Int -> Array a -> Array a
arrSwap i j arr =
    case ( Array.get i arr, Array.get j arr ) of
        ( Just ii, Just jj ) ->
            Array.set i jj arr |> Array.set j ii

        _ ->
            arr


minHeapCreate : (a -> a -> Basics.Order) -> MinHeap a
minHeapCreate f =
    { size = 0
    , arr = Array.empty
    , cmpFunction = f
    }


minHeapPeek : MinHeap a -> Maybe a
minHeapPeek h =
    if h.size == 0 then
        Nothing

    else
        Array.get 0 h.arr


minHeapInsert : a -> MinHeap a -> MinHeap a
minHeapInsert value heap =
    let
        insertAux : Int -> Array a -> Array a
        insertAux n arr =
            if n == 0 then
                arr

            else
                let
                    parent =
                        minHeapParent n
                in
                if heapCompare n parent heap.cmpFunction arr then
                    insertAux parent (arrSwap n parent arr)

                else
                    arr

        newArr =
            if Array.length heap.arr <= heap.size then
                Array.push value heap.arr |> insertAux heap.size

            else
                Array.set heap.size value heap.arr |> insertAux heap.size
    in
    { heap | size = heap.size + 1, arr = newArr }


minHeapRemove : MinHeap a -> MinHeap a
minHeapRemove heap =
    let
        newSize =
            heap.size - 1

        removeAux : Int -> Array a -> Array a
        removeAux n arr =
            let
                left =
                    minHeapLeft n

                right =
                    minHeapRight n
            in
            case ( left <= (newSize - 1), right <= (newSize - 1) ) of
                ( True, True ) ->
                    if heapCompare left right heap.cmpFunction arr && heapCompare left n heap.cmpFunction arr then
                        removeAux left (arrSwap left n arr)

                    else if heapCompare right n heap.cmpFunction arr then
                        removeAux right (arrSwap right n arr)

                    else
                        arr

                ( True, False ) ->
                    if heapCompare left n heap.cmpFunction arr then
                        removeAux left (arrSwap left n arr)

                    else
                        arr

                _ ->
                    arr

        newArr =
            arrSwap 0 newSize heap.arr |> removeAux 0
    in
    { heap | size = newSize, arr = newArr }


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
            List.map createNode l |> List.foldl (\n acc -> minHeapInsert n acc) (minHeapCreate cmpHuffman) |> solveAux
    in
    case minHeapPeek heap of
        Just hd ->
            hd.nodes

        Nothing ->
            []


solveAux : MinHeap HuffmanData -> MinHeap HuffmanData
solveAux heap =
    case ( minHeapPeek heap, minHeapRemove heap |> minHeapPeek ) of
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
            solveAux (minHeapRemove heap |> minHeapRemove |> minHeapInsert { newNode | nodes = newNodes })

        _ ->
            heap
