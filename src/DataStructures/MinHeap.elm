module DataStructures.MinHeap exposing (MinHeap, create, insert, peek, remove)

import Array exposing (Array)


type alias CmpFuntion a =
    a -> a -> Basics.Order


type alias MinHeap a =
    { size : Int
    , arr : Array a
    , cmpFunction : CmpFuntion a
    }


parentIndex : Int -> Int
parentIndex n =
    (n - 1) // 2


leftIndex : Int -> Int
leftIndex n =
    n * 2 + 1


rightIndex : Int -> Int
rightIndex n =
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


create : (a -> a -> Basics.Order) -> MinHeap a
create f =
    { size = 0
    , arr = Array.empty
    , cmpFunction = f
    }


peek : MinHeap a -> Maybe a
peek h =
    if h.size == 0 then
        Nothing

    else
        Array.get 0 h.arr


insert : a -> MinHeap a -> MinHeap a
insert value heap =
    let
        insertAux : Int -> Array a -> Array a
        insertAux n arr =
            if n == 0 then
                arr

            else
                let
                    parent =
                        parentIndex n
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


remove : MinHeap a -> MinHeap a
remove heap =
    let
        newSize =
            heap.size - 1

        removeAux : Int -> Array a -> Array a
        removeAux n arr =
            let
                left =
                    leftIndex n

                right =
                    rightIndex n
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
