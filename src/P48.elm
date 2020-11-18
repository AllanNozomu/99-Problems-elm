module P48 exposing (..)

import Bitwise exposing (shiftLeftBy)
import List


and : Bool -> Bool -> Bool
and =
    (&&)


or : Bool -> Bool -> Bool
or =
    (||)


nand : Bool -> Bool -> Bool
nand a b =
    and a b |> not


nor : Bool -> Bool -> Bool
nor a b =
    or a b |> not


xor : Bool -> Bool -> Bool
xor a b =
    equ a b |> not


impl : Bool -> Bool -> Bool
impl a b =
    not a || b


equ : Bool -> Bool -> Bool
equ =
    (==)


allValues : Int -> List (List Bool)
allValues n =
    let
        getValue x =
            List.repeat n x
                |> List.indexedMap
                    (\i y ->
                        Bitwise.and y (shiftLeftBy (n - i - 1) 1) == 0
                    )
    in
    List.range 0 (shiftLeftBy n 1 - 1) |> List.map getValue


solve : Int -> (List Bool -> Bool) -> List ( List Bool, Bool )
solve n expr =
    List.map
        (\params ->
            ( params, expr params )
        )
        (allValues n)
