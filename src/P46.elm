module P46 exposing (solve)


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


solve : (Bool -> Bool -> Bool) -> List ( ( Bool, Bool ), Bool )
solve expr =
    let
        allValeus =
            [ ( True, True ), ( True, False ), ( False, True ), ( False, False ) ]
    in
    List.map
        (\( left, right ) ->
            ( ( left, right ), expr left right )
        )
        allValeus
