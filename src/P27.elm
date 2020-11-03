module P27 exposing (solve)

import Array exposing (Array)


solve : List a -> List Int -> List (List (List a))
solve people groupsSize =
    if List.sum groupsSize /= List.length people || List.isEmpty people then
        []

    else
        solveAux people (Array.fromList groupsSize) (List.sum groupsSize) (Array.initialize (List.length groupsSize) (\_ -> []))


solveAux : List a -> Array Int -> Int -> Array (List a) -> List (List (List a))
solveAux people groupsSize n acc =
    if n == 0 then
        [ Array.toList acc ]

    else
        case people of
            [] ->
                []

            person :: others ->
                Array.indexedMap
                    (\i size ->
                        if size <= 0 then
                            []

                        else
                            solveAux others (arrayDec i groupsSize) (n - 1) (arrayPushPerson i person acc)
                    )
                    groupsSize
                    |> Array.toList
                    |> List.concat


arrayDec : Int -> Array Int -> Array Int
arrayDec n a =
    case Array.get n a of
        Just x ->
            Array.set n (x - 1) a

        Nothing ->
            a


arrayPushPerson : Int -> a -> Array (List a) -> Array (List a)
arrayPushPerson n person a =
    case Array.get n a of
        Just x ->
            Array.set n (person :: x) a

        Nothing ->
            a
