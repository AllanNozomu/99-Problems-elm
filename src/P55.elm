module P55 exposing (Tree(..), solve)


type Tree comparable
    = Empty
    | Branch comparable (Tree comparable) (Tree comparable)


leaf : comparable -> Tree comparable
leaf x =
    Branch x Empty Empty


decapsulate : List (List (Tree comparable)) -> List (Tree comparable)
decapsulate =
    List.map
        (\ll ->
            case ll of
                [] ->
                    Empty

                lll :: _ ->
                    lll
        )


solve : Int -> List (Tree String)
solve n =
    case n of
        0 ->
            []

        _ ->
            let
                level =
                    lastLevel n 0

                allLastLevels =
                    generateLastLevel (n - (2 ^ level - 1)) (2 ^ level) []
            in
            List.map (\ll -> generateLevel 0 n ll) allLastLevels
                |> decapsulate


lastLevel : Int -> Int -> Int
lastLevel n level =
    if n <= 2 ^ level then
        level

    else
        lastLevel (n - (2 ^ level)) (level + 1)


generateLastLevel : Int -> Int -> List (Tree String) -> List (List (Tree String))
generateLastLevel remaining size acc =
    if size == 0 then
        if remaining == 0 then
            [ acc ]

        else
            []

    else if remaining == 0 then
        generateLastLevel 0 (size - 1) (Empty :: acc)

    else
        generateLastLevel remaining (size - 1) (Empty :: acc)
            ++ generateLastLevel (remaining - 1) (size - 1) (leaf "x" :: acc)


mergeLevel : List (Tree String) -> List (Tree String) -> List (Tree String)
mergeLevel parents children =
    case ( parents, children ) of
        ( p :: ps, c1 :: c2 :: cs ) ->
            case p of
                Branch val _ _ ->
                    Branch val c1 c2 :: mergeLevel ps cs

                _ ->
                    []

        _ ->
            []


generateLevel : Int -> Int -> List (Tree String) -> List (Tree String)
generateLevel level remaining ll =
    if 2 ^ level < remaining then
        let
            curr =
                List.repeat (2 ^ level) (leaf "x")

            children =
                generateLevel (level + 1) (remaining - 2 ^ level) ll
        in
        mergeLevel curr children

    else
        ll
