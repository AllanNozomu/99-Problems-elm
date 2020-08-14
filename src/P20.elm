module P20 exposing (solve)


solve : List a -> Int -> ( List a, Maybe a )
solve list n =
    case ( list, n ) of
        ( [], _ ) ->
            ( [], Nothing )

        ( _, 0 ) ->
            ( list, Nothing )

        _ ->
            if n < 0 then
                ( list, Nothing )

            else
                solveAux list n


solveAux : List a -> Int -> ( List a, Maybe a )
solveAux list n =
    case ( list, n ) of
        ( [], _ ) ->
            ( [], Nothing )

        ( x :: r, 1 ) ->
            ( r, Just x )

        ( x :: r, _ ) ->
            let
                ( rlist, res ) =
                    solveAux r (n - 1)
            in
            ( x :: rlist, res )
