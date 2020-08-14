module P17 exposing (solve)


solve : List a -> Int -> ( List a, List a )
solve list n =
    case ( list, n ) of
        ( [], _ ) ->
            ( [], [] )

        ( _, 0 ) ->
            ( [], list )

        _ ->
            if n < 0 then
                ( [], [] )

            else
                solveAux list n


solveAux : List a -> Int -> ( List a, List a )
solveAux list n =
    case ( list, n ) of
        ( [], _ ) ->
            ( [], [] )

        ( _, 0 ) ->
            ( [], list )

        ( x :: r, _ ) ->
            let
                ( ll, rr ) =
                    solveAux r (n - 1)
            in
            ( x :: ll, rr )
