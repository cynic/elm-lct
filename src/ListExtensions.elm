module ListExtensions exposing (first,window)

first : (a -> Bool) -> List a -> Maybe a
first predicate list =
    case list of
        [] ->
            Nothing
        h::rest ->
            if predicate h then
                Just h
            else
                first predicate rest

window : Int -> List a -> List (List a)
window n list =
    let
        taken = List.take n list
    in
        if n <= 0 || List.length taken < n then
            []
        else
            Maybe.map
                (\rest -> taken :: window n rest)
                (List.tail list)
            |> Maybe.withDefault []