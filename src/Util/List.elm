module Util.List exposing (rotateLeft)


rotateLeft : Int -> List a -> List a
rotateLeft amount list =
    if amount <= 0 then
        list

    else
        case list of
            [] ->
                []

            x :: xs ->
                rotateLeft (amount - 1) (xs ++ [ x ])
