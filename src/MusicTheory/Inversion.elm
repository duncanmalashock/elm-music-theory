module MusicTheory.Inversion exposing
    ( Inversion
    , fifth
    , first
    , fourth
    , fromInt
    , none
    , second
    , seventh
    , sixth
    , third
    , toInt
    )


type Inversion
    = Inversion Int


fromInt : Int -> Inversion
fromInt inv =
    Inversion inv


toInt : Inversion -> Int
toInt (Inversion inv) =
    inv


none : Inversion
none =
    fromInt 0


first : Inversion
first =
    fromInt 1


second : Inversion
second =
    fromInt 2


third : Inversion
third =
    fromInt 3


fourth : Inversion
fourth =
    fromInt 4


fifth : Inversion
fifth =
    fromInt 5


sixth : Inversion
sixth =
    fromInt 6


seventh : Inversion
seventh =
    fromInt 7
