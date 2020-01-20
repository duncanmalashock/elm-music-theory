module MusicTheory.Tuplet exposing
    ( Tuplet
    , duplet
    , quadruplet
    , quintuplet
    , quintupletOverFour
    , quintupletOverThree
    , sextuplet
    , sextupletOverFour
    , split
    , toRational
    , triplet
    )

import Libs.Ratio as Ratio exposing (Rational)


type Tuplet
    = Tuplet Int Int


duplet : Tuplet
duplet =
    Tuplet 2 3


triplet : Tuplet
triplet =
    Tuplet 3 2


quadruplet : Tuplet
quadruplet =
    Tuplet 4 3


quintuplet : Tuplet
quintuplet =
    Tuplet 5 2


quintupletOverThree : Tuplet
quintupletOverThree =
    Tuplet 5 3


quintupletOverFour : Tuplet
quintupletOverFour =
    Tuplet 5 4


sextuplet : Tuplet
sextuplet =
    Tuplet 6 2


sextupletOverFour : Tuplet
sextupletOverFour =
    Tuplet 6 4


split : Tuplet -> ( Int, Int )
split (Tuplet x y) =
    ( x, y )


toRational : Tuplet -> Rational
toRational (Tuplet x y) =
    Ratio.over x y
