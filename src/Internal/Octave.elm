module Internal.Octave exposing
    ( Octave
    , allValid
    , zero, one, two, three, four, five, six, seven, eight
    , semitones
    , add
    , octave
    , number
    , toInterval, toString
    )

{-|

@docs Octave

@docs allValid

@docs zero, one, two, three, four, five, six, seven, eight

@docs semitones

@docs add

@docs octave
@docs number

@docs toInterval, toString

-}

import Internal.Interval as Interval


type Octave
    = Octave Int


octave : Int -> Octave
octave n =
    Octave n


add : Int -> Octave -> Octave
add n (Octave o) =
    octave (o + n)


number : Octave -> Int
number (Octave o) =
    o


allValid : List Octave
allValid =
    [ octave 0
    , octave 1
    , octave 2
    , octave 3
    , octave 4
    , octave 5
    , octave 6
    , octave 7
    , octave 8
    , octave 9
    ]


toInterval : Octave -> Interval.Interval
toInterval (Octave int) =
    List.foldl
        (always Interval.addOctave)
        Interval.perfectUnison
        (List.repeat int ())


semitones : Octave -> Int
semitones (Octave o) =
    o * 12


zero : Octave
zero =
    Octave 0


one : Octave
one =
    Octave 1


two : Octave
two =
    Octave 2


three : Octave
three =
    Octave 3


four : Octave
four =
    Octave 4


five : Octave
five =
    Octave 5


six : Octave
six =
    Octave 6


seven : Octave
seven =
    Octave 7


eight : Octave
eight =
    Octave 8


toString : Octave -> String
toString (Octave o) =
    String.fromInt o
