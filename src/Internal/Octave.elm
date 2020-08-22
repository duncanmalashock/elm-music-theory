module Internal.Octave exposing
    ( Octave
    , add
    , allValid
    , eight
    , five
    , four
    , number
    , octave
    , one
    , semitones
    , seven
    , six
    , three
    , toString
    , two
    , zero
    )


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
    ]


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
