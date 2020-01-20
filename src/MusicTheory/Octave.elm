module MusicTheory.Octave exposing
    ( Octave
    , OctaveError(..)
    , add
    , all
    , eight
    , errorToString
    , five
    , four
    , number
    , octave
    , one
    , semitones
    , seven
    , six
    , three
    , two
    , zero
    )


type OctaveError
    = AboveValidRange Int
    | BelowValidRange Int


errorToString : OctaveError -> String
errorToString error =
    case error of
        AboveValidRange n ->
            "Octave number " ++ String.fromInt n ++ " is above the valid octave range of [0,8]."

        BelowValidRange n ->
            "Octave number " ++ String.fromInt n ++ " is below the valid octave range of [0,8]."


type Octave
    = Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight


octave : Int -> Result OctaveError Octave
octave n =
    case n of
        0 ->
            Ok Zero

        1 ->
            Ok One

        2 ->
            Ok Two

        3 ->
            Ok Three

        4 ->
            Ok Four

        5 ->
            Ok Five

        6 ->
            Ok Six

        7 ->
            Ok Seven

        8 ->
            Ok Eight

        other ->
            if other > 8 then
                Err <| AboveValidRange other

            else
                Err <| BelowValidRange other


add : Int -> Octave -> Result OctaveError Octave
add n o =
    octave (number o + n)


semitones : Octave -> Int
semitones o =
    number o * 12


number : Octave -> Int
number o =
    case o of
        Zero ->
            0

        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8


all : List Octave
all =
    [ Zero, One, Two, Three, Four, Five, Six, Seven, Eight ]


zero : Octave
zero =
    Zero


one : Octave
one =
    One


two : Octave
two =
    Two


three : Octave
three =
    Three


four : Octave
four =
    Four


five : Octave
five =
    Five


six : Octave
six =
    Six


seven : Octave
seven =
    Seven


eight : Octave
eight =
    Eight
