module Music.PitchClass exposing
    ( PitchClass
    , fromPitch, areEnharmonicEquivalents, letter, Letter(..), accidentals
    , toString
    , c, cSharp, d, dSharp, dFlat, e, eFlat, f, fSharp, g, gSharp, gFlat, a, aSharp, aFlat, b, bFlat
    , cFlat, eSharp, fFlat, bSharp
    , cDoubleSharp, cDoubleFlat, dDoubleSharp, dDoubleFlat, eDoubleSharp, eDoubleFlat, fDoubleSharp, fDoubleFlat, gDoubleSharp, gDoubleFlat, aDoubleSharp, aDoubleFlat, bDoubleSharp, bDoubleFlat
    )

{-| A [pitch class](https://en.wikipedia.org/wiki/Pitch_class) is like a pitch, but with no octave specified. E.g. the pitch class "B♭".

@docs PitchClass


# Helpers

@docs fromPitch, areEnharmonicEquivalents, letter, Letter, accidentals


# Conversion

@docs toString


# Constructors

@docs c, cSharp, d, dSharp, dFlat, e, eFlat, f, fSharp, g, gSharp, gFlat, a, aSharp, aFlat, b, bFlat


# Less common pitch classes

These are used less frequently in music. Most likely you want a pitch from the previous section.


## Enharmonic equivalents of natural notes

@docs cFlat, eSharp, fFlat, bSharp


## Pitch classes with [double accidentals](https://en.wikipedia.org/wiki/Accidental_%28music%29#Double_accidentals)

@docs cDoubleSharp, cDoubleFlat, dDoubleSharp, dDoubleFlat, eDoubleSharp, eDoubleFlat, fDoubleSharp, fDoubleFlat, gDoubleSharp, gDoubleFlat, aDoubleSharp, aDoubleFlat, bDoubleSharp, bDoubleFlat

-}

import Internal.Letter as Letter
import Internal.Pitch as Pitch
import Internal.PitchClass as PitchClass


{-| -}
type alias PitchClass =
    PitchClass.PitchClass


{-| Convert a `Pitch` to a `PitchClass`.

    fromPitch Pitch.c4 == c

-}
fromPitch : Pitch.Pitch -> PitchClass
fromPitch pitch =
    Pitch.pitchClass pitch


{-| Get a pitch class's letter name and accidental:

    toString bFlat == "B♭"

-}
toString : PitchClass -> String
toString pitch =
    PitchClass.toString pitch


{-| Check whether two pitch classes are enharmonic equivalents of each other. [Enharmonic equivalents](https://en.wikipedia.org/wiki/Enharmonic) are two pitch classes with different names but the same sound:

    areEnharmonicEquivalents cFlat b == True

-}
areEnharmonicEquivalents : PitchClass -> PitchClass -> Bool
areEnharmonicEquivalents pcA pcB =
    PitchClass.areEnharmonicEquivalents pcA pcB


{-| Get the letter name of a pitch class:

    letter fSharp =
        F

-}
letter : PitchClass -> Letter
letter thePitchClass =
    PitchClass.letter thePitchClass
        |> toLetter


toLetter : Letter.Letter -> Letter
toLetter internalLetter =
    case internalLetter of
        Letter.C ->
            C

        Letter.D ->
            D

        Letter.E ->
            E

        Letter.F ->
            F

        Letter.G ->
            G

        Letter.A ->
            A

        Letter.B ->
            B


{-| -}
type Letter
    = A
    | B
    | C
    | D
    | E
    | F
    | G


{-| Get the [accidentals](https://en.wikipedia.org/wiki/Accidental_%28music%29) of a pitch class.

Sharps are positive:

    accidentals fSharp =
        1

Flats are negative:

    accidentals aFlat =
        -1

Naturals are zero:

    accidentals c =
        0

-}
accidentals : PitchClass -> Int
accidentals thePitchClass =
    PitchClass.accidentals thePitchClass


{-| -}
a : PitchClass
a =
    PitchClass.a


{-| -}
aSharp : PitchClass
aSharp =
    PitchClass.aSharp


{-| -}
aDoubleSharp : PitchClass
aDoubleSharp =
    PitchClass.aDoubleSharp


{-| -}
aFlat : PitchClass
aFlat =
    PitchClass.aFlat


{-| -}
aDoubleFlat : PitchClass
aDoubleFlat =
    PitchClass.aDoubleFlat


{-| -}
b : PitchClass
b =
    PitchClass.b


{-| -}
bSharp : PitchClass
bSharp =
    PitchClass.bSharp


{-| -}
bDoubleSharp : PitchClass
bDoubleSharp =
    PitchClass.bDoubleSharp


{-| -}
bFlat : PitchClass
bFlat =
    PitchClass.bFlat


{-| -}
bDoubleFlat : PitchClass
bDoubleFlat =
    PitchClass.bDoubleFlat


{-| -}
c : PitchClass
c =
    PitchClass.c


{-| -}
cSharp : PitchClass
cSharp =
    PitchClass.cSharp


{-| -}
cDoubleSharp : PitchClass
cDoubleSharp =
    PitchClass.cDoubleSharp


{-| -}
cFlat : PitchClass
cFlat =
    PitchClass.cFlat


{-| -}
cDoubleFlat : PitchClass
cDoubleFlat =
    PitchClass.cDoubleFlat


{-| -}
d : PitchClass
d =
    PitchClass.d


{-| -}
dSharp : PitchClass
dSharp =
    PitchClass.dSharp


{-| -}
dDoubleSharp : PitchClass
dDoubleSharp =
    PitchClass.dDoubleSharp


{-| -}
dFlat : PitchClass
dFlat =
    PitchClass.dFlat


{-| -}
dDoubleFlat : PitchClass
dDoubleFlat =
    PitchClass.dDoubleFlat


{-| -}
e : PitchClass
e =
    PitchClass.e


{-| -}
eSharp : PitchClass
eSharp =
    PitchClass.eSharp


{-| -}
eDoubleSharp : PitchClass
eDoubleSharp =
    PitchClass.eDoubleSharp


{-| -}
eFlat : PitchClass
eFlat =
    PitchClass.eFlat


{-| -}
eDoubleFlat : PitchClass
eDoubleFlat =
    PitchClass.eDoubleFlat


{-| -}
f : PitchClass
f =
    PitchClass.f


{-| -}
fSharp : PitchClass
fSharp =
    PitchClass.fSharp


{-| -}
fDoubleSharp : PitchClass
fDoubleSharp =
    PitchClass.fDoubleSharp


{-| -}
fFlat : PitchClass
fFlat =
    PitchClass.fFlat


{-| -}
fDoubleFlat : PitchClass
fDoubleFlat =
    PitchClass.fDoubleFlat


{-| -}
g : PitchClass
g =
    PitchClass.g


{-| -}
gSharp : PitchClass
gSharp =
    PitchClass.gSharp


{-| -}
gDoubleSharp : PitchClass
gDoubleSharp =
    PitchClass.gDoubleSharp


{-| -}
gFlat : PitchClass
gFlat =
    PitchClass.gFlat


{-| -}
gDoubleFlat : PitchClass
gDoubleFlat =
    PitchClass.gDoubleFlat
