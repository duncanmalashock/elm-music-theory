module Music.Pitch exposing
    ( Pitch
    , transposeUp, transposeDown, intervalBetween, areEnharmonicEquivalents, octave
    , chromaticRun
    , semitones, toMIDINoteNumber, toFrequency, toString, fromPitchClassInOctave
    , simplify
    , c0, c1, c2, c3, c4, c5, c6, c7, c8
    , cSharp0, cSharp1, cSharp2, cSharp3, cSharp4, cSharp5, cSharp6, cSharp7, cSharp8
    , d0, d1, d2, d3, d4, d5, d6, d7, d8
    , dFlat0, dFlat1, dFlat2, dFlat3, dFlat4, dFlat5, dFlat6, dFlat7, dFlat8
    , dSharp0, dSharp1, dSharp2, dSharp3, dSharp4, dSharp5, dSharp6, dSharp7, dSharp8
    , e0, e1, e2, e3, e4, e5, e6, e7, e8
    , eFlat0, eFlat1, eFlat2, eFlat3, eFlat4, eFlat5, eFlat6, eFlat7, eFlat8
    , f0, f1, f2, f3, f4, f5, f6, f7, f8
    , fSharp0, fSharp1, fSharp2, fSharp3, fSharp4, fSharp5, fSharp6, fSharp7, fSharp8
    , g0, g1, g2, g3, g4, g5, g6, g7, g8
    , gFlat0, gFlat1, gFlat2, gFlat3, gFlat4, gFlat5, gFlat6, gFlat7, gFlat8
    , gSharp0, gSharp1, gSharp2, gSharp3, gSharp4, gSharp5, gSharp6, gSharp7, gSharp8
    , a0, a1, a2, a3, a4, a5, a6, a7, a8
    , aFlat0, aFlat1, aFlat2, aFlat3, aFlat4, aFlat5, aFlat6, aFlat7, aFlat8
    , aSharp0, aSharp1, aSharp2, aSharp3, aSharp4, aSharp5, aSharp6, aSharp7, aSharp8
    , b0, b1, b2, b3, b4, b5, b6, b7, b8
    , bFlat0, bFlat1, bFlat2, bFlat3, bFlat4, bFlat5, bFlat6, bFlat7, bFlat8
    , cFlat1, cFlat2, cFlat3, cFlat4, cFlat5, cFlat6, cFlat7, cFlat8
    , eSharp0, eSharp1, eSharp2, eSharp3, eSharp4, eSharp5, eSharp6, eSharp7, eSharp8
    , fFlat0, fFlat1, fFlat2, fFlat3, fFlat4, fFlat5, fFlat6, fFlat7, fFlat8
    , bSharp0, bSharp1, bSharp2, bSharp3, bSharp4, bSharp5, bSharp6, bSharp7, bSharp8
    )

{-| A [pitch](https://en.wikipedia.org/wiki/Pitch_%28music%29) represents a specific frequency of sound with a letter name, octave, and accidental. E.g. the pitch "B♭4".

@docs Pitch


# Helpers

@docs transposeUp, transposeDown, intervalBetween, areEnharmonicEquivalents, octave


# Generating

@docs chromaticRun


# Conversion

@docs semitones, toMIDINoteNumber, toFrequency, toString, fromPitchClassInOctave


# Respelling

@docs simplify


# Constructors

@docs c0, c1, c2, c3, c4, c5, c6, c7, c8
@docs cSharp0, cSharp1, cSharp2, cSharp3, cSharp4, cSharp5, cSharp6, cSharp7, cSharp8
@docs d0, d1, d2, d3, d4, d5, d6, d7, d8
@docs dFlat0, dFlat1, dFlat2, dFlat3, dFlat4, dFlat5, dFlat6, dFlat7, dFlat8
@docs dSharp0, dSharp1, dSharp2, dSharp3, dSharp4, dSharp5, dSharp6, dSharp7, dSharp8
@docs e0, e1, e2, e3, e4, e5, e6, e7, e8
@docs eFlat0, eFlat1, eFlat2, eFlat3, eFlat4, eFlat5, eFlat6, eFlat7, eFlat8
@docs f0, f1, f2, f3, f4, f5, f6, f7, f8
@docs fSharp0, fSharp1, fSharp2, fSharp3, fSharp4, fSharp5, fSharp6, fSharp7, fSharp8
@docs g0, g1, g2, g3, g4, g5, g6, g7, g8
@docs gFlat0, gFlat1, gFlat2, gFlat3, gFlat4, gFlat5, gFlat6, gFlat7, gFlat8
@docs gSharp0, gSharp1, gSharp2, gSharp3, gSharp4, gSharp5, gSharp6, gSharp7, gSharp8
@docs a0, a1, a2, a3, a4, a5, a6, a7, a8
@docs aFlat0, aFlat1, aFlat2, aFlat3, aFlat4, aFlat5, aFlat6, aFlat7, aFlat8
@docs aSharp0, aSharp1, aSharp2, aSharp3, aSharp4, aSharp5, aSharp6, aSharp7, aSharp8
@docs b0, b1, b2, b3, b4, b5, b6, b7, b8
@docs bFlat0, bFlat1, bFlat2, bFlat3, bFlat4, bFlat5, bFlat6, bFlat7, bFlat8


# Less common pitches

These are used less frequently in music. Most likely you want a pitch from the previous section.


## Enharmonic equivalents of natural notes

@docs cFlat1, cFlat2, cFlat3, cFlat4, cFlat5, cFlat6, cFlat7, cFlat8
@docs eSharp0, eSharp1, eSharp2, eSharp3, eSharp4, eSharp5, eSharp6, eSharp7, eSharp8
@docs fFlat0, fFlat1, fFlat2, fFlat3, fFlat4, fFlat5, fFlat6, fFlat7, fFlat8
@docs bSharp0, bSharp1, bSharp2, bSharp3, bSharp4, bSharp5, bSharp6, bSharp7, bSharp8

-}

import Music.Internal.Interval as Interval
import Music.Internal.Octave as Octave
import Music.Internal.Pitch as Pitch
import Music.Internal.PitchClass as PitchClass


{-| -}
type alias Pitch =
    Pitch.Pitch


{-| Create a pitch from a pitch class and octave number:

    fromPitchClassInOctave 4 c == c4

-}
fromPitchClassInOctave : Int -> PitchClass.PitchClass -> Pitch
fromPitchClassInOctave o pc =
    Pitch.fromPitchClassWithInt o pc


{-| Get the octave of a pitch:

    octave c4 == 4

-}
octave : Pitch -> Int
octave thePitch =
    Pitch.octave thePitch
        |> Octave.number


{-| [Transpose](https://en.wikipedia.org/wiki/Transposition_%28music%29) a pitch upward by an interval.

    transposeUp Interval.perfectFifth c4 == g4

-}
transposeUp : Interval.Interval -> Pitch -> Pitch
transposeUp interval pitch =
    Pitch.transposeUp interval pitch


{-| [Transpose](https://en.wikipedia.org/wiki/Transposition_%28music%29) a pitch downward by an interval.

    transposeDown Interval.perfectFifth c4 == f3

-}
transposeDown : Interval.Interval -> Pitch -> Pitch
transposeDown interval pitch =
    Pitch.transposeDown interval pitch


{-| Get the interval between two pitches:

    intervalBetween c4 g4 == Interval.perfectFifth

-}
intervalBetween : Pitch -> Pitch -> Interval.Interval
intervalBetween a b =
    Pitch.intervalBetween a b


{-| Check whether two pitches are enharmonic equivalents of each other. [Enharmonic equivalents](https://en.wikipedia.org/wiki/Enharmonic) are two pitches with different names but the same sound:

    areEnharmonicEquivalents cSharp4 dFlat4 == True

-}
areEnharmonicEquivalents : Pitch -> Pitch -> Bool
areEnharmonicEquivalents a b =
    Pitch.areEnharmonicEquivalents a b


{-| Generate a section of the ascending or descending [chromatic scale](https://en.wikipedia.org/wiki/Chromatic_scale) between two pitches:

    chromaticRun a4 d5
        == [ a4
           , aSharp4
           , b4
           , c5
           , cSharp5
           , d5
           ]

Ascending runs use sharps; descending runs use flats:

    chromaticRun d5 a4
        == [ d5
           , dFlat5
           , c5
           , b4
           , bFlat4
           , a4
           ]

-}
chromaticRun : Pitch -> Pitch -> List Pitch
chromaticRun start end =
    Pitch.chromaticRun start end


{-| Respell a pitch with as few accidentals as possible:

    bDoubleSharp4 =
        transposeUp Interval.augmentedUnison bSharp4

    simplify bDoubleSharp
        == cSharp5

Pitches with two or more accidentals are fairly rare, but possible. This function is helpful if you plan to use pitch data with software (for notation, etc.) in which multiple accidentals are _not_ possible.

-}
simplify : Pitch -> Pitch
simplify thePitch =
    Pitch.simplify thePitch


{-| Distance in semitones from C0. Useful for sorting.

    semitones c4 == 48

-}
semitones : Pitch -> Int
semitones pitch =
    Pitch.semitones pitch


{-| Convert a pitch to a [MIDI](https://en.wikipedia.org/wiki/MIDI#Messages) number:

    toMIDINoteNumber c4 == 60

-}
toMIDINoteNumber : Pitch -> Int
toMIDINoteNumber pitch =
    Pitch.toMIDINoteNumber pitch


{-| Convert a pitch to a frequency in [Hertz](https://en.wikipedia.org/wiki/Hertz):

    toFrequency c4 == 261.6255653005986

-}
toFrequency : Pitch -> Float
toFrequency pitch =
    Pitch.toFrequency pitch


{-| A pitch's letter name, accidental, and octave number:

    toString cSharp4 == "C♯4"

-}
toString : Pitch -> String
toString pitch =
    Pitch.toString pitch



-- Constructors


{-| -}
c0 : Pitch.Pitch
c0 =
    Pitch.c0


{-| -}
cSharp0 : Pitch.Pitch
cSharp0 =
    Pitch.cSharp0


{-| -}
d0 : Pitch.Pitch
d0 =
    Pitch.d0


{-| -}
dSharp0 : Pitch.Pitch
dSharp0 =
    Pitch.dSharp0


{-| -}
dFlat0 : Pitch.Pitch
dFlat0 =
    Pitch.dFlat0


{-| -}
e0 : Pitch.Pitch
e0 =
    Pitch.e0


{-| -}
eSharp0 : Pitch.Pitch
eSharp0 =
    Pitch.eSharp0


{-| -}
eFlat0 : Pitch.Pitch
eFlat0 =
    Pitch.eFlat0


{-| -}
f0 : Pitch.Pitch
f0 =
    Pitch.f0


{-| -}
fSharp0 : Pitch.Pitch
fSharp0 =
    Pitch.fSharp0


{-| -}
fFlat0 : Pitch.Pitch
fFlat0 =
    Pitch.fFlat0


{-| -}
g0 : Pitch.Pitch
g0 =
    Pitch.g0


{-| -}
gSharp0 : Pitch.Pitch
gSharp0 =
    Pitch.gSharp0


{-| -}
gFlat0 : Pitch.Pitch
gFlat0 =
    Pitch.gFlat0


{-| -}
a0 : Pitch.Pitch
a0 =
    Pitch.a0


{-| -}
aSharp0 : Pitch.Pitch
aSharp0 =
    Pitch.aSharp0


{-| -}
aFlat0 : Pitch.Pitch
aFlat0 =
    Pitch.aFlat0


{-| -}
b0 : Pitch.Pitch
b0 =
    Pitch.b0


{-| -}
bSharp0 : Pitch.Pitch
bSharp0 =
    Pitch.bSharp0


{-| -}
bFlat0 : Pitch.Pitch
bFlat0 =
    Pitch.bFlat0


{-| -}
c1 : Pitch.Pitch
c1 =
    Pitch.c1


{-| -}
cSharp1 : Pitch.Pitch
cSharp1 =
    Pitch.cSharp1


{-| -}
cFlat1 : Pitch.Pitch
cFlat1 =
    Pitch.cFlat1


{-| -}
d1 : Pitch.Pitch
d1 =
    Pitch.d1


{-| -}
dSharp1 : Pitch.Pitch
dSharp1 =
    Pitch.dSharp1


{-| -}
dFlat1 : Pitch.Pitch
dFlat1 =
    Pitch.dFlat1


{-| -}
e1 : Pitch.Pitch
e1 =
    Pitch.e1


{-| -}
eSharp1 : Pitch.Pitch
eSharp1 =
    Pitch.eSharp1


{-| -}
eFlat1 : Pitch.Pitch
eFlat1 =
    Pitch.eFlat1


{-| -}
f1 : Pitch.Pitch
f1 =
    Pitch.f1


{-| -}
fSharp1 : Pitch.Pitch
fSharp1 =
    Pitch.fSharp1


{-| -}
fFlat1 : Pitch.Pitch
fFlat1 =
    Pitch.fFlat1


{-| -}
g1 : Pitch.Pitch
g1 =
    Pitch.g1


{-| -}
gSharp1 : Pitch.Pitch
gSharp1 =
    Pitch.gSharp1


{-| -}
gFlat1 : Pitch.Pitch
gFlat1 =
    Pitch.gFlat1


{-| -}
a1 : Pitch.Pitch
a1 =
    Pitch.a1


{-| -}
aSharp1 : Pitch.Pitch
aSharp1 =
    Pitch.aSharp1


{-| -}
aFlat1 : Pitch.Pitch
aFlat1 =
    Pitch.aFlat1


{-| -}
b1 : Pitch.Pitch
b1 =
    Pitch.b1


{-| -}
bSharp1 : Pitch.Pitch
bSharp1 =
    Pitch.bSharp1


{-| -}
bFlat1 : Pitch.Pitch
bFlat1 =
    Pitch.bFlat1


{-| -}
c2 : Pitch.Pitch
c2 =
    Pitch.c2


{-| -}
cSharp2 : Pitch.Pitch
cSharp2 =
    Pitch.cSharp2


{-| -}
cFlat2 : Pitch.Pitch
cFlat2 =
    Pitch.cFlat2


{-| -}
d2 : Pitch.Pitch
d2 =
    Pitch.d2


{-| -}
dSharp2 : Pitch.Pitch
dSharp2 =
    Pitch.dSharp2


{-| -}
dFlat2 : Pitch.Pitch
dFlat2 =
    Pitch.dFlat2


{-| -}
e2 : Pitch.Pitch
e2 =
    Pitch.e2


{-| -}
eSharp2 : Pitch.Pitch
eSharp2 =
    Pitch.eSharp2


{-| -}
eFlat2 : Pitch.Pitch
eFlat2 =
    Pitch.eFlat2


{-| -}
f2 : Pitch.Pitch
f2 =
    Pitch.f2


{-| -}
fSharp2 : Pitch.Pitch
fSharp2 =
    Pitch.fSharp2


{-| -}
fFlat2 : Pitch.Pitch
fFlat2 =
    Pitch.fFlat2


{-| -}
g2 : Pitch.Pitch
g2 =
    Pitch.g2


{-| -}
gSharp2 : Pitch.Pitch
gSharp2 =
    Pitch.gSharp2


{-| -}
gFlat2 : Pitch.Pitch
gFlat2 =
    Pitch.gFlat2


{-| -}
a2 : Pitch.Pitch
a2 =
    Pitch.a2


{-| -}
aSharp2 : Pitch.Pitch
aSharp2 =
    Pitch.aSharp2


{-| -}
aFlat2 : Pitch.Pitch
aFlat2 =
    Pitch.aFlat2


{-| -}
b2 : Pitch.Pitch
b2 =
    Pitch.b2


{-| -}
bSharp2 : Pitch.Pitch
bSharp2 =
    Pitch.bSharp2


{-| -}
bFlat2 : Pitch.Pitch
bFlat2 =
    Pitch.bFlat2


{-| -}
c3 : Pitch.Pitch
c3 =
    Pitch.c3


{-| -}
cSharp3 : Pitch.Pitch
cSharp3 =
    Pitch.cSharp3


{-| -}
cFlat3 : Pitch.Pitch
cFlat3 =
    Pitch.cFlat3


{-| -}
d3 : Pitch.Pitch
d3 =
    Pitch.d3


{-| -}
dSharp3 : Pitch.Pitch
dSharp3 =
    Pitch.dSharp3


{-| -}
dFlat3 : Pitch.Pitch
dFlat3 =
    Pitch.dFlat3


{-| -}
e3 : Pitch.Pitch
e3 =
    Pitch.e3


{-| -}
eSharp3 : Pitch.Pitch
eSharp3 =
    Pitch.eSharp3


{-| -}
eFlat3 : Pitch.Pitch
eFlat3 =
    Pitch.eFlat3


{-| -}
f3 : Pitch.Pitch
f3 =
    Pitch.f3


{-| -}
fSharp3 : Pitch.Pitch
fSharp3 =
    Pitch.fSharp3


{-| -}
fFlat3 : Pitch.Pitch
fFlat3 =
    Pitch.fFlat3


{-| -}
g3 : Pitch.Pitch
g3 =
    Pitch.g3


{-| -}
gSharp3 : Pitch.Pitch
gSharp3 =
    Pitch.gSharp3


{-| -}
gFlat3 : Pitch.Pitch
gFlat3 =
    Pitch.gFlat3


{-| -}
a3 : Pitch.Pitch
a3 =
    Pitch.a3


{-| -}
aSharp3 : Pitch.Pitch
aSharp3 =
    Pitch.aSharp3


{-| -}
aFlat3 : Pitch.Pitch
aFlat3 =
    Pitch.aFlat3


{-| -}
b3 : Pitch.Pitch
b3 =
    Pitch.b3


{-| -}
bSharp3 : Pitch.Pitch
bSharp3 =
    Pitch.bSharp3


{-| -}
bFlat3 : Pitch.Pitch
bFlat3 =
    Pitch.bFlat3


{-| -}
c4 : Pitch.Pitch
c4 =
    Pitch.c4


{-| -}
cSharp4 : Pitch.Pitch
cSharp4 =
    Pitch.cSharp4


{-| -}
cFlat4 : Pitch.Pitch
cFlat4 =
    Pitch.cFlat4


{-| -}
d4 : Pitch.Pitch
d4 =
    Pitch.d4


{-| -}
dSharp4 : Pitch.Pitch
dSharp4 =
    Pitch.dSharp4


{-| -}
dFlat4 : Pitch.Pitch
dFlat4 =
    Pitch.dFlat4


{-| -}
e4 : Pitch.Pitch
e4 =
    Pitch.e4


{-| -}
eSharp4 : Pitch.Pitch
eSharp4 =
    Pitch.eSharp4


{-| -}
eFlat4 : Pitch.Pitch
eFlat4 =
    Pitch.eFlat4


{-| -}
f4 : Pitch.Pitch
f4 =
    Pitch.f4


{-| -}
fSharp4 : Pitch.Pitch
fSharp4 =
    Pitch.fSharp4


{-| -}
fFlat4 : Pitch.Pitch
fFlat4 =
    Pitch.fFlat4


{-| -}
g4 : Pitch.Pitch
g4 =
    Pitch.g4


{-| -}
gSharp4 : Pitch.Pitch
gSharp4 =
    Pitch.gSharp4


{-| -}
gFlat4 : Pitch.Pitch
gFlat4 =
    Pitch.gFlat4


{-| -}
a4 : Pitch.Pitch
a4 =
    Pitch.a4


{-| -}
aSharp4 : Pitch.Pitch
aSharp4 =
    Pitch.aSharp4


{-| -}
aFlat4 : Pitch.Pitch
aFlat4 =
    Pitch.aFlat4


{-| -}
b4 : Pitch.Pitch
b4 =
    Pitch.b4


{-| -}
bSharp4 : Pitch.Pitch
bSharp4 =
    Pitch.bSharp4


{-| -}
bFlat4 : Pitch.Pitch
bFlat4 =
    Pitch.bFlat4


{-| -}
c5 : Pitch.Pitch
c5 =
    Pitch.c5


{-| -}
cSharp5 : Pitch.Pitch
cSharp5 =
    Pitch.cSharp5


{-| -}
cFlat5 : Pitch.Pitch
cFlat5 =
    Pitch.cFlat5


{-| -}
d5 : Pitch.Pitch
d5 =
    Pitch.d5


{-| -}
dSharp5 : Pitch.Pitch
dSharp5 =
    Pitch.dSharp5


{-| -}
dFlat5 : Pitch.Pitch
dFlat5 =
    Pitch.dFlat5


{-| -}
e5 : Pitch.Pitch
e5 =
    Pitch.e5


{-| -}
eSharp5 : Pitch.Pitch
eSharp5 =
    Pitch.eSharp5


{-| -}
eFlat5 : Pitch.Pitch
eFlat5 =
    Pitch.eFlat5


{-| -}
f5 : Pitch.Pitch
f5 =
    Pitch.f5


{-| -}
fSharp5 : Pitch.Pitch
fSharp5 =
    Pitch.fSharp5


{-| -}
fFlat5 : Pitch.Pitch
fFlat5 =
    Pitch.fFlat5


{-| -}
g5 : Pitch.Pitch
g5 =
    Pitch.g5


{-| -}
gSharp5 : Pitch.Pitch
gSharp5 =
    Pitch.gSharp5


{-| -}
gFlat5 : Pitch.Pitch
gFlat5 =
    Pitch.gFlat5


{-| -}
a5 : Pitch.Pitch
a5 =
    Pitch.a5


{-| -}
aSharp5 : Pitch.Pitch
aSharp5 =
    Pitch.aSharp5


{-| -}
aFlat5 : Pitch.Pitch
aFlat5 =
    Pitch.aFlat5


{-| -}
b5 : Pitch.Pitch
b5 =
    Pitch.b5


{-| -}
bSharp5 : Pitch.Pitch
bSharp5 =
    Pitch.bSharp5


{-| -}
bFlat5 : Pitch.Pitch
bFlat5 =
    Pitch.bFlat5


{-| -}
c6 : Pitch.Pitch
c6 =
    Pitch.c6


{-| -}
cSharp6 : Pitch.Pitch
cSharp6 =
    Pitch.cSharp6


{-| -}
cFlat6 : Pitch.Pitch
cFlat6 =
    Pitch.cFlat6


{-| -}
d6 : Pitch.Pitch
d6 =
    Pitch.d6


{-| -}
dSharp6 : Pitch.Pitch
dSharp6 =
    Pitch.dSharp6


{-| -}
dFlat6 : Pitch.Pitch
dFlat6 =
    Pitch.dFlat6


{-| -}
e6 : Pitch.Pitch
e6 =
    Pitch.e6


{-| -}
eSharp6 : Pitch.Pitch
eSharp6 =
    Pitch.eSharp6


{-| -}
eFlat6 : Pitch.Pitch
eFlat6 =
    Pitch.eFlat6


{-| -}
f6 : Pitch.Pitch
f6 =
    Pitch.f6


{-| -}
fSharp6 : Pitch.Pitch
fSharp6 =
    Pitch.fSharp6


{-| -}
fFlat6 : Pitch.Pitch
fFlat6 =
    Pitch.fFlat6


{-| -}
g6 : Pitch.Pitch
g6 =
    Pitch.g6


{-| -}
gSharp6 : Pitch.Pitch
gSharp6 =
    Pitch.gSharp6


{-| -}
gFlat6 : Pitch.Pitch
gFlat6 =
    Pitch.gFlat6


{-| -}
a6 : Pitch.Pitch
a6 =
    Pitch.a6


{-| -}
aSharp6 : Pitch.Pitch
aSharp6 =
    Pitch.aSharp6


{-| -}
aFlat6 : Pitch.Pitch
aFlat6 =
    Pitch.aFlat6


{-| -}
b6 : Pitch.Pitch
b6 =
    Pitch.b6


{-| -}
bSharp6 : Pitch.Pitch
bSharp6 =
    Pitch.bSharp6


{-| -}
bFlat6 : Pitch.Pitch
bFlat6 =
    Pitch.bFlat6


{-| -}
c7 : Pitch.Pitch
c7 =
    Pitch.c7


{-| -}
cSharp7 : Pitch.Pitch
cSharp7 =
    Pitch.cSharp7


{-| -}
cFlat7 : Pitch.Pitch
cFlat7 =
    Pitch.cFlat7


{-| -}
d7 : Pitch.Pitch
d7 =
    Pitch.d7


{-| -}
dSharp7 : Pitch.Pitch
dSharp7 =
    Pitch.dSharp7


{-| -}
dFlat7 : Pitch.Pitch
dFlat7 =
    Pitch.dFlat7


{-| -}
e7 : Pitch.Pitch
e7 =
    Pitch.e7


{-| -}
eSharp7 : Pitch.Pitch
eSharp7 =
    Pitch.eSharp7


{-| -}
eFlat7 : Pitch.Pitch
eFlat7 =
    Pitch.eFlat7


{-| -}
f7 : Pitch.Pitch
f7 =
    Pitch.f7


{-| -}
fSharp7 : Pitch.Pitch
fSharp7 =
    Pitch.fSharp7


{-| -}
fFlat7 : Pitch.Pitch
fFlat7 =
    Pitch.fFlat7


{-| -}
g7 : Pitch.Pitch
g7 =
    Pitch.g7


{-| -}
gSharp7 : Pitch.Pitch
gSharp7 =
    Pitch.gSharp7


{-| -}
gFlat7 : Pitch.Pitch
gFlat7 =
    Pitch.gFlat7


{-| -}
a7 : Pitch.Pitch
a7 =
    Pitch.a7


{-| -}
aSharp7 : Pitch.Pitch
aSharp7 =
    Pitch.aSharp7


{-| -}
aFlat7 : Pitch.Pitch
aFlat7 =
    Pitch.aFlat7


{-| -}
b7 : Pitch.Pitch
b7 =
    Pitch.b7


{-| -}
bSharp7 : Pitch.Pitch
bSharp7 =
    Pitch.bSharp7


{-| -}
bFlat7 : Pitch.Pitch
bFlat7 =
    Pitch.bFlat7


{-| -}
c8 : Pitch.Pitch
c8 =
    Pitch.c8


{-| -}
cSharp8 : Pitch.Pitch
cSharp8 =
    Pitch.cSharp8


{-| -}
cFlat8 : Pitch.Pitch
cFlat8 =
    Pitch.cFlat8


{-| -}
d8 : Pitch.Pitch
d8 =
    Pitch.d8


{-| -}
dSharp8 : Pitch.Pitch
dSharp8 =
    Pitch.dSharp8


{-| -}
dFlat8 : Pitch.Pitch
dFlat8 =
    Pitch.dFlat8


{-| -}
e8 : Pitch.Pitch
e8 =
    Pitch.e8


{-| -}
eSharp8 : Pitch.Pitch
eSharp8 =
    Pitch.eSharp8


{-| -}
eFlat8 : Pitch.Pitch
eFlat8 =
    Pitch.eFlat8


{-| -}
f8 : Pitch.Pitch
f8 =
    Pitch.f8


{-| -}
fSharp8 : Pitch.Pitch
fSharp8 =
    Pitch.fSharp8


{-| -}
fFlat8 : Pitch.Pitch
fFlat8 =
    Pitch.fFlat8


{-| -}
g8 : Pitch.Pitch
g8 =
    Pitch.g8


{-| -}
gSharp8 : Pitch.Pitch
gSharp8 =
    Pitch.gSharp8


{-| -}
gFlat8 : Pitch.Pitch
gFlat8 =
    Pitch.gFlat8


{-| -}
a8 : Pitch.Pitch
a8 =
    Pitch.a8


{-| -}
aSharp8 : Pitch.Pitch
aSharp8 =
    Pitch.aSharp8


{-| -}
aFlat8 : Pitch.Pitch
aFlat8 =
    Pitch.aFlat8


{-| -}
b8 : Pitch.Pitch
b8 =
    Pitch.b8


{-| -}
bSharp8 : Pitch.Pitch
bSharp8 =
    Pitch.bSharp8


{-| -}
bFlat8 : Pitch.Pitch
bFlat8 =
    Pitch.bFlat8
