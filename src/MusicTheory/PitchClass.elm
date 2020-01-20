module MusicTheory.PitchClass exposing
    ( PitchClass
    , all
    , areEnharmonicEquivalents
    , doubleFlat
    , doubleSharp
    , flat
    , natural
    , pitchClass
    , semitones
    , sharp
    , toString
    , transposeDown
    , transposeUp
    , tripleFlat
    , tripleSharp
    )

{-| A pitch class is a set of all pitches that are a whole number of octaves
apart. A pitch class is represented as a letter together with an accidental.
-}

import MusicTheory.Internal.PitchClass as Internal
import MusicTheory.Interval exposing (Interval)
import MusicTheory.Letter exposing (Letter)
import MusicTheory.PitchClass.Enharmonic as Enharmonic



-- DEFINITION


{-| Internal representation of an unlimited raised or lowered letter.
E.g. four flats are represented as as `Offset -4`.
-}
type alias Offset =
    Internal.Offset


{-| Opaque type that represents a pitch class.
-}
type alias PitchClass =
    Internal.PitchClass



-- CONSTRUCTORS


tripleFlat : Offset
tripleFlat =
    Internal.tripleFlat


doubleFlat : Offset
doubleFlat =
    Internal.doubleFlat


flat : Offset
flat =
    Internal.flat


natural : Offset
natural =
    Internal.natural


sharp : Offset
sharp =
    Internal.sharp


doubleSharp : Offset
doubleSharp =
    Internal.doubleSharp


tripleSharp : Offset
tripleSharp =
    Internal.tripleSharp


{-| Create a pitch class from a letter and an accidental.

    pitchClass C sharp -- creates the pitch class C♯

-}
pitchClass : Letter -> Offset -> PitchClass
pitchClass =
    Internal.pitchClass



-- ACCESSORS


{-| A list of all pitch classes that with at most 3 accidentals.
`Cbbb, Cbb, Cb, C, C#, C##, C###, Dbbb, Dbb, ...`.
-}
all : List PitchClass
all =
    Internal.all



-- TRANSFORM


{-| Number of semitones between C natural and a given pitch class.

    semitones (pitchClass E natural) == 4

-}
semitones : PitchClass -> Int
semitones =
    Internal.semitones


toString : PitchClass -> String
toString pc =
    Internal.toString (Enharmonic.simple pc)



-- TRANSPOSE


{-| Moves a pitch class up by a given interval while taking the correct
number off staff positions between root and target pitch class into account.
-}
transposeUp : Interval -> PitchClass -> PitchClass
transposeUp =
    Internal.transposeUp


{-| Moves a pitch class down by a given interval while taking the correct
number off staff positions between root and target pitch class into account.

    (pitchClass B natural |> transposeDown Interval.minorSecond) == pitchClass A sharp

-}
transposeDown : Interval -> PitchClass -> PitchClass
transposeDown =
    Internal.transposeDown



-- COMPARISON


{-| Returns true if two pitch classes are enharmonic equivalent.
-}
areEnharmonicEquivalents : PitchClass -> PitchClass -> Bool
areEnharmonicEquivalents lhs rhs =
    semitones lhs == semitones rhs
