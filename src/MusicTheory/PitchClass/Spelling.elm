module MusicTheory.PitchClass.Spelling exposing
    ( Accidental(..)
    , PitchClassSpelling
    , naturalOrElseFlat
    , naturalOrElseSharp
    , simple
    , toPitchClass
    , toString
    )

import MusicTheory.Internal.PitchClass as Internal
import MusicTheory.Internal.PitchClass.Enharmonic as InternalEnharmonic exposing (NaturalOrSingleAccidental(..))
import MusicTheory.Letter as Letter exposing (Letter(..))
import MusicTheory.PitchClass as PitchClass exposing (PitchClass)


{-| An accidental describes by how many semitones a letter is raised or lowered.
-}
type Accidental
    = Flat
    | Natural
    | Sharp


type alias PitchClassSpelling =
    { letter : Letter
    , accidental : Accidental
    }


simple : PitchClass -> PitchClassSpelling
simple pitchClass =
    if Internal.offset pitchClass == 0 then
        { letter = Internal.letter pitchClass, accidental = Natural }

    else if Internal.offset pitchClass < 0 then
        naturalOrElseFlat pitchClass

    else
        naturalOrElseSharp pitchClass


{-| String representation of a letter and an accidental.

    toString ( D, Sharp ) == "D♯"

-}
toString : PitchClassSpelling -> String
toString { letter, accidental } =
    Letter.toString letter ++ accidentalToString accidental


{-| Create a pitch class from a tuple of a letter and an accidental.

    toPitchClass ( G, Flat ) -- creates the pitch class G♭

-}
toPitchClass : PitchClassSpelling -> PitchClass
toPitchClass { letter, accidental } =
    PitchClass.pitchClass letter (accidentalToOffset accidental)


{-| Returns the enharmonic equivalent pitch class expressed as a note from the diatonic C major scale that is natural or lowered once.

    naturalOrElseFlat (pitchClass F DoubleSharp) == ( G, Natural )

    naturalOrElseFlat (pitchClass C TripleSharp) == ( E, Flat )

-}
naturalOrElseFlat : PitchClass -> PitchClassSpelling
naturalOrElseFlat pitchClass =
    case pitchClass |> PitchClass.semitones |> InternalEnharmonic.semitonesToNaturalOrAccidental 0 of
        Nat letter _ ->
            { letter = letter, accidental = Natural }

        SharpFlat _ letter _ ->
            { letter = letter, accidental = Flat }


{-| Returns the enharmonic equivalent pitch class expressed as a note from the diatonic C major scale that is natural or raised once

    naturalOrElseSharp (pitchClass F DoubleSharp) == ( G, Natural )

    naturalOrElseSharp (pitchClass C TripleSharp) == ( D, Sharp )

-}
naturalOrElseSharp : PitchClass -> PitchClassSpelling
naturalOrElseSharp pitchClass =
    case pitchClass |> PitchClass.semitones |> InternalEnharmonic.semitonesToNaturalOrAccidental 0 of
        Nat letter _ ->
            { letter = letter, accidental = Natural }

        SharpFlat letter _ _ ->
            { letter = letter, accidental = Sharp }



-- INTERNALS


accidentalToOffset : Accidental -> Internal.Offset
accidentalToOffset accidental =
    case accidental of
        Flat ->
            Internal.flat

        Natural ->
            Internal.natural

        Sharp ->
            Internal.sharp


accidentalToString : Accidental -> String
accidentalToString accidental =
    case accidental of
        Flat ->
            "♭"

        Natural ->
            ""

        Sharp ->
            "♯"
