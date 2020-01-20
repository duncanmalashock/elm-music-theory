module MusicTheory.Pitch exposing
    ( Pitch
    , TransposeError
    , all
    , areEnharmonicEquivalents
    , doubleFlat
    , doubleSharp
    , flat
    , fromPitchClass
    , natural
    , octave
    , pitch
    , pitchClass
    , semitones
    , sharp
    , toString
    , transposeDown
    , transposeUp
    , tripleFlat
    , tripleSharp
    )

import MusicTheory.Internal.Pitch as Pitch
import MusicTheory.Internal.PitchClass as PitchClass exposing (Offset, PitchClass)
import MusicTheory.Interval exposing (Interval)
import MusicTheory.Letter exposing (Letter)
import MusicTheory.Octave as Octave exposing (Octave, OctaveError(..))
import MusicTheory.Pitch.Enharmonic as Enharmonic exposing (EnharmonicTransformationError(..))


type alias Pitch =
    Pitch.Pitch


type alias TransposeError =
    Pitch.TransposeError


pitch : Letter -> Offset -> Octave -> Pitch
pitch =
    Pitch.pitch


all : List Pitch
all =
    Pitch.all


tripleFlat : Offset
tripleFlat =
    PitchClass.tripleFlat


doubleFlat : Offset
doubleFlat =
    PitchClass.doubleFlat


flat : Offset
flat =
    PitchClass.flat


natural : Offset
natural =
    PitchClass.natural


sharp : Offset
sharp =
    PitchClass.sharp


doubleSharp : Offset
doubleSharp =
    PitchClass.doubleSharp


tripleSharp : Offset
tripleSharp =
    PitchClass.tripleSharp


pitchClass : Pitch -> PitchClass
pitchClass =
    Pitch.pitchClass


octave : Pitch -> Octave
octave =
    Pitch.octave


fromPitchClass : Octave -> PitchClass -> Pitch
fromPitchClass =
    Pitch.fromPitchClass


semitones : Pitch -> Int
semitones =
    Pitch.semitones


areEnharmonicEquivalents : Pitch -> Pitch -> Bool
areEnharmonicEquivalents =
    Pitch.areEnharmonicEquivalents


transposeUp : Interval -> Pitch -> Result TransposeError Pitch
transposeUp =
    Pitch.transposeUp


transposeDown : Interval -> Pitch -> Result TransposeError Pitch
transposeDown =
    Pitch.transposeDown


toString : Pitch -> String
toString pc =
    case pc |> Enharmonic.simple of
        Ok enharmonic ->
            (pitchClass enharmonic |> PitchClass.toString) ++ String.fromInt (octave enharmonic |> Octave.number)

        Err (Invalid thePitchClass (AboveValidRange o)) ->
            PitchClass.toString thePitchClass ++ String.fromInt o

        Err (Invalid thePitchClass (BelowValidRange o)) ->
            PitchClass.toString thePitchClass ++ String.fromInt o
