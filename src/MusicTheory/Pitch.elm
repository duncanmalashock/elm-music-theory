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
    , transposeDown
    , transposeUp
    , tripleFlat
    , tripleSharp
    )

import MusicTheory.Internal.Pitch as Pitch
import MusicTheory.Internal.PitchClass as PitchClass exposing (Offset, PitchClass)
import MusicTheory.Interval exposing (Interval)
import MusicTheory.Letter exposing (Letter)
import MusicTheory.Octave exposing (Octave, OctaveError(..))


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
