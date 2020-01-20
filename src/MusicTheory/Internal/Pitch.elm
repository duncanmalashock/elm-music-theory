module MusicTheory.Internal.Pitch exposing
    ( Pitch
    , TransposeError(..)
    , all
    , areEnharmonicEquivalents
    , doubleFlat
    , doubleSharp
    , errorToString
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

import MusicTheory.Internal.PitchClass as PitchClass exposing (Offset, PitchClass)
import MusicTheory.Interval as Interval exposing (Interval)
import MusicTheory.Letter as Letter exposing (Letter)
import MusicTheory.Octave as Octave exposing (Octave, OctaveError(..))


type Pitch
    = Pitch PitchClass Octave


type TransposeError
    = InvalidOctave OctaveError
    | InternalError


pitch : Letter -> Offset -> Octave -> Pitch
pitch l os o =
    fromPitchClass o (PitchClass.pitchClass l os)


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
pitchClass (Pitch pc _) =
    pc


octave : Pitch -> Octave
octave (Pitch _ o) =
    o


fromPitchClass : Octave -> PitchClass -> Pitch
fromPitchClass o p =
    Pitch p o


semitones : Pitch -> Int
semitones (Pitch pc o) =
    Octave.semitones o + PitchClass.semitonesNotOctaveBound pc


areEnharmonicEquivalents : Pitch -> Pitch -> Bool
areEnharmonicEquivalents lhs rhs =
    semitones lhs == semitones rhs


all : List Pitch
all =
    Octave.all
        |> List.concatMap (\o -> PitchClass.all |> List.map (\pc -> Pitch pc o))


errorToString : TransposeError -> String
errorToString error =
    case error of
        InvalidOctave err ->
            "Could not transpose pitch. " ++ Octave.errorToString err

        InternalError ->
            "Could not transpose pitch. Something went wrong internally."


transposeUp : Interval -> Pitch -> Result TransposeError Pitch
transposeUp =
    transpose PitchClass.transposeUp (+)


transposeDown : Interval -> Pitch -> Result TransposeError Pitch
transposeDown =
    transpose PitchClass.transposeDown (-)


transpose : (Interval -> PitchClass -> PitchClass) -> (Int -> Int -> Int) -> Interval -> Pitch -> Result TransposeError Pitch
transpose trans addIntervalSemitones interval p =
    let
        transposedPitchClass =
            pitchClass p
                |> trans interval

        targetOctaveSemitones =
            addIntervalSemitones (semitones p) (Interval.semitones interval) - PitchClass.semitonesNotOctaveBound transposedPitchClass

        numberOfOctaves =
            targetOctaveSemitones // 12

        remainder =
            targetOctaveSemitones |> remainderBy 12
    in
    if remainder == 0 then
        Octave.octave numberOfOctaves
            |> Result.map (\o -> Pitch transposedPitchClass o)
            |> Result.mapError InvalidOctave

    else
        Err InternalError
