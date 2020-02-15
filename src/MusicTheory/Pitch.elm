module MusicTheory.Pitch exposing
    ( Pitch
    , PitchError(..)
    , all
    , allForPitchClass
    , areEnharmonicEquivalents
    , doubleFlat
    , doubleSharp
    , errorToString
    , firstAbove
    , firstBelow
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

import MusicTheory.Interval as Interval exposing (Interval)
import MusicTheory.Letter exposing (Letter)
import MusicTheory.Octave as Octave exposing (Octave, OctaveError(..))
import MusicTheory.PitchClass as PitchClass exposing (Offset, PitchClass)


semitonesUpperLimit : Int
semitonesUpperLimit =
    120


semitonesLowerLimit : Int
semitonesLowerLimit =
    0


type Pitch
    = Pitch PitchClass Octave


type PitchError
    = InvalidOctave OctaveError
    | OutOfRange
    | InternalError


pitch : Letter -> Offset -> Octave -> Result PitchError Pitch
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


fromPitchClass : Octave -> PitchClass -> Result PitchError Pitch
fromPitchClass o p =
    let
        newPitch =
            Pitch p o
    in
    if
        (semitones newPitch > semitonesUpperLimit)
            || (semitones newPitch < semitonesLowerLimit)
    then
        Err <| OutOfRange

    else
        Ok newPitch


semitones : Pitch -> Int
semitones (Pitch pc o) =
    Octave.semitones o + PitchClass.semitones pc


areEnharmonicEquivalents : Pitch -> Pitch -> Bool
areEnharmonicEquivalents lhs rhs =
    semitones lhs == semitones rhs


all : List Pitch
all =
    Octave.all
        |> List.concatMap
            (\o ->
                PitchClass.all
                    |> List.map (\pc -> Pitch pc o)
            )


allForPitchClass : PitchClass -> List Pitch
allForPitchClass thePitchClass =
    Octave.all
        |> List.map
            (\theOctave ->
                fromPitchClass theOctave thePitchClass
            )
        |> List.filterMap Result.toMaybe


firstBelow : PitchClass -> Pitch -> Result PitchError Pitch
firstBelow thePitchClass startPitch =
    let
        maybePitch =
            allForPitchClass thePitchClass
                |> List.filter
                    (\thePitch ->
                        semitones thePitch < semitones startPitch
                    )
                |> List.sortBy semitones
                |> List.reverse
                |> List.head
    in
    case maybePitch of
        Nothing ->
            Err OutOfRange

        Just thePitch ->
            Ok thePitch


firstAbove : PitchClass -> Pitch -> Result PitchError Pitch
firstAbove thePitchClass startPitch =
    let
        maybePitch =
            allForPitchClass thePitchClass
                |> List.filter
                    (\thePitch ->
                        semitones thePitch > semitones startPitch
                    )
                |> List.sortBy semitones
                |> List.head
    in
    case maybePitch of
        Nothing ->
            Err OutOfRange

        Just thePitch ->
            Ok thePitch


errorToString :
    PitchError
    -> String
errorToString error =
    case error of
        InvalidOctave err ->
            Octave.errorToString err

        OutOfRange ->
            "Pitch semitone count is not between the legal range of "
                ++ String.fromInt semitonesLowerLimit
                ++ " and "
                ++ String.fromInt semitonesUpperLimit
                ++ "."

        InternalError ->
            "Something went wrong internally."


transposeUp :
    Interval
    -> Pitch
    -> Result PitchError Pitch
transposeUp =
    transpose PitchClass.transposeUp (+)


transposeDown :
    Interval
    -> Pitch
    -> Result PitchError Pitch
transposeDown =
    transpose PitchClass.transposeDown (-)


transpose :
    (Interval -> PitchClass -> PitchClass)
    -> (Int -> Int -> Int)
    -> Interval
    -> Pitch
    -> Result PitchError Pitch
transpose trans addIntervalSemitones interval p =
    let
        transposedPitchClass =
            pitchClass p
                |> trans interval

        targetOctaveSemitones =
            addIntervalSemitones
                (semitones p)
                (Interval.semitones interval)
                - PitchClass.semitones transposedPitchClass

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
