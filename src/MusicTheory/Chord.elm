module MusicTheory.Chord exposing
    ( Chord
    , ChordError(..)
    , allChords
    , chord
    , chordClass
    , inversion
    , possibleInversions
    , root
    , toPitchClasses
    , toPitchesFromOctave
    , withInversion
    )

import MusicTheory.ChordClass as ChordClass
import MusicTheory.Inversion as Inversion
import MusicTheory.Octave as Octave
import MusicTheory.Pitch as Pitch
import MusicTheory.PitchClass as PitchClass
import Result.Extra
import Util.List


type Chord
    = Chord PitchClass.PitchClass ChordClass.ChordClass Inversion.Inversion


type ChordError
    = InvalidInversion
    | PitchConversionError


chord : PitchClass.PitchClass -> ChordClass.ChordClass -> Chord
chord rootPitchClass theChordClass =
    Chord rootPitchClass theChordClass Inversion.none


root : Chord -> PitchClass.PitchClass
root (Chord rootPitchClass _ _) =
    rootPitchClass


chordClass : Chord -> ChordClass.ChordClass
chordClass (Chord _ theChordClass _) =
    theChordClass


inversion : Chord -> Inversion.Inversion
inversion (Chord _ _ theInversion) =
    theInversion


toPitchClasses : Chord -> List PitchClass.PitchClass
toPitchClasses (Chord rootPitchClass theChordClass theInversion) =
    ChordClass.toIntervals theChordClass
        |> List.map
            (\interval -> PitchClass.transpose interval rootPitchClass)
        |> Util.List.rotateLeft (Inversion.toInt theInversion)


toPitchesFromOctave : Octave.Octave -> Chord -> Result ChordError (List Pitch.Pitch)
toPitchesFromOctave octave theChord =
    case toPitchClasses theChord of
        [] ->
            Ok []

        rootPitchClass :: remainingPitchClasses ->
            let
                theRoot =
                    Ok <| Pitch.fromPitchClass octave rootPitchClass
            in
            remainingPitchClasses
                |> List.foldl
                    (\pc ( pitchResult, acc ) ->
                        let
                            next =
                                Result.andThen
                                    (\pitch -> Pitch.firstAbove pc pitch)
                                    pitchResult
                        in
                        ( next, List.append acc [ next ] )
                    )
                    ( theRoot, [ theRoot ] )
                |> Tuple.second
                |> Result.Extra.combine
                |> Result.mapError (\_ -> PitchConversionError)


allChords : Chord -> List (List Pitch.Pitch)
allChords thisChord =
    Octave.allValid
        |> List.map (\octave -> toPitchesFromOctave octave thisChord)
        |> Result.Extra.partition
        |> Tuple.first


withInversion : Inversion.Inversion -> Chord -> Result ChordError Chord
withInversion theInversion theChord =
    case List.member theInversion (possibleInversions theChord) of
        True ->
            case theChord of
                Chord theRoot theChordClass _ ->
                    Ok (Chord theRoot theChordClass theInversion)

        False ->
            Err InvalidInversion


possibleInversions : Chord -> List Inversion.Inversion
possibleInversions theChord =
    toPitchClasses theChord
        |> List.length
        |> (+) -1
        |> List.range 0
        |> List.map Inversion.fromInt
