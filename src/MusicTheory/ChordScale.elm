module MusicTheory.ChordScale exposing (chordIsInScale, diatonicChordsAt)

import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.PitchClass as PitchClass
import MusicTheory.Scale as Scale


diatonicChordsAt :
    { root : PitchClass.PitchClass
    , scale : Scale.Scale
    }
    -> List Chord.Chord
diatonicChordsAt { root, scale } =
    List.map
        (Chord.chord root)
        ChordClass.all
        |> List.filter (chordIsInScale scale)


chordIsInScale : Scale.Scale -> Chord.Chord -> Bool
chordIsInScale scale chord =
    let
        pitchClasses =
            Chord.toPitchClasses chord
    in
    List.all (\pc -> Scale.containsPitchClass pc scale { ignoreSpelling = True }) pitchClasses
