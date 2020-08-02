module MusicTheory.ChordScale exposing (chordIsInScale, diatonicChordsAtInterval)

import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.Interval as Interval
import MusicTheory.PitchClass as PitchClass
import MusicTheory.Scale as Scale


diatonicChordsAtInterval :
    { targetChord : Chord.Chord
    , approachByInterval : Interval.Interval
    , scale : Scale.Scale
    }
    -> List Chord.Chord
diatonicChordsAtInterval { targetChord, scale, approachByInterval } =
    targetChord
        |> Chord.root
        |> PitchClass.transpose approachByInterval
        |> (\root ->
                List.map
                    (Chord.chord root)
                    ChordClass.all
           )
        |> List.filter (chordIsInScale scale)


chordIsInScale : Scale.Scale -> Chord.Chord -> Bool
chordIsInScale scale chord =
    let
        pitchClasses =
            Chord.toPitchClasses chord
    in
    List.all (\pc -> Scale.containsPitchClass pc scale) pitchClasses
