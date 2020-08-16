module MusicTheory.Internal.ChordScale exposing (allChordsInScale, chordIsInScale, diatonicChordsAt)

import MusicTheory.Internal.Chord as Chord
import MusicTheory.Internal.ChordClass as ChordClass
import MusicTheory.Internal.PitchClass as PitchClass
import MusicTheory.Internal.Scale as Scale


diatonicChordsAt :
    { root : PitchClass.PitchClass
    , scale : Scale.Scale
    , chordClassesAllowed : List ChordClass.ChordClass
    }
    -> List Chord.Chord
diatonicChordsAt { root, scale, chordClassesAllowed } =
    List.map
        (Chord.chord root)
        chordClassesAllowed
        |> List.filter (chordIsInScale scale)


allChordsInScale : Scale.Scale -> List Chord.Chord
allChordsInScale scale =
    Scale.toList scale
        |> List.concatMap
            (\pitchClass ->
                diatonicChordsAt
                    { root = pitchClass
                    , scale = scale
                    , chordClassesAllowed = ChordClass.all
                    }
            )


chordIsInScale : Scale.Scale -> Chord.Chord -> Bool
chordIsInScale scale chord =
    let
        pitchClasses =
            Chord.toPitchClasses chord
    in
    List.all (\pc -> Scale.containsPitchClass pc scale { ignoreSpelling = True }) pitchClasses
