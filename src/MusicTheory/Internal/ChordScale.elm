module MusicTheory.Internal.ChordScale exposing (allChordsInScale, chordIsInScale, diatonicChordsAt)

import MusicTheory.Internal.Chord as Chord
import MusicTheory.Internal.ChordType as ChordType
import MusicTheory.Internal.PitchClass as PitchClass
import MusicTheory.Internal.Scale as Scale


diatonicChordsAt :
    { root : PitchClass.PitchClass
    , scale : Scale.Scale
    , chordTypesAllowed : List ChordType.ChordType
    }
    -> List Chord.Chord
diatonicChordsAt { root, scale, chordTypesAllowed } =
    List.map
        (Chord.chord root)
        chordTypesAllowed
        |> List.filter (chordIsInScale scale)


allChordsInScale : List ChordType.ChordType -> Scale.Scale -> List Chord.Chord
allChordsInScale chordTypesAllowed scale =
    Scale.toList scale
        |> List.concatMap
            (\pitchClass ->
                diatonicChordsAt
                    { root = pitchClass
                    , scale = scale
                    , chordTypesAllowed = chordTypesAllowed
                    }
            )


chordIsInScale : Scale.Scale -> Chord.Chord -> Bool
chordIsInScale scale chord =
    let
        pitchClasses =
            Chord.toPitchClasses chord
    in
    List.all
        (\pc ->
            Scale.containsPitchClass pc scale { ignoreSpelling = True }
        )
        pitchClasses
