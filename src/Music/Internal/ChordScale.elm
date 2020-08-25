module Music.Internal.ChordScale exposing (allChordsInScale, chordIsInScale, diatonicChordsAt)

import Music.Internal.Chord as Chord
import Music.Internal.ChordType as ChordType
import Music.Internal.PitchClass as PitchClass
import Music.Internal.Scale as Scale


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
