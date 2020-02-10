module MusicTheory.Analyze.Chord exposing (containsPitchClass)

import MusicTheory.Chord as Chord
import MusicTheory.PitchClass as PitchClass


containsPitchClass : PitchClass.PitchClass -> Chord.Chord -> Bool
containsPitchClass pitchClass chord =
    List.member pitchClass
        (Chord.toPitchClasses chord)
