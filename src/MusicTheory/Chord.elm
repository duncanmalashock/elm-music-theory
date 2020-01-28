module MusicTheory.Chord exposing (chord, toPitchClasses)

import MusicTheory.ChordClass as ChordClass
import MusicTheory.Internal.PitchClass
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.PitchClass as PitchClass


type Chord
    = Chord PitchClass.PitchClass ChordClass.ChordClass


chord : PitchClass.PitchClass -> ChordClass.ChordClass -> Chord
chord root chordClass =
    Chord root chordClass


toPitchClasses : Chord -> List PitchClass.PitchClass
toPitchClasses (Chord root chordClass) =
    List.map
        (\interval ->
            MusicTheory.Internal.PitchClass.transposeUp interval root
        )
        (ChordClass.toIntervals chordClass)
