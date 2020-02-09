module MusicTheory.Chord exposing (Chord, chord, root, toPitchClasses)

import MusicTheory.ChordClass as ChordClass
import MusicTheory.Internal.PitchClass
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.PitchClass as PitchClass


type Chord
    = Chord PitchClass.PitchClass ChordClass.ChordClass


chord : PitchClass.PitchClass -> ChordClass.ChordClass -> Chord
chord rootPitchClass chordClass =
    Chord rootPitchClass chordClass


root : Chord -> PitchClass.PitchClass
root (Chord rootPitchClass _) =
    rootPitchClass


toPitchClasses : Chord -> List PitchClass.PitchClass
toPitchClasses (Chord rootPitchClass chordClass) =
    List.map
        (\interval ->
            MusicTheory.Internal.PitchClass.transposeUp interval rootPitchClass
        )
        (ChordClass.toIntervals chordClass)
