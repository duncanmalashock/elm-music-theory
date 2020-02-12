module MusicTheory.Chord exposing
    ( Chord
    , chord
    , chordClass
    , root
    , toPitchClasses
    )

import MusicTheory.ChordClass as ChordClass
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.PitchClass as PitchClass


type Chord
    = Chord PitchClass.PitchClass ChordClass.ChordClass


chord : PitchClass.PitchClass -> ChordClass.ChordClass -> Chord
chord rootPitchClass theChordClass =
    Chord rootPitchClass theChordClass


root : Chord -> PitchClass.PitchClass
root (Chord rootPitchClass _) =
    rootPitchClass


chordClass : Chord -> ChordClass.ChordClass
chordClass (Chord _ theChordClass) =
    theChordClass


toPitchClasses : Chord -> List PitchClass.PitchClass
toPitchClasses (Chord rootPitchClass theChordClass) =
    List.map
        (\interval ->
            PitchClass.transposeUp interval rootPitchClass
        )
        (ChordClass.toIntervals theChordClass)
