module Internal.Chord exposing
    ( Chord
    , chord
    , chordClass
    , containsPitchClass
    , root
    , toPitchClasses
    )

import Internal.ChordClass as ChordClass
import Internal.PitchClass as PitchClass


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
            PitchClass.transpose interval rootPitchClass
        )
        (ChordClass.toIntervals theChordClass)


containsPitchClass : PitchClass.PitchClass -> Chord -> Bool
containsPitchClass pitchClass theChord =
    List.member pitchClass
        (toPitchClasses theChord)
