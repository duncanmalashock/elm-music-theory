module Music.Internal.Chord exposing
    ( Chord
    , chord
    , chordType
    , containsPitchClass
    , detect
    , root
    , symbol
    , toPitchClasses
    )

import Music.Internal.ChordType as ChordType
import Music.Internal.PitchClass as PitchClass


type Chord
    = Chord PitchClass.PitchClass ChordType.ChordType


chord : PitchClass.PitchClass -> ChordType.ChordType -> Chord
chord rootPitchClass theChordType =
    Chord rootPitchClass theChordType


root : Chord -> PitchClass.PitchClass
root (Chord rootPitchClass _) =
    rootPitchClass


symbol : Chord -> String
symbol (Chord rootPitchClass theChordType) =
    PitchClass.toString rootPitchClass ++ ChordType.symbol theChordType


chordType : Chord -> ChordType.ChordType
chordType (Chord _ theChordType) =
    theChordType


toPitchClasses : Chord -> List PitchClass.PitchClass
toPitchClasses (Chord rootPitchClass theChordType) =
    List.map
        (\interval ->
            PitchClass.transpose interval rootPitchClass
        )
        (ChordType.toIntervals theChordType)


containsPitchClass : PitchClass.PitchClass -> Chord -> Bool
containsPitchClass pitchClass theChord =
    List.member pitchClass
        (toPitchClasses theChord)


detect : List ChordType.ChordType -> List PitchClass.PitchClass -> List Chord
detect chordTypesToDetect pitchClasses =
    PitchClass.allInChromaticScale
        |> List.concatMap
            (\theRoot ->
                List.map
                    (\theChordType ->
                        chord theRoot theChordType
                    )
                    chordTypesToDetect
            )
        |> List.filterMap
            (\theChord ->
                if List.all (\pc -> containsPitchClass pc theChord) pitchClasses then
                    Just theChord

                else
                    Nothing
            )
