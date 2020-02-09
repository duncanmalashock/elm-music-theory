module MusicTheory.Analyze.ChordClass exposing (isInScaleClass)

import MusicTheory.ChordClass as ChordClass
import MusicTheory.ScaleClass as ScaleClass


isInScaleClass : ScaleClass.ScaleClass -> ChordClass.ChordClass -> Bool
isInScaleClass scaleClass chordClass =
    List.all
        (\interval ->
            List.member interval
                (ScaleClass.toIntervals scaleClass)
        )
        (ChordClass.toIntervals chordClass)
