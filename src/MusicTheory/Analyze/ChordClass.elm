module MusicTheory.Analyze.ChordClass exposing
    ( isInScaleClass
    , scaleClassesFor
    )

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


scaleClassesFor : ChordClass.ChordClass -> List ScaleClass.ScaleClass
scaleClassesFor chordClass =
    List.filter
        (\scaleClass ->
            isInScaleClass scaleClass chordClass
        )
        ScaleClass.all
