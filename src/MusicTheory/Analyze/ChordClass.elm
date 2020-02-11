module MusicTheory.Analyze.ChordClass exposing
    ( isInScaleClass
    , scaleClassesFor
    , taggedIntervalsInFifthCategoryFor
    , taggedIntervalsInRootCategoryFor
    , taggedIntervalsInSeventhCategoryFor
    , taggedIntervalsInThirdCategoryFor
    )

import MusicTheory.Analyze.TertianFactors as AnalyzeTertianFactors
import MusicTheory.ChordClass as ChordClass
import MusicTheory.ScaleClass as ScaleClass
import MusicTheory.TertianFactors as TertianFactors


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


taggedIntervalsInRootCategoryFor : ChordClass.ChordClass -> Result ChordClass.ChordClassError (List TertianFactors.TaggedInterval)
taggedIntervalsInRootCategoryFor chordClass =
    ChordClass.toTaggedIntervals chordClass
        |> Result.map
            (\list ->
                List.filter
                    (\( tertianFactor, _ ) ->
                        AnalyzeTertianFactors.isRootToneCategory tertianFactor
                    )
                    list
            )


taggedIntervalsInThirdCategoryFor : ChordClass.ChordClass -> Result ChordClass.ChordClassError (List TertianFactors.TaggedInterval)
taggedIntervalsInThirdCategoryFor chordClass =
    ChordClass.toTaggedIntervals chordClass
        |> Result.map
            (\list ->
                List.filter
                    (\( tertianFactor, _ ) ->
                        AnalyzeTertianFactors.isThirdToneCategory tertianFactor
                    )
                    list
            )


taggedIntervalsInFifthCategoryFor : ChordClass.ChordClass -> Result ChordClass.ChordClassError (List TertianFactors.TaggedInterval)
taggedIntervalsInFifthCategoryFor chordClass =
    ChordClass.toTaggedIntervals chordClass
        |> Result.map
            (\list ->
                List.filter
                    (\( tertianFactor, _ ) ->
                        AnalyzeTertianFactors.isFifthToneCategory tertianFactor
                    )
                    list
            )


taggedIntervalsInSeventhCategoryFor : ChordClass.ChordClass -> Result ChordClass.ChordClassError (List TertianFactors.TaggedInterval)
taggedIntervalsInSeventhCategoryFor chordClass =
    ChordClass.toTaggedIntervals chordClass
        |> Result.map
            (\list ->
                List.filter
                    (\( tertianFactor, _ ) ->
                        AnalyzeTertianFactors.isSeventhToneCategory tertianFactor
                    )
                    list
            )
