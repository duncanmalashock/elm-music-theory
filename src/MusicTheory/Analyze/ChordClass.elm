module MusicTheory.Analyze.ChordClass exposing
    ( isInScaleClass
    , scaleClassesFor
    , taggedIntervalsByCategory
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


type alias ChordTonesByCategory =
    { root : List TertianFactors.TaggedInterval
    , third : List TertianFactors.TaggedInterval
    , fifth : List TertianFactors.TaggedInterval
    , seventh : List TertianFactors.TaggedInterval
    }


taggedIntervalsByCategory : ChordClass.ChordClass -> Result ChordClass.ChordClassError ChordTonesByCategory
taggedIntervalsByCategory chordClass =
    ChordClass.toTaggedIntervals chordClass
        |> Result.map
            (\list ->
                { root = List.filter (\( tertianFactor, _ ) -> AnalyzeTertianFactors.isRootToneCategory tertianFactor) list
                , third = List.filter (\( tertianFactor, _ ) -> AnalyzeTertianFactors.isThirdToneCategory tertianFactor) list
                , fifth = List.filter (\( tertianFactor, _ ) -> AnalyzeTertianFactors.isFifthToneCategory tertianFactor) list
                , seventh = List.filter (\( tertianFactor, _ ) -> AnalyzeTertianFactors.isSeventhToneCategory tertianFactor) list
                }
            )
