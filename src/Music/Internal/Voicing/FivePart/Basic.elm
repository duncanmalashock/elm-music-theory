module Music.Internal.Voicing.FivePart.Basic exposing (basic)

import Music.Internal.ChordType as ChordType exposing (ChordType(..))
import Music.Internal.Interval as Interval exposing (IntervalNumber(..))
import Music.Internal.Voicing.FivePart as FivePart


basic : FivePart.VoicingMethod
basic =
    FivePart.custom
        ChordType.categorizeFactors
        (\factors ->
            FivePart.selectFactors
                |> FivePart.withFactorFrom
                    (List.concat
                        [ [ factors.third ]
                        , factors.ninth
                        ]
                    )
                |> FivePart.withFactorFrom
                    (List.filterMap identity
                        [ Just Interval.perfectUnison
                        , factors.sixthOrSeventh
                        ]
                    )
                |> FivePart.withFactor factors.fifth
                |> FivePart.withFactor factors.third
                |> FivePart.withFactor Interval.perfectUnison
                |> FivePart.placeSelectedFactors spacingLimits
        )


spacingLimits : FivePart.SpacingLimits
spacingLimits =
    { twoToOne =
        Interval.range
            Interval.augmentedUnison
            Interval.perfectOctave
    , threeToTwo =
        Interval.range
            Interval.augmentedUnison
            Interval.perfectOctave
    , fourToThree =
        Interval.range
            Interval.augmentedUnison
            Interval.perfectOctave
    , fiveToFour =
        Interval.range
            Interval.augmentedUnison
            Interval.perfectOctave
    }
