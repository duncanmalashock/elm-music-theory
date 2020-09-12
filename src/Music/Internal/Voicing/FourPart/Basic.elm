module Music.Internal.Voicing.FourPart.Basic exposing (basic)

import Music.Internal.ChordType as ChordType exposing (ChordType(..))
import Music.Internal.Interval as Interval exposing (IntervalNumber(..))
import Music.Internal.Voicing.FourPart as FourPart


basic : FourPart.VoicingMethod
basic =
    FourPart.custom
        ChordType.categorizeFactors
        (\factors ->
            FourPart.selectFactors
                |> FourPart.withFactorFrom
                    (List.filterMap identity
                        [ Just Interval.perfectUnison
                        , factors.sixthOrSeventh
                        ]
                    )
                |> FourPart.withFactor factors.fifth
                |> FourPart.withFactor factors.third
                |> FourPart.withFactor Interval.perfectUnison
                |> FourPart.placeSelectedFactors spacingLimits
        )


spacingLimits : FourPart.SpacingLimits
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
    }
