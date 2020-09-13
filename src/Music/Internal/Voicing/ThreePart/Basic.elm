module Music.Internal.Voicing.ThreePart.Basic exposing (basic)

import Music.Internal.ChordType as ChordType exposing (ChordType(..))
import Music.Internal.Interval as Interval exposing (IntervalNumber(..))
import Music.Internal.Voicing.ThreePart as ThreePart


basic : ThreePart.VoicingMethod
basic =
    ThreePart.custom
        ChordType.categorizeFactors
        (\factors ->
            ThreePart.selectFactors
                |> ThreePart.withFactor factors.fifth
                |> ThreePart.withFactor factors.third
                |> ThreePart.withFactor Interval.perfectUnison
                |> ThreePart.placeSelectedFactors spacingLimits
        )


spacingLimits : ThreePart.SpacingLimits
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
