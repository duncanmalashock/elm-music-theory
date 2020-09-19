module Music.Internal.Voicing.ThreePart.Basic exposing (basic, shell)

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


shell : ThreePart.VoicingMethod
shell =
    ThreePart.custom
        ChordType.categorizeFactors
        (\factors ->
            ThreePart.selectFactors
                |> ThreePart.withUniqueFactorFrom
                    (List.filterMap identity
                        [ Just factors.third
                        , factors.sixthOrSeventh
                        ]
                    )
                |> ThreePart.withUniqueFactorFrom
                    (List.filterMap identity
                        [ Just factors.third
                        , factors.sixthOrSeventh
                        ]
                    )
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
    }
