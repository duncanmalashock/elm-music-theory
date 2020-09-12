module Music.Internal.Voicing.FivePart.Jazz exposing (..)

import Music.Internal.ChordType as ChordType exposing (AvailableTensions)
import Music.Internal.Interval as Interval
import Music.Internal.Voicing.FivePart as FivePart exposing (VoicingMethod)


close : FivePart.VoicingMethod
close =
    [ FivePart.custom
        ChordType.availableTensions
        (\available ->
            FivePart.selectFactors
                |> FivePart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FivePart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FivePart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FivePart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FivePart.withFactorFrom (available.root.true :: available.root.substitutes)
                |> FivePart.placeSelectedFactors spacingLimits
        )
    , FivePart.custom
        ChordType.availableTensions
        (\available ->
            FivePart.selectFactors
                |> FivePart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FivePart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FivePart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FivePart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FivePart.withFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FivePart.placeSelectedFactors spacingLimits
        )
    , FivePart.custom
        ChordType.availableTensions
        (\available ->
            FivePart.selectFactors
                |> FivePart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FivePart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FivePart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FivePart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FivePart.withFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FivePart.placeSelectedFactors spacingLimits
        )
    , FivePart.custom
        ChordType.availableTensions
        (\available ->
            FivePart.selectFactors
                |> FivePart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FivePart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FivePart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FivePart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FivePart.withFactorFrom (available.third.true :: available.third.substitutes)
                |> FivePart.placeSelectedFactors spacingLimits
        )
    ]
        |> FivePart.combineVoicingMethods


drop2 : FivePart.VoicingMethod
drop2 =
    [ FivePart.custom
        ChordType.availableTensions
        (\available ->
            FivePart.selectFactors
                |> FivePart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FivePart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FivePart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FivePart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FivePart.withFactorFrom (available.root.true :: available.root.substitutes)
                |> FivePart.placeSelectedFactors spacingLimits
        )
    , FivePart.custom
        ChordType.availableTensions
        (\available ->
            FivePart.selectFactors
                |> FivePart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FivePart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FivePart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FivePart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FivePart.withFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FivePart.placeSelectedFactors spacingLimits
        )
    , FivePart.custom
        ChordType.availableTensions
        (\available ->
            FivePart.selectFactors
                |> FivePart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FivePart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FivePart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FivePart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FivePart.withFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FivePart.placeSelectedFactors spacingLimits
        )
    , FivePart.custom
        ChordType.availableTensions
        (\available ->
            FivePart.selectFactors
                |> FivePart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FivePart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FivePart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FivePart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FivePart.withFactorFrom (available.third.true :: available.third.substitutes)
                |> FivePart.placeSelectedFactors spacingLimits
        )
    ]
        |> FivePart.combineVoicingMethods


drop3 : FivePart.VoicingMethod
drop3 =
    [ FivePart.custom
        ChordType.availableTensions
        (\available ->
            FivePart.selectFactors
                |> FivePart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FivePart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FivePart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FivePart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FivePart.withFactorFrom (available.root.true :: available.root.substitutes)
                |> FivePart.placeSelectedFactors spacingLimits
        )
    , FivePart.custom
        ChordType.availableTensions
        (\available ->
            FivePart.selectFactors
                |> FivePart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FivePart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FivePart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FivePart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FivePart.withFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FivePart.placeSelectedFactors spacingLimits
        )
    , FivePart.custom
        ChordType.availableTensions
        (\available ->
            FivePart.selectFactors
                |> FivePart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FivePart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FivePart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FivePart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FivePart.withFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FivePart.placeSelectedFactors spacingLimits
        )
    , FivePart.custom
        ChordType.availableTensions
        (\available ->
            FivePart.selectFactors
                |> FivePart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FivePart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FivePart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FivePart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FivePart.withFactorFrom (available.third.true :: available.third.substitutes)
                |> FivePart.placeSelectedFactors spacingLimits
        )
    ]
        |> FivePart.combineVoicingMethods


drop2and4 : FivePart.VoicingMethod
drop2and4 =
    [ FivePart.custom
        ChordType.availableTensions
        (\available ->
            FivePart.selectFactors
                |> FivePart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FivePart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FivePart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FivePart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FivePart.withFactorFrom (available.root.true :: available.root.substitutes)
                |> FivePart.placeSelectedFactors spacingLimits
        )
    , FivePart.custom
        ChordType.availableTensions
        (\available ->
            FivePart.selectFactors
                |> FivePart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FivePart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FivePart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FivePart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FivePart.withFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FivePart.placeSelectedFactors spacingLimits
        )
    , FivePart.custom
        ChordType.availableTensions
        (\available ->
            FivePart.selectFactors
                |> FivePart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FivePart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FivePart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FivePart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FivePart.withFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FivePart.placeSelectedFactors spacingLimits
        )
    , FivePart.custom
        ChordType.availableTensions
        (\available ->
            FivePart.selectFactors
                |> FivePart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FivePart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FivePart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FivePart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FivePart.withFactorFrom (available.third.true :: available.third.substitutes)
                |> FivePart.placeSelectedFactors spacingLimits
        )
    ]
        |> FivePart.combineVoicingMethods


spread : VoicingMethod
spread =
    FivePart.custom
        ChordType.availableTensions
        (\available ->
            FivePart.selectFactors
                |> FivePart.withUniqueFactorFrom
                    (List.concat
                        [ available.root.substitutes
                        , available.third.substitutes
                        , available.fifth.true :: available.fifth.substitutes
                        , available.seventh.substitutes
                        ]
                    )
                |> FivePart.withUniqueFactorFrom
                    (List.concat
                        [ available.root.substitutes
                        , available.third.substitutes
                        , available.fifth.true :: available.fifth.substitutes
                        , available.seventh.substitutes
                        ]
                    )
                |> FivePart.withUniqueFactorFrom
                    [ available.third.true
                    , available.fifth.true
                    , available.seventh.true
                    ]
                |> FivePart.withUniqueFactorFrom
                    [ available.third.true
                    , available.seventh.true
                    ]
                |> FivePart.withUniqueFactor available.root.true
                |> FivePart.placeSelectedFactors spacingLimits
        )


spacingLimits =
    { twoToOne = Interval.range Interval.augmentedUnison Interval.perfectOctave
    , threeToTwo = Interval.range Interval.augmentedUnison Interval.perfectOctave
    , fourToThree = Interval.range Interval.augmentedUnison Interval.perfectOctave
    , fiveToFour = Interval.range Interval.augmentedUnison Interval.perfectOctave
    }
