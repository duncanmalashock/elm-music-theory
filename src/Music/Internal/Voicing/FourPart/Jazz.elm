module Music.Internal.Voicing.FourPart.Jazz exposing
    ( close
    , drop2
    , drop2and4
    , drop3
    , spread
    )

import Music.Internal.ChordType as ChordType
import Music.Internal.Interval as Interval
import Music.Internal.Voicing.FourPart as FourPart


spacingLimits : FourPart.SpacingLimits
spacingLimits =
    { twoToOne =
        Interval.range
            Interval.majorSecond
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


close : FourPart.VoicingMethod
close =
    [ FourPart.custom
        ChordType.availableTensions
        (\available ->
            FourPart.selectFactors
                |> FourPart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FourPart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FourPart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FourPart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FourPart.placeSelectedFactors spacingLimits
        )
    , FourPart.custom
        ChordType.availableTensions
        (\available ->
            FourPart.selectFactors
                |> FourPart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FourPart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FourPart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FourPart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FourPart.placeSelectedFactors spacingLimits
        )
    , FourPart.custom
        ChordType.availableTensions
        (\available ->
            FourPart.selectFactors
                |> FourPart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FourPart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FourPart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FourPart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FourPart.placeSelectedFactors spacingLimits
        )
    , FourPart.custom
        ChordType.availableTensions
        (\available ->
            FourPart.selectFactors
                |> FourPart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FourPart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FourPart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FourPart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FourPart.placeSelectedFactors spacingLimits
        )
    ]
        |> FourPart.combineVoicingMethods


drop2 : FourPart.VoicingMethod
drop2 =
    [ FourPart.custom
        ChordType.availableTensions
        (\available ->
            FourPart.selectFactors
                |> FourPart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FourPart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FourPart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FourPart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FourPart.placeSelectedFactors spacingLimits
        )
    , FourPart.custom
        ChordType.availableTensions
        (\available ->
            FourPart.selectFactors
                |> FourPart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FourPart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FourPart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FourPart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FourPart.placeSelectedFactors spacingLimits
        )
    , FourPart.custom
        ChordType.availableTensions
        (\available ->
            FourPart.selectFactors
                |> FourPart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FourPart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FourPart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FourPart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FourPart.placeSelectedFactors spacingLimits
        )
    , FourPart.custom
        ChordType.availableTensions
        (\available ->
            FourPart.selectFactors
                |> FourPart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FourPart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FourPart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FourPart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FourPart.placeSelectedFactors spacingLimits
        )
    ]
        |> FourPart.combineVoicingMethods


drop3 : FourPart.VoicingMethod
drop3 =
    [ FourPart.custom
        ChordType.availableTensions
        (\available ->
            FourPart.selectFactors
                |> FourPart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FourPart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FourPart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FourPart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FourPart.placeSelectedFactors spacingLimits
        )
    , FourPart.custom
        ChordType.availableTensions
        (\available ->
            FourPart.selectFactors
                |> FourPart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FourPart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FourPart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FourPart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FourPart.placeSelectedFactors spacingLimits
        )
    , FourPart.custom
        ChordType.availableTensions
        (\available ->
            FourPart.selectFactors
                |> FourPart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FourPart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FourPart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FourPart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FourPart.placeSelectedFactors spacingLimits
        )
    , FourPart.custom
        ChordType.availableTensions
        (\available ->
            FourPart.selectFactors
                |> FourPart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FourPart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FourPart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FourPart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FourPart.placeSelectedFactors spacingLimits
        )
    ]
        |> FourPart.combineVoicingMethods


drop2and4 : FourPart.VoicingMethod
drop2and4 =
    [ FourPart.custom
        ChordType.availableTensions
        (\available ->
            FourPart.selectFactors
                |> FourPart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FourPart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FourPart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FourPart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FourPart.placeSelectedFactors spacingLimits
        )
    , FourPart.custom
        ChordType.availableTensions
        (\available ->
            FourPart.selectFactors
                |> FourPart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FourPart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FourPart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FourPart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FourPart.placeSelectedFactors spacingLimits
        )
    , FourPart.custom
        ChordType.availableTensions
        (\available ->
            FourPart.selectFactors
                |> FourPart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FourPart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FourPart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FourPart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FourPart.placeSelectedFactors spacingLimits
        )
    , FourPart.custom
        ChordType.availableTensions
        (\available ->
            FourPart.selectFactors
                |> FourPart.withUniqueFactorFrom (available.third.true :: available.third.substitutes)
                |> FourPart.withUniqueFactorFrom (available.seventh.true :: available.seventh.substitutes)
                |> FourPart.withUniqueFactorFrom (available.root.true :: available.root.substitutes)
                |> FourPart.withUniqueFactorFrom (available.fifth.true :: available.fifth.substitutes)
                |> FourPart.placeSelectedFactors spacingLimits
        )
    ]
        |> FourPart.combineVoicingMethods


spread : FourPart.VoicingMethod
spread =
    FourPart.custom
        ChordType.availableTensions
        (\available ->
            FourPart.selectFactors
                |> FourPart.withUniqueFactorFrom
                    (List.concat
                        [ available.root.substitutes
                        , available.third.substitutes
                        , available.fifth.true :: available.fifth.substitutes
                        , available.seventh.substitutes
                        ]
                    )
                |> FourPart.withUniqueFactorFrom
                    [ available.third.true
                    , available.fifth.true
                    , available.seventh.true
                    ]
                |> FourPart.withUniqueFactorFrom
                    [ available.third.true
                    , available.seventh.true
                    ]
                |> FourPart.withUniqueFactor
                    available.root.true
                |> FourPart.placeSelectedFactors spacingLimits
        )
