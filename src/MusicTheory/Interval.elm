module MusicTheory.Interval exposing
    ( Interval
    , perfectUnison, minorSecond, majorSecond, minorThird, majorThird, perfectFourth, perfectFifth, minorSixth, majorSixth, minorSeventh, majorSeventh, perfectOctave
    , augmentedUnison, augmentedSecond, augmentedThird, augmentedFourth, augmentedFifth, augmentedSixth, augmentedSeventh, augmentedOctave
    , diminishedSecond, diminishedThird, diminishedFourth, diminishedFifth, diminishedSixth, diminishedSeventh, diminishedOctave
    , minorNinth, majorNinth, minorTenth, majorTenth, perfectEleventh, perfectTwelfth, minorThirteenth, majorThirteenth
    , augmentedNinth, augmentedEleventh, augmentedTwelfth
    , diminishedTwelfth
    )

{-|

@docs Interval


# Interval constructors


## Simple intervals


### Diatonic

@docs perfectUnison, minorSecond, majorSecond, minorThird, majorThird, perfectFourth, perfectFifth, minorSixth, majorSixth, minorSeventh, majorSeventh, perfectOctave


### Chromatic

@docs augmentedUnison, augmentedSecond, augmentedThird, augmentedFourth, augmentedFifth, augmentedSixth, augmentedSeventh, augmentedOctave
@docs diminishedSecond, diminishedThird, diminishedFourth, diminishedFifth, diminishedSixth, diminishedSeventh, diminishedOctave


## Compound intervals


### Diatonic

@docs minorNinth, majorNinth, minorTenth, majorTenth, perfectEleventh, perfectTwelfth, minorThirteenth, majorThirteenth


### Chromatic

@docs augmentedNinth, augmentedEleventh, augmentedTwelfth
@docs diminishedTwelfth

-}

import MusicTheory.Internal.Interval as Interval


{-| -}
type alias Interval =
    Interval.Interval


{-| -}
perfectUnison : Interval
perfectUnison =
    Interval.perfectUnison


{-| -}
diminishedSecond : Interval
diminishedSecond =
    Interval.diminishedSecond


{-| -}
minorSecond : Interval
minorSecond =
    Interval.minorSecond


{-| -}
augmentedUnison : Interval
augmentedUnison =
    Interval.augmentedUnison


{-| -}
majorSecond : Interval
majorSecond =
    Interval.majorSecond


{-| -}
diminishedThird : Interval
diminishedThird =
    Interval.diminishedThird


{-| -}
minorThird : Interval
minorThird =
    Interval.minorThird


{-| -}
augmentedSecond : Interval
augmentedSecond =
    Interval.augmentedSecond


{-| -}
majorThird : Interval
majorThird =
    Interval.majorThird


{-| -}
diminishedFourth : Interval
diminishedFourth =
    Interval.diminishedFourth


{-| -}
perfectFourth : Interval
perfectFourth =
    Interval.perfectFourth


{-| -}
augmentedThird : Interval
augmentedThird =
    Interval.augmentedThird


{-| -}
augmentedFourth : Interval
augmentedFourth =
    Interval.augmentedFourth


{-| -}
diminishedFifth : Interval
diminishedFifth =
    Interval.diminishedFifth


{-| -}
perfectFifth : Interval
perfectFifth =
    Interval.perfectFifth


{-| -}
diminishedSixth : Interval
diminishedSixth =
    Interval.diminishedSixth


{-| -}
augmentedFifth : Interval
augmentedFifth =
    Interval.augmentedFifth


{-| -}
minorSixth : Interval
minorSixth =
    Interval.minorSixth


{-| -}
majorSixth : Interval
majorSixth =
    Interval.majorSixth


{-| -}
diminishedSeventh : Interval
diminishedSeventh =
    Interval.diminishedSeventh


{-| -}
minorSeventh : Interval
minorSeventh =
    Interval.minorSeventh


{-| -}
augmentedSixth : Interval
augmentedSixth =
    Interval.augmentedSixth


{-| -}
majorSeventh : Interval
majorSeventh =
    Interval.majorSeventh


{-| -}
augmentedOctave : Interval
augmentedOctave =
    Interval.augmentedOctave


{-| -}
diminishedOctave : Interval
diminishedOctave =
    Interval.diminishedOctave


{-| -}
perfectOctave : Interval
perfectOctave =
    Interval.perfectOctave


{-| -}
augmentedSeventh : Interval
augmentedSeventh =
    Interval.augmentedSeventh


{-| -}
minorNinth : Interval
minorNinth =
    Interval.minorNinth


{-| -}
majorNinth : Interval
majorNinth =
    Interval.majorNinth


{-| -}
augmentedNinth : Interval
augmentedNinth =
    Interval.augmentedNinth


{-| -}
minorTenth : Interval
minorTenth =
    Interval.minorTenth


{-| -}
majorTenth : Interval
majorTenth =
    Interval.majorTenth


{-| -}
perfectEleventh : Interval
perfectEleventh =
    Interval.perfectEleventh


{-| -}
augmentedEleventh : Interval
augmentedEleventh =
    Interval.augmentedEleventh


{-| -}
diminishedTwelfth : Interval
diminishedTwelfth =
    Interval.diminishedTwelfth


{-| -}
perfectTwelfth : Interval
perfectTwelfth =
    Interval.perfectTwelfth


{-| -}
augmentedTwelfth : Interval
augmentedTwelfth =
    Interval.augmentedTwelfth


{-| -}
minorThirteenth : Interval
minorThirteenth =
    Interval.minorThirteenth


{-| -}
majorThirteenth : Interval
majorThirteenth =
    Interval.majorThirteenth
