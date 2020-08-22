module MusicTheory.Interval exposing
    ( Interval
    , perfectUnison, minorSecond, majorSecond, minorThird, majorThird, perfectFourth, perfectFifth, minorSixth, majorSixth, minorSeventh, majorSeventh, perfectOctave
    , augmentedUnison, augmentedSecond, augmentedThird, augmentedFourth, augmentedFifth, augmentedSixth, augmentedSeventh
    , diminishedSecond, diminishedThird, diminishedFourth, diminishedFifth, diminishedSixth, diminishedSeventh, diminishedOctave
    , minorNinth, majorNinth, minorTenth, majorTenth, perfectEleventh, perfectTwelfth, minorThirteenth, majorThirteenth
    , augmentedOctave, augmentedNinth, augmentedEleventh, augmentedTwelfth
    , diminishedTwelfth
    )

{-| An interval is a measure of the distance between two pitches.

@docs Interval


# Interval constructors

Note: if you're not familiar with the way intervals are classified, here's some explanation of the terminology here. Intervals can be:

**Simple or compound**:

[Simple intervals](https://en.wikipedia.org/wiki/Interval_%28music%29#Simple_and_compound) span a distance of an octave or less, and [compound intervals](https://en.wikipedia.org/wiki/Interval_%28music%29#Simple_and_compound) span a distance of more than one octave.

**Diatonic or chromatic**:

[Diatonic intervals](https://en.wikipedia.org/wiki/Interval_%28music%29#Diatonic_and_chromatic) are those found in diatonic scales (major, minor, and perfect intervals), and [chromatic intervals](https://en.wikipedia.org/wiki/Interval_%28music%29#Diatonic_and_chromatic) are those that aren't (augmented and diminished intervals).


## Simple intervals


### Simple diatonic

@docs perfectUnison, minorSecond, majorSecond, minorThird, majorThird, perfectFourth, perfectFifth, minorSixth, majorSixth, minorSeventh, majorSeventh, perfectOctave


### Simple chromatic

@docs augmentedUnison, augmentedSecond, augmentedThird, augmentedFourth, augmentedFifth, augmentedSixth, augmentedSeventh
@docs diminishedSecond, diminishedThird, diminishedFourth, diminishedFifth, diminishedSixth, diminishedSeventh, diminishedOctave


## Compound intervals

Note: This module includes constructors for compound intervals up to the thirteenth, which is the highest interval found in standard chords in tonal music. If you need larger compound intervals, take a look at the `add` or `addOctave` helper functions in this module.


### Compound diatonic

@docs minorNinth, majorNinth, minorTenth, majorTenth, perfectEleventh, perfectTwelfth, minorThirteenth, majorThirteenth


### Compound chromatic

@docs augmentedOctave, augmentedNinth, augmentedEleventh, augmentedTwelfth
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
