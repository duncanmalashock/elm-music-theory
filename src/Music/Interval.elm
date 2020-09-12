module Music.Interval exposing
    ( Interval
    , betweenPitches
    , isEqualTo, isGreaterThan, isLessThan
    , add, subtract, simplify, addOctave
    , reverse, isUp, isDown
    , semitones, toString
    , perfectUnison, minorSecond, majorSecond, minorThird, majorThird, perfectFourth, perfectFifth, minorSixth, majorSixth, minorSeventh, majorSeventh, perfectOctave
    , augmentedUnison, augmentedSecond, augmentedThird, augmentedFourth, augmentedFifth, augmentedSixth, augmentedSeventh
    , diminishedSecond, diminishedThird, diminishedFourth, diminishedFifth, diminishedSixth, diminishedSeventh, diminishedOctave
    , minorNinth, majorNinth, minorTenth, majorTenth, perfectEleventh, perfectTwelfth, minorThirteenth, majorThirteenth
    , augmentedOctave, augmentedNinth, augmentedEleventh, augmentedTwelfth
    , diminishedTwelfth
    )

{-| An [interval](https://en.wikipedia.org/wiki/Interval_%28music%29) is a measure of the distance between two pitches. E.g. a "perfect fifth".

@docs Interval

@docs betweenPitches


# Comparison

@docs isEqualTo, isGreaterThan, isLessThan


# Operations

@docs add, subtract, simplify, addOctave


# Direction

Intervals have direction: a perfect fifth up is different from a perfect fifth down.

@docs reverse, isUp, isDown


# Conversion

@docs semitones, toString


# Constructors


## Simple intervals


### Diatonic

@docs perfectUnison, minorSecond, majorSecond, minorThird, majorThird, perfectFourth, perfectFifth, minorSixth, majorSixth, minorSeventh, majorSeventh, perfectOctave


### Chromatic

@docs augmentedUnison, augmentedSecond, augmentedThird, augmentedFourth, augmentedFifth, augmentedSixth, augmentedSeventh
@docs diminishedSecond, diminishedThird, diminishedFourth, diminishedFifth, diminishedSixth, diminishedSeventh, diminishedOctave


## Compound intervals

Note: This module includes constructors for compound intervals up to the thirteenth, which is the highest interval found in standard chords in tonal music. If you need larger compound intervals, take a look at the `add` or `addOctave` helper functions in this module.


### Diatonic

@docs minorNinth, majorNinth, minorTenth, majorTenth, perfectEleventh, perfectTwelfth, minorThirteenth, majorThirteenth


### Chromatic

@docs augmentedOctave, augmentedNinth, augmentedEleventh, augmentedTwelfth
@docs diminishedTwelfth

-}

import Music.Internal.Interval as Interval
import Music.Internal.Pitch as Pitch


{-| -}
type alias Interval =
    Interval.Interval


{-| Get the interval between two pitches:

    betweenPitches Pitch.c4 Pitch.g4 == perfectFifth

-}
betweenPitches : Pitch.Pitch -> Pitch.Pitch -> Interval
betweenPitches a b =
    Pitch.intervalBetween a b


{-| Check whether two intervals are equivalent:

    isEqualTo augmentedFourth diminishedFifth == True

-}
isEqualTo : Interval -> Interval -> Bool
isEqualTo a b =
    Interval.isEqualTo a b


{-| Check whether the distance of one interval is greater than another:

    isGreaterThan perfectFifth perfectUnison == True

-}
isGreaterThan : Interval -> Interval -> Bool
isGreaterThan a b =
    Interval.isGreaterThan a b


{-| Check whether the distance of one interval is less than another:

    isLessThan perfectFifth perfectUnison == False

-}
isLessThan : Interval -> Interval -> Bool
isLessThan a b =
    Interval.isLessThan a b


{-| Add two intervals together:

    add majorSecond majorThird == augmentedFourth

-}
add : Interval -> Interval -> Interval
add a b =
    Interval.add a b


{-| Subtract two intervals:

    subtract majorSecond minorThird == minorSecond

-}
subtract : Interval -> Interval -> Interval
subtract a b =
    Interval.subtract a b


{-| Reverse the direction of an interval
-}
reverse : Interval -> Interval
reverse interval =
    Interval.reverse interval


{-| Find out whether an interval is in the "up" direction:

    isUp majorThird == True

-}
isUp : Interval -> Bool
isUp interval =
    Interval.isPositive interval


{-| Find out whether an interval is in the "down" direction:

    isDown (reverse majorThird) == True

-}
isDown : Interval -> Bool
isDown interval =
    Interval.isNegative interval


{-| Get the [short name](https://en.wikipedia.org/wiki/Interval_%28music%29#Alternative_interval_naming_conventions) of an interval:

    toString perfectFifth == "P5"

-}
toString : Interval -> String
toString interval =
    Interval.shortName interval


{-| Get the number of [semitones](https://en.wikipedia.org/wiki/Semitone) an interval spans:

    semitones majorThird == 4

-}
semitones : Interval -> Int
semitones interval =
    Interval.semitones interval


{-| Convert any interval to its [simple](https://en.wikipedia.org/wiki/Interval_%28music%29#Simple_and_compound) form. A simple interval is one that spans an octave or less.

    simplify majorThirteenth == majorSixth

-}
simplify : Interval -> Interval
simplify interval =
    Interval.toSimple interval


{-| Add an octave to an interval:

    addOctave majorSecond == majorNinth

-}
addOctave : Interval -> Interval
addOctave interval =
    Interval.addOctave interval


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
