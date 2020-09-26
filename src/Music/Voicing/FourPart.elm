module Music.Voicing.FourPart exposing
    ( Voicing
    , voicing
    , chord, span, center
    , voiceOne, voiceTwo, voiceThree, voiceFour
    , containsPitchInVoiceOne, containsPitchInVoiceTwo, containsPitchInVoiceThree, containsPitchInVoiceFour
    , commonTones
    , usesContraryMotion, containsParallelFifths, containsParallelOctaves
    , totalSemitoneDistance, semitoneDistanceVoiceOne, semitoneDistanceVoiceTwo, semitoneDistanceVoiceThree, semitoneDistanceVoiceFour
    , isWithinLowIntervalLimits
    , sortWeighted, orderWeighted
    , centerOrder, commonTonesOrder, contraryMotionOrder, totalSemitoneDistanceOrder, semitoneDistanceVoiceOneOrder, semitoneDistanceVoiceTwoOrder, semitoneDistanceVoiceThreeOrder, semitoneDistanceVoiceFourOrder
    , Pitches, toPitches, toPitchList, toString
    , Intervals, toIntervals, toIntervalList
    , basic
    , close, drop2, drop3, drop2and4, spread
    , rootPosition, firstInversion, secondInversion, thirdInversion
    , VoicingMethod
    , SpacingLimits
    , method
    , selectFactors, withFactor, withUniqueFactor, withFactorFrom, withUniqueFactorFrom, withTwoFactorsFrom, withUniqueTwoFactorsFrom, withThreeFactorsFrom, withUniqueThreeFactorsFrom
    , placeSelectedFactors
    , combineVoicingMethods
    )

{-| A chord voicing is an instance of a `Chord` that can be played or sung.

@docs Voicing


# Generating voicings

This package's recommended way of creating four-part `Voicing`s is to use the [Chord.voiceFourParts](Music-Chord#voicing-chords) function along with `VoicingMethod`s like the ones in this module:

    Chord.voiceFourParts
        { voiceOne = Range.sopranoVoice
        , voiceTwo = Range.altoVoice
        , voiceThree = Range.tenorVoice
        , voiceFour = Range.bassVoice
        }
        [ basic ]
        (Chord.majorSeventh PitchClass.c)

In this example, we pass the following to `voiceFourParts`:

1.  The `Range`s of the instruments involved
2.  A list of `VoicingMethod`s to be used
3.  The chord to voice

This uses the characteristics of the `VoicingMethod` to return a `List` of all possible `Voicing`s.

From here, you may choose from this list based on some criteria:

    myGeneratedVoicings
        |> List.filter
            (\voicing ->
                containsPitchInVoiceOne Pitch.e5 voicing
                    && (span voicing >= Interval.perfectOctave)
            )
        |> List.sortWith (centerOrder Pitch.g4)
        |> List.head

This does the following:

1.  Filters the list to only include `Voicing`s which have E5 in the top voice, and have a span of at least an octave from bottom to top voice
2.  Sorts the list so that the `Voicing`s which are centered around the pitch G4 are at the beginning
3.  Takes the first item in the remaining list

These sorting and filtering processes allow you to account for considerations like [voice leading](https://en.wikipedia.org/wiki/Voice_leading), or the harmonization of a melody. You'll find comparison functions in this module for doing that.


# Constructing a single voicing

There are cases where you may want to create a specific voicing you have in mind:

@docs voicing


# Helpers

@docs chord, span, center


# Voices

@docs voiceOne, voiceTwo, voiceThree, voiceFour
@docs containsPitchInVoiceOne, containsPitchInVoiceTwo, containsPitchInVoiceThree, containsPitchInVoiceFour


# Comparing voicings


## Common tones

@docs commonTones


## Direction

@docs usesContraryMotion, containsParallelFifths, containsParallelOctaves


## Distance

@docs totalSemitoneDistance, semitoneDistanceVoiceOne, semitoneDistanceVoiceTwo, semitoneDistanceVoiceThree, semitoneDistanceVoiceFour


# Low interval limits

@docs isWithinLowIntervalLimits


# Sorting voicings

@docs sortWeighted, orderWeighted
@docs centerOrder, commonTonesOrder, contraryMotionOrder, totalSemitoneDistanceOrder, semitoneDistanceVoiceOneOrder, semitoneDistanceVoiceTwoOrder, semitoneDistanceVoiceThreeOrder, semitoneDistanceVoiceFourOrder


# Conversion

@docs Pitches, toPitches, toPitchList, toString


## Intervals

@docs Intervals, toIntervals, toIntervalList


# Voicing methods


## Basic

@docs basic


## Jazz

These methods were adapted from [Jazz Arranging Techniques](http://lindsayjazz.com/jazz-arranging-techniques/) by Gary Lindsay.

Notes:

  - Jazz voicing methods can involve substitutions within families of related chords, so don't be surprised when you see voicings that include a pitch class that isn't strictly in the chord you specified.
  - Not all chords are compatible with these methods; in those cases this function will return an empty list.

@docs close, drop2, drop3, drop2and4, spread


## Classical

Note: these classical methods were developed before jazz harmony, and so chord extensions and added tones will not be included.

@docs rootPosition, firstInversion, secondInversion, thirdInversion


## Custom voicing methods

Voicing methods are a deep and nuanced topic, that I hope to make approachable. I've modeled this API with three main concepts of chord voicing in mind:

  - **Spacing**: how close together or far apart can each voice be from its neighbor?
  - **Placement**: which factors from the chord can be used in which places?
  - **Uniqueness**: can certain factors be repeated, or must they be used only once?

Here are the steps in building a voicing method:


### Categorize the factors in the chord

Since chords can vary, use some method of categorizing the factors of a chord into groups that you can guarantee are present. Examples of this type of categorization function are `Chord.categorizeFactors` and `Chord.availableTensions`.


### Specify the placement and uniqueness of factors

Select factors using the `with...` functions in this section.


### Finish and specify spacing limits

Use the `placeSelectedFactors` function, passing a `SpacingLimits`.

Example:

    myCustomVoicingMethod : VoicingMethod
    myCustomVoicingMethod =
        custom
            ChordType.categorizeFactors
            (\factors ->
                selectFactors
                    |> withFactorFrom
                        (List.filterMap identity
                            [ Just Interval.perfectUnison
                            , factors.sixthOrSeventh
                            ]
                        )
                    |> withFactor factors.fifth
                    |> withFactor factors.third
                    |> withFactor Interval.perfectUnison
                    |> placeSelectedFactors spacingLimits
            )

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

@docs VoicingMethod
@docs SpacingLimits
@docs method
@docs selectFactors, withFactor, withUniqueFactor, withFactorFrom, withUniqueFactorFrom, withTwoFactorsFrom, withUniqueTwoFactorsFrom, withThreeFactorsFrom, withUniqueThreeFactorsFrom
@docs placeSelectedFactors
@docs combineVoicingMethods

-}

import Music.Internal.Chord as Chord
import Music.Internal.ChordType as ChordType
import Music.Internal.Interval as Interval
import Music.Internal.Pitch as Pitch
import Music.Internal.Voicing as Voicing
import Music.Internal.Voicing.FourPart as FourPart
import Music.Internal.Voicing.FourPart.Basic as FourPartBasic
import Music.Internal.Voicing.FourPart.Classical as FourPartClassical
import Music.Internal.Voicing.FourPart.Jazz as FourPartJazz
import Music.Internal.VoicingClass as VoicingClass
import Music.PitchClass as PitchClass


{-| -}
type alias Voicing =
    FourPart.Voicing


{-| -}
type alias VoicingMethod =
    FourPart.VoicingMethod


{-| Combine multiple `VoicingMethod`s together:

    myComboVoicingMethod =
        combineVoicingMethods [ voicingMethodOne, voicingMethodTwo, voicingMethodThree ]

This is useful if you want to define multiple `VoicingMethod`s that act as a group (e.g. inversions or similar methods that use different chord factors).

-}
combineVoicingMethods : List VoicingMethod -> VoicingMethod
combineVoicingMethods voicingMethodsToCombine =
    FourPart.combineVoicingMethods voicingMethodsToCombine


type alias InstrumentRanges =
    { voiceOne : Pitch.Range
    , voiceTwo : Pitch.Range
    , voiceThree : Pitch.Range
    , voiceFour : Pitch.Range
    }


{-| -}
type alias SpacingLimits =
    { twoToOne : Interval.Range
    , threeToTwo : Interval.Range
    , fourToThree : Interval.Range
    }


{-| Begin a custom voicing method:

    method Chord.categorizeFactors
        (\categorized ->
            ...
        )

-}
method :
    (ChordType.ChordType -> Maybe categorized)
    -> (categorized -> List FourPart.VoicingClass)
    -> VoicingMethod
method categorizeFn buildFromCategorized =
    FourPart.custom categorizeFn buildFromCategorized


{-| Select a chord factor for use in a voice.
-}
withFactor :
    Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withFactor factor builder =
    FourPart.withFactor factor builder


{-| Select a factor that has not yet been used.
-}
withUniqueFactor :
    Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withUniqueFactor factor builder =
    FourPart.withUniqueFactor factor builder


{-| Select a factor from a list of options.
-}
withFactorFrom :
    List Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withFactorFrom options builder =
    FourPart.withFactorFrom options builder


{-| Select a factor that has not yet been used, from a list of options.
-}
withUniqueFactorFrom :
    List Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withUniqueFactorFrom options builder =
    FourPart.withUniqueFactorFrom options builder


{-| Select two factors from a list of options.
-}
withTwoFactorsFrom :
    List Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withTwoFactorsFrom options builder =
    FourPart.withTwoFactorsFrom options builder


{-| Select two factors that have not yet been used, from a list of options.
-}
withUniqueTwoFactorsFrom :
    List Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withUniqueTwoFactorsFrom options builder =
    FourPart.withUniqueTwoFactorsFrom options builder


{-| Select three factors from a list of options.
-}
withThreeFactorsFrom :
    List Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> Interval.Interval -> Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withThreeFactorsFrom options builder =
    FourPart.withThreeFactorsFrom options builder


{-| Select two factors that have not yet been used, from a list of options.
-}
withUniqueThreeFactorsFrom :
    List Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> Interval.Interval -> Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withUniqueThreeFactorsFrom options builder =
    FourPart.withUniqueThreeFactorsFrom options builder


{-| Begin selecting factors for a voicing method.
-}
selectFactors :
    VoicingClass.VoicingClassBuilder
        (Interval.Interval
         -> Interval.Interval
         -> Interval.Interval
         -> Interval.Interval
         -> FourPart.VoicingClass
        )
selectFactors =
    FourPart.selectFactors


{-| Finish selecting factors for a voicing method and place them according to the spacing limits.
-}
placeSelectedFactors :
    SpacingLimits
    -> VoicingClass.VoicingClassBuilder FourPart.VoicingClass
    -> List FourPart.VoicingClass
placeSelectedFactors voiceIntervalLimits builder =
    FourPart.placeSelectedFactors voiceIntervalLimits builder


{-| The pitches contained in a voicing.

These are in order from highest (`voiceOne`) to lowest (`voiceFour`), the way you might read them on a staff.

-}
type alias Pitches =
    { voiceOne : Pitch.Pitch
    , voiceTwo : Pitch.Pitch
    , voiceThree : Pitch.Pitch
    , voiceFour : Pitch.Pitch
    }


{-| A low interval limit is the lowest pitch at which the character of an interval cannot be heard clearly. I have heard this explained in terms of the [harmonic series](https://en.wikipedia.org/wiki/Harmonic_series_%28music%29), but it seems to be taught as more of a [guideline for arrangers](https://www.berklee.edu/core/glossary.html#:~:text=Low%20Interval%20Limit%20%2D%20The%20lowest,within%20a%20normal%20harmonic%20context.) than a physical absolute.
-}
isWithinLowIntervalLimits : Voicing -> Bool
isWithinLowIntervalLimits theVoicing =
    FourPart.violatesLowIntervalLimits theVoicing
        |> not


{-| Get the chord being voiced:

    chord myVoicing == Chord.majorSixNine PitchClass.c

-}
chord : Voicing -> Chord.Chord
chord theVoicing =
    Voicing.chord theVoicing


{-| Get the interval from the lowest to the highest pitch in the voicing:

     span myVoicing == Interval.minorSeventh

-}
span : Voicing -> Interval.Interval
span theVoicing =
    Voicing.range
        { getTopVoice = FourPart.getVoiceOne
        , getBottomVoice = FourPart.getVoiceFour
        }
        theVoicing


{-| Get the midpoint between the highest and lowest voices in semitones:

    center myVoicing
        == Pitch.semitones Pitch.fSharp3

-}
center : Voicing -> Int
center theVoicing =
    FourPart.semitoneCenter theVoicing


{-| Compare voicings by how far their centers are from a goal pitch:

    myVoicingList
        |> List.sortWith (centerOrder Pitch.g4)

Useful for finding voicings that are centered around a particular pitch.

-}
centerOrder : Pitch.Pitch -> (Voicing -> Voicing -> Order)
centerOrder goal =
    Voicing.semitoneCenterOrder (Pitch.semitones goal) voiceFour voiceOne


{-| Get the first (highest) pitch of the voicing:

    voiceOne myVoicing == Pitch.d5

-}
voiceOne : Voicing -> Pitch.Pitch
voiceOne theVoicing =
    FourPart.getVoiceOne theVoicing


{-| Get the second pitch of the voicing:

    voiceTwo myVoicing == Pitch.a4

-}
voiceTwo : Voicing -> Pitch.Pitch
voiceTwo theVoicing =
    FourPart.getVoiceTwo theVoicing


{-| Get the third pitch of the voicing:

    voiceThree myVoicing == Pitch.g4

-}
voiceThree : Voicing -> Pitch.Pitch
voiceThree theVoicing =
    FourPart.getVoiceThree theVoicing


{-| Get the fourth (lowest) pitch of the voicing:

    voiceFour myVoicing == Pitch.e4

-}
voiceFour : Voicing -> Pitch.Pitch
voiceFour theVoicing =
    FourPart.getVoiceFour theVoicing


{-| Sort by multiple ordering functions:

    sortWeighted
        [ ( totalSemitoneDistanceOrder previousVoicing, 10.0 )
        , ( contraryMotionOrder previousVoicing, 5.0 )
        , ( commonTonesOrder previousVoicing, 2.0 )
        ]
        voicingList

-}
sortWeighted :
    List ( Voicing -> Voicing -> Order, Float )
    -> List Voicing
    -> List Voicing
sortWeighted weightedSortFns listToSort =
    FourPart.sortWeighted weightedSortFns listToSort


{-| Combine multiple ordering functions:

    orderWeighted
        [ ( totalSemitoneDistanceOrder previousVoicing, 10.0 )
        , ( contraryMotionOrder previousVoicing, 5.0 )
        , ( commonTonesOrder previousVoicing, 2.0 )
        ]
        |> List.orderWith

-}
orderWeighted :
    List ( Voicing -> Voicing -> Order, Float )
    -> (Voicing -> Voicing -> Order)
orderWeighted weightedSortFns =
    FourPart.orderWeighted weightedSortFns


{-| Create a voicing from pitches and a chord:

    voicing
        { voiceOne = Pitch.c5
        , voiceTwo = Pitch.a4
        , voiceThree = Pitch.g4
        , voiceFour = Pitch.e4
        }
        (Chord.majorSix PitchClass.c)
        == Ok Voicing ...

If the pitches given do not match the chord, this function will return `Err` with the erroneous pitch classes:

    voicing
        { voiceOne = Pitch.c5
        , voiceTwo = Pitch.d4
        , voiceThree = Pitch.g4
        , voiceFour = Pitch.e4
        }
        (Chord.majorSix PitchClass.c)
        == Err [ PitchClass.d ]

-}
voicing : Pitches -> Chord.Chord -> Result (List PitchClass.PitchClass) Voicing
voicing pitches theChord =
    FourPart.voicing pitches theChord


{-| Find out whether a voicing has a specific pitch in the first voice:

     containsPitchInVoiceOne Pitch.d5 myVoicing == True

-}
containsPitchInVoiceOne : Pitch.Pitch -> Voicing -> Bool
containsPitchInVoiceOne pitch theVoicing =
    Voicing.containsPitchInVoice pitch FourPart.getVoiceOne theVoicing


{-| Find out whether a voicing has a specific pitch in the second voice:

     containsPitchInVoiceTwo Pitch.a4 myVoicing == True

-}
containsPitchInVoiceTwo : Pitch.Pitch -> Voicing -> Bool
containsPitchInVoiceTwo pitch theVoicing =
    Voicing.containsPitchInVoice pitch FourPart.getVoiceTwo theVoicing


{-| Find out whether a voicing has a specific pitch in the third voice:

     containsPitchInVoiceThree Pitch.g4 myVoicing == True

-}
containsPitchInVoiceThree : Pitch.Pitch -> Voicing -> Bool
containsPitchInVoiceThree pitch theVoicing =
    Voicing.containsPitchInVoice pitch FourPart.getVoiceThree theVoicing


{-| Find out whether a voicing has a specific pitch in the fourth voice:

     containsPitchInVoiceFour Pitch.e4 myVoicing == True

-}
containsPitchInVoiceFour : Pitch.Pitch -> Voicing -> Bool
containsPitchInVoiceFour pitch theVoicing =
    Voicing.containsPitchInVoice pitch FourPart.getVoiceFour theVoicing


{-| Get all pitches in common between two voicings:

    commonTones bFlatVoicing bDimVoicing
        == [ Pitch.d4
           , Pitch.f4
           ]

-}
commonTones : Voicing -> Voicing -> List Pitch.Pitch
commonTones a b =
    Voicing.commonTones FourPart.allVoices a b


{-| Find out whether the first and fourth voices move in opposite directions (known as [contrary motion](https://en.wikipedia.org/wiki/Contrapuntal_motion#Contrary_motion)).
-}
usesContraryMotion : Voicing -> Voicing -> Bool
usesContraryMotion a b =
    Voicing.usesContraryMotion FourPart.getVoiceFour FourPart.getVoiceOne a b


{-| Find out whether any two moving voices maintain a perfect fifth interval between them. Identifying [parallel fifths and octaves](https://en.wikipedia.org/wiki/Consecutive_fifths) is important in the study of [counterpoint](https://en.wikipedia.org/wiki/Counterpoint).
-}
containsParallelFifths : Voicing -> Voicing -> Bool
containsParallelFifths a b =
    FourPart.containsParallelFifths a b


{-| Find out whether any two moving voices maintain a perfect octave interval between them.
-}
containsParallelOctaves : Voicing -> Voicing -> Bool
containsParallelOctaves a b =
    Voicing.containsParallelOctaves Voicing.root FourPart.allFactors a b


{-| Given two voicings, find out how far in semitones all voices move.
-}
totalSemitoneDistance : Voicing -> Voicing -> Int
totalSemitoneDistance a b =
    Voicing.totalSemitoneDistance FourPart.allVoices a b


{-| Given two voicings, find out how far in semitones the first (top) voice moves.
-}
semitoneDistanceVoiceOne : Voicing -> Voicing -> Int
semitoneDistanceVoiceOne a b =
    Voicing.semitoneDistance (FourPart.getVoiceOne a) (FourPart.getVoiceOne b)


{-| Given two voicings, find out how far in semitones the second voice moves.
-}
semitoneDistanceVoiceTwo : Voicing -> Voicing -> Int
semitoneDistanceVoiceTwo a b =
    Voicing.semitoneDistance (FourPart.getVoiceTwo a) (FourPart.getVoiceTwo b)


{-| Given two voicings, find out how far in semitones the third voice moves.
-}
semitoneDistanceVoiceThree : Voicing -> Voicing -> Int
semitoneDistanceVoiceThree a b =
    Voicing.semitoneDistance (FourPart.getVoiceThree a) (FourPart.getVoiceThree b)


{-| Given two voicings, find out how far in semitones the fourth (bottom) voice moves.
-}
semitoneDistanceVoiceFour : Voicing -> Voicing -> Int
semitoneDistanceVoiceFour a b =
    Voicing.semitoneDistance (FourPart.getVoiceFour a) (FourPart.getVoiceFour b)


{-| Compare voicings by how many pitches they have in common with a previous voicing:

    myVoicingList
        |> List.sortWith (commonTonesOrder previousVoicing)

-}
commonTonesOrder : Voicing -> (Voicing -> Voicing -> Order)
commonTonesOrder from =
    Voicing.compareByCommonTones FourPart.allVoices from


{-| Compare voicings by whether they use contrary motion from a previous voicing:

    myVoicingList
        |> List.sortWith (contraryMotionOrder previousVoicing)

"Contrary motion" here is in respect to the top and bottom voices.

-}
contraryMotionOrder : Voicing -> (Voicing -> Voicing -> Order)
contraryMotionOrder from =
    Voicing.compareByContraryMotion FourPart.getVoiceFour FourPart.getVoiceOne from


{-| Compare voicings by absolute difference in semitones from a previous voicing:

    myVoicingList
        |> List.sortWith (totalSemitoneDistanceOrder previousVoicing)

-}
totalSemitoneDistanceOrder : Voicing -> (Voicing -> Voicing -> Order)
totalSemitoneDistanceOrder from =
    Voicing.compareByTotalSemitoneDistance FourPart.allVoices from


{-| Compare voicings by absolute difference in semitones in voice one:

    myVoicingList
        |> List.sortWith (semitoneDistanceVoiceOneOrder previousVoicing)

-}
semitoneDistanceVoiceOneOrder : Voicing -> (Voicing -> Voicing -> Order)
semitoneDistanceVoiceOneOrder from =
    Voicing.compareByVoiceSemitoneDistance FourPart.getVoiceOne from


{-| Compare voicings by absolute difference in semitones in voice two:

    myVoicingList
        |> List.sortWith (semitoneDistanceVoiceTwoOrder previousVoicing)

-}
semitoneDistanceVoiceTwoOrder : Voicing -> (Voicing -> Voicing -> Order)
semitoneDistanceVoiceTwoOrder from =
    Voicing.compareByVoiceSemitoneDistance FourPart.getVoiceTwo from


{-| Compare voicings by absolute difference in semitones in voice three:

    myVoicingList
        |> List.sortWith (semitoneDistanceVoiceThreeOrder previousVoicing)

-}
semitoneDistanceVoiceThreeOrder : Voicing -> (Voicing -> Voicing -> Order)
semitoneDistanceVoiceThreeOrder from =
    Voicing.compareByVoiceSemitoneDistance FourPart.getVoiceThree from


{-| Compare voicings by absolute difference in semitones in voice four:

    myVoicingList
        |> List.sortWith (semitoneDistanceVoiceFourOrder previousVoicing)

-}
semitoneDistanceVoiceFourOrder : Voicing -> (Voicing -> Voicing -> Order)
semitoneDistanceVoiceFourOrder from =
    Voicing.compareByVoiceSemitoneDistance FourPart.getVoiceFour from


{-| Get all pitches contained in a voicing, as a stringified list:

    toPitchList myVoicing
        == "D5, A4, G4, E4"

-}
toString : Voicing -> String
toString theVoicing =
    theVoicing
        |> toPitchList
        |> List.map Pitch.toString
        |> String.join ", "


{-| Get all pitches contained in a voicing:

    toPitches myVoicing
        == { voiceOne = Pitch.d5
           , voiceTwo = Pitch.a4
           , voiceThree = Pitch.g4
           , voiceFour = Pitch.e4
           }

-}
toPitches : Voicing -> Pitches
toPitches theVoicing =
    FourPart.toPitches theVoicing


{-| Get all pitches contained in a voicing, as a `List`:

    toPitchList myVoicing
        == [ Pitch.d5
           , Pitch.a4
           , Pitch.g4
           , Pitch.e4
           ]

-}
toPitchList : Voicing -> List Pitch.Pitch
toPitchList theVoicing =
    FourPart.toPitches theVoicing
        |> (\v ->
                [ v.voiceOne, v.voiceTwo, v.voiceThree, v.voiceFour ]
           )


{-| Get all intervals between each pitch in a voicing:

    toIntervals myVoicing
        == { fourToOne = Interval.majorSixth
           , fourToTwo = Interval.majorSecond
           , fourToThree = Interval.majorThird
           , threeToOne = Interval.perfectFifth
           , threeToTwo = Interval.majorSecond
           , twoToOne = Interval.perfectFourth
           }

-}
toIntervals : Voicing -> Intervals
toIntervals theVoicing =
    Voicing.voicingClass theVoicing
        |> FourPart.allIntervals


{-| Get all intervals between each pitch in a voicing as a `List`:

    toIntervalList myVoicing
        == [ Interval.majorSecond
           , Interval.majorSecond
           , Interval.majorThird
           , Interval.perfectFourth
           , Interval.perfectFifth
           , Interval.majorSixth
           ]

-}
toIntervalList : Voicing -> List Interval.Interval
toIntervalList theVoicing =
    toIntervals theVoicing
        |> (\i ->
                [ i.fourToOne
                , i.fourToTwo
                , i.threeToOne
                , i.fourToThree
                , i.threeToTwo
                , i.twoToOne
                ]
           )
        |> List.sortBy Interval.semitones


{-| -}
type alias Intervals =
    { fourToOne : Interval.Interval
    , fourToTwo : Interval.Interval
    , fourToThree : Interval.Interval
    , threeToOne : Interval.Interval
    , threeToTwo : Interval.Interval
    , twoToOne : Interval.Interval
    }


{-| A basic textbook method for voicing a chord in root position:

    Chord.voiceFourParts
        { voiceOne = Range.sopranoVoice
        , voiceTwo = Range.altoVoice
        , voiceThree = Range.tenorVoice
        , voiceFour = Range.bassVoice
        }
        [ basic ]
        (Chord.majorSeventh PitchClass.c)
        |> List.map toString
        == [ "B4, G4, E4, C4"
           , -- 2 others...
           ]

-}
basic : VoicingMethod
basic =
    FourPartBasic.basic


{-| Voice a chord using the "four-way close" method:

    Chord.voiceFourParts
        { voiceOne = Range.sopranoVoice
        , voiceTwo = Range.altoVoice
        , voiceThree = Range.tenorVoice
        , voiceFour = Range.bassVoice
        }
        [ close ]
        (Chord.majorSeventh PitchClass.c)
        |> List.map toString
        == [ "C4, B3, G3, E3"
           , -- 39 others...
           ]

This method voices all four notes "closely" within the span of an octave.

-}
close : VoicingMethod
close =
    FourPartJazz.close


{-| Voice a chord using the "four-way drop-2" method:

    Chord.voiceFourParts
        { voiceOne = Range.sopranoVoice
        , voiceTwo = Range.altoVoice
        , voiceThree = Range.tenorVoice
        , voiceFour = Range.bassVoice
        }
        [ drop2 ]
        (Chord.majorSeventh PitchClass.c)
        |> List.map toString
        == [ "C4, G3, E3, B2"
           , -- 55 others...
           ]

This method is the same as four-way close, but with the second pitch from the top dropped by an octave for a wider, semi-open sound.

-}
drop2 : VoicingMethod
drop2 =
    FourPartJazz.drop2


{-| Voice a chord using the "four-way drop-2-and-4" method:

    Chord.voiceFourParts
        { voiceOne = Range.sopranoVoice
        , voiceTwo = Range.altoVoice
        , voiceThree = Range.tenorVoice
        , voiceFour = Range.bassVoice
        }
        [ drop2and4 ]
        (Chord.majorSeventh PitchClass.c)
        |> List.map toString
        == [ "G4, C4, E3, B2"
           , -- 55 others...
           ]

A double drop voicing, with the second and fourth pitches from the top dropped for a wide, open sound.

-}
drop2and4 : VoicingMethod
drop2and4 =
    FourPartJazz.drop2and4


{-| Voice a chord using the "four-way drop-3" method:

    Chord.voiceFourParts
        { voiceOne = Range.sopranoVoice
        , voiceTwo = Range.altoVoice
        , voiceThree = Range.tenorVoice
        , voiceFour = Range.bassVoice
        }
        [ drop3 ]
        (Chord.majorSeventh PitchClass.c)
        |> List.map toString
        == [ "C4, G3, E3, B2"
           , -- 55 others...
           ]

Same as drop-2, but with the third pitch from the top dropped instead of the second.

-}
drop3 : VoicingMethod
drop3 =
    FourPartJazz.drop3


{-| Voice a chord using the "four-way spread" method:

    Chord.voiceFourParts
        { voiceOne = Range.sopranoVoice
        , voiceTwo = Range.altoVoice
        , voiceThree = Range.tenorVoice
        , voiceFour = Range.bassVoice
        }
        [ spread ]
        (Chord.majorSeventh PitchClass.c)
        |> List.map toString
        == [ "C4, B3, E3, G2"
           , -- 22 others...
           ]

Another open voicing method, with the root of the chord on the bottom for a dramatic effect.

-}
spread : VoicingMethod
spread =
    FourPartJazz.spread


{-| Voice a chord in "root position":

    Chord.voiceFourParts
        { voiceOne = Range.sopranoVoice
        , voiceTwo = Range.altoVoice
        , voiceThree = Range.tenorVoice
        , voiceFour = Range.bassVoice
        }
        [ rootPosition ]
        (Chord.major PitchClass.c)
        |> List.map toString
        == [ "G4, E4, G3, C3"
           , -- 30 others...
           ]

A root position voicing is any voicing where the root is in the lowest voice.

-}
rootPosition : VoicingMethod
rootPosition =
    FourPartClassical.rootPosition


{-| Voice a chord in "first inversion":

    Chord.voiceFourParts
        { voiceOne = Range.sopranoVoice
        , voiceTwo = Range.altoVoice
        , voiceThree = Range.tenorVoice
        , voiceFour = Range.bassVoice
        }
        [ firstInversion ]
        (Chord.major PitchClass.c)
        |> List.map toString
        == [ "C6, C5, G4, E3"
           , -- 32 others...
           ]

A first inversion voicing is any voicing where the third of the chord is in the lowest voice.

-}
firstInversion : VoicingMethod
firstInversion =
    FourPartClassical.firstInversion


{-| Voice a chord in "second inversion":

    Chord.voiceFourParts
        { voiceOne = Range.sopranoVoice
        , voiceTwo = Range.altoVoice
        , voiceThree = Range.tenorVoice
        , voiceFour = Range.bassVoice
        }
        [ secondInversion ]
        (Chord.major PitchClass.c)
        |> List.map toString
        == [ "E4, G3, C3, G2"
           , -- 32 others...
           ]

A second inversion voicing is any voicing where the fifth of the chord is in the lowest voice.

-}
secondInversion : VoicingMethod
secondInversion =
    FourPartClassical.secondInversion


{-| Voice a chord in "third inversion":

    Chord.voiceFourParts
        { voiceOne = Range.sopranoVoice
        , voiceTwo = Range.altoVoice
        , voiceThree = Range.tenorVoice
        , voiceFour = Range.bassVoice
        }
        [ thirdInversion ]
        (Chord.dominantSeventh PitchClass.c)
        |> List.map toString
        == [ "E4, C4, C3, B♭2"
           , -- 32 others...
           ]

A third inversion voicing is any voicing where the seventh of the chord is in the lowest voice.

Note: this will return an empty list when used with triad chord types like major and minor, because a seventh must be included.

-}
thirdInversion : VoicingMethod
thirdInversion =
    FourPartClassical.thirdInversion
