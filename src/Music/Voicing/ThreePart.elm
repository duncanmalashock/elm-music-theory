module Music.Voicing.ThreePart exposing
    ( Voicing
    , voicing
    , chord, span, center
    , voiceOne, voiceTwo, voiceThree
    , containsPitchInVoiceOne, containsPitchInVoiceTwo, containsPitchInVoiceThree
    , commonTones
    , usesContraryMotion, containsParallelFifths, containsParallelOctaves
    , totalSemitoneDistance, semitoneDistanceVoiceOne, semitoneDistanceVoiceTwo, semitoneDistanceVoiceThree
    , isWithinLowIntervalLimits
    , sortWeighted, orderWeighted
    , centerOrder, commonTonesOrder, contraryMotionOrder, totalSemitoneDistanceOrder, semitoneDistanceVoiceOneOrder, semitoneDistanceVoiceTwoOrder, semitoneDistanceVoiceThreeOrder
    , Pitches, toPitches, toPitchList, toString
    , Intervals, toIntervals, toIntervalList
    , basic
    , shell
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

This package's recommended way of creating three-part `Voicing`s is to use the [Chord.voiceThreeParts](Music-Chord#voicing-chords) function along with `VoicingMethod`s like the ones in this module:

    Chord.voiceThreeParts
        { voiceOne = Range.sopranoVoice
        , voiceTwo = Range.altoVoice
        , voiceThree = Range.tenorVoice
        }
        [ basic ]
        (Chord.majorSeventh PitchClass.c)

In this example, we pass the following to `voiceThreeParts`:

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

@docs voiceOne, voiceTwo, voiceThree
@docs containsPitchInVoiceOne, containsPitchInVoiceTwo, containsPitchInVoiceThree


# Comparing voicings


## Common tones

@docs commonTones


## Direction

@docs usesContraryMotion, containsParallelFifths, containsParallelOctaves


## Distance

@docs totalSemitoneDistance, semitoneDistanceVoiceOne, semitoneDistanceVoiceTwo, semitoneDistanceVoiceThree


# Low interval limits

@docs isWithinLowIntervalLimits


# Sorting voicings

@docs sortWeighted, orderWeighted
@docs centerOrder, commonTonesOrder, contraryMotionOrder, totalSemitoneDistanceOrder, semitoneDistanceVoiceOneOrder, semitoneDistanceVoiceTwoOrder, semitoneDistanceVoiceThreeOrder


# Conversion

@docs Pitches, toPitches, toPitchList, toString


## Intervals

@docs Intervals, toIntervals, toIntervalList


# Voicing methods


## Basic

@docs basic


## Jazz

@docs shell


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
import Music.Internal.Voicing.ThreePart as ThreePart
import Music.Internal.Voicing.ThreePart.Basic as ThreePartBasic
import Music.Internal.VoicingClass as VoicingClass
import Music.PitchClass as PitchClass


{-| -}
type alias Voicing =
    ThreePart.Voicing


{-| -}
type alias VoicingMethod =
    ThreePart.VoicingMethod


{-| Combine multiple `VoicingMethod`s together:

    myComboVoicingMethod =
        combineVoicingMethods [ voicingMethodOne, voicingMethodTwo, voicingMethodThree ]

This is useful if you want to define multiple `VoicingMethod`s that act as a group (e.g. inversions or similar methods that use different chord factors).

-}
combineVoicingMethods : List VoicingMethod -> VoicingMethod
combineVoicingMethods voicingMethodsToCombine =
    ThreePart.combineVoicingMethods voicingMethodsToCombine


type alias InstrumentRanges =
    { voiceOne : Pitch.Range
    , voiceTwo : Pitch.Range
    , voiceThree : Pitch.Range
    }


{-| -}
type alias SpacingLimits =
    { twoToOne : Interval.Range
    , threeToTwo : Interval.Range
    }


{-| Begin a custom voicing method:

    method Chord.categorizeFactors
        (\categorized ->
            ...
        )

-}
method :
    (ChordType.ChordType -> Maybe categorized)
    -> (categorized -> List ThreePart.VoicingClass)
    -> VoicingMethod
method categorizeFn buildFromCategorized =
    ThreePart.custom categorizeFn buildFromCategorized


{-| Select a chord factor for use in a voice.
-}
withFactor :
    Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withFactor factor builder =
    ThreePart.withFactor factor builder


{-| Select a factor that has not yet been used.
-}
withUniqueFactor :
    Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withUniqueFactor factor builder =
    ThreePart.withUniqueFactor factor builder


{-| Select a factor from a list of options.
-}
withFactorFrom :
    List Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withFactorFrom options builder =
    ThreePart.withFactorFrom options builder


{-| Select a factor that has not yet been used, from a list of options.
-}
withUniqueFactorFrom :
    List Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withUniqueFactorFrom options builder =
    ThreePart.withUniqueFactorFrom options builder


{-| Select two factors from a list of options.
-}
withTwoFactorsFrom :
    List Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withTwoFactorsFrom options builder =
    ThreePart.withTwoFactorsFrom options builder


{-| Select two factors that have not yet been used, from a list of options.
-}
withUniqueTwoFactorsFrom :
    List Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withUniqueTwoFactorsFrom options builder =
    ThreePart.withUniqueTwoFactorsFrom options builder


{-| Select three factors from a list of options.
-}
withThreeFactorsFrom :
    List Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> Interval.Interval -> Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withThreeFactorsFrom options builder =
    ThreePart.withThreeFactorsFrom options builder


{-| Select two factors that have not yet been used, from a list of options.
-}
withUniqueThreeFactorsFrom :
    List Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> Interval.Interval -> Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withUniqueThreeFactorsFrom options builder =
    ThreePart.withUniqueThreeFactorsFrom options builder


{-| Begin selecting factors for a voicing method.
-}
selectFactors :
    VoicingClass.VoicingClassBuilder
        (Interval.Interval
         -> Interval.Interval
         -> Interval.Interval
         -> ThreePart.VoicingClass
        )
selectFactors =
    ThreePart.selectFactors


{-| Finish selecting factors for a voicing method and place them according to the spacing limits.
-}
placeSelectedFactors :
    SpacingLimits
    -> VoicingClass.VoicingClassBuilder ThreePart.VoicingClass
    -> List ThreePart.VoicingClass
placeSelectedFactors voiceIntervalLimits builder =
    ThreePart.placeSelectedFactors voiceIntervalLimits builder


{-| The pitches contained in a voicing.

These are in order from highest (`voiceOne`) to lowest (`voiceThree`), the way you might read them on a staff.

-}
type alias Pitches =
    { voiceOne : Pitch.Pitch
    , voiceTwo : Pitch.Pitch
    , voiceThree : Pitch.Pitch
    }


{-| A low interval limit is the lowest pitch at which the character of an interval cannot be heard clearly. I have heard this explained in terms of the [harmonic series](https://en.wikipedia.org/wiki/Harmonic_series_%28music%29), but it seems to be taught as more of a [guideline for arrangers](https://www.berklee.edu/core/glossary.html#:~:text=Low%20Interval%20Limit%20%2D%20The%20lowest,within%20a%20normal%20harmonic%20context.) than a physical absolute.
-}
isWithinLowIntervalLimits : Voicing -> Bool
isWithinLowIntervalLimits theVoicing =
    ThreePart.violatesLowIntervalLimits theVoicing
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
        { getTopVoice = ThreePart.getVoiceOne
        , getBottomVoice = ThreePart.getVoiceThree
        }
        theVoicing


{-| Get the midpoint between the highest and lowest voices in semitones:

    center myVoicing
        == Pitch.semitones Pitch.fSharp3

-}
center : Voicing -> Int
center theVoicing =
    ThreePart.semitoneCenter theVoicing


{-| Compare voicings by how far their centers are from a goal pitch:

    myVoicingList
        |> List.sortWith (centerOrder Pitch.g4)

Useful for finding voicings that are centered around a particular pitch.

-}
centerOrder : Pitch.Pitch -> (Voicing -> Voicing -> Order)
centerOrder goal =
    Voicing.semitoneCenterOrder (Pitch.semitones goal) voiceThree voiceOne


{-| Get the first (highest) pitch of the voicing:

    voiceOne myVoicing == Pitch.d5

-}
voiceOne : Voicing -> Pitch.Pitch
voiceOne theVoicing =
    ThreePart.getVoiceOne theVoicing


{-| Get the second pitch of the voicing:

    voiceTwo myVoicing == Pitch.a4

-}
voiceTwo : Voicing -> Pitch.Pitch
voiceTwo theVoicing =
    ThreePart.getVoiceTwo theVoicing


{-| Get the third (lowest) pitch of the voicing:

    voiceThree myVoicing == Pitch.g4

-}
voiceThree : Voicing -> Pitch.Pitch
voiceThree theVoicing =
    ThreePart.getVoiceThree theVoicing


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
    ThreePart.sortWeighted weightedSortFns listToSort


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
    ThreePart.orderWeighted weightedSortFns


{-| Create a voicing from pitches and a chord:

    voicing
        { voiceOne = Pitch.g4
        , voiceTwo = Pitch.e4
        , voiceThree = Pitch.c4
        }
        (Chord.majorSix PitchClass.c)
        == Ok Voicing ...

If the pitches given do not match the chord, this function will return `Err` with the erroneous pitch classes:

    voicing
        { voiceOne = Pitch.g4
        , voiceTwo = Pitch.d4
        , voiceThree = Pitch.c4
        }
        (Chord.majorSix PitchClass.c)
        == Err [ PitchClass.d ]

-}
voicing : Pitches -> Chord.Chord -> Result (List PitchClass.PitchClass) Voicing
voicing pitches theChord =
    ThreePart.voicing pitches theChord


{-| Find out whether a voicing has a specific pitch in the first voice:

     containsPitchInVoiceOne Pitch.d5 myVoicing == True

-}
containsPitchInVoiceOne : Pitch.Pitch -> Voicing -> Bool
containsPitchInVoiceOne pitch theVoicing =
    Voicing.containsPitchInVoice pitch ThreePart.getVoiceOne theVoicing


{-| Find out whether a voicing has a specific pitch in the second voice:

     containsPitchInVoiceTwo Pitch.a4 myVoicing == True

-}
containsPitchInVoiceTwo : Pitch.Pitch -> Voicing -> Bool
containsPitchInVoiceTwo pitch theVoicing =
    Voicing.containsPitchInVoice pitch ThreePart.getVoiceTwo theVoicing


{-| Find out whether a voicing has a specific pitch in the third voice:

     containsPitchInVoiceThree Pitch.g4 myVoicing == True

-}
containsPitchInVoiceThree : Pitch.Pitch -> Voicing -> Bool
containsPitchInVoiceThree pitch theVoicing =
    Voicing.containsPitchInVoice pitch ThreePart.getVoiceThree theVoicing


{-| Get all pitches in common between two voicings:

    commonTones bFlatVoicing bDimVoicing
        == [ Pitch.d4
           , Pitch.f4
           ]

-}
commonTones : Voicing -> Voicing -> List Pitch.Pitch
commonTones a b =
    Voicing.commonTones ThreePart.allVoices a b


{-| Find out whether the first and third voices move in opposite directions (known as [contrary motion](https://en.wikipedia.org/wiki/Contrapuntal_motion#Contrary_motion)).
-}
usesContraryMotion : Voicing -> Voicing -> Bool
usesContraryMotion a b =
    Voicing.usesContraryMotion ThreePart.getVoiceThree ThreePart.getVoiceOne a b


{-| Find out whether any two moving voices maintain a perfect fifth interval between them. Identifying [parallel fifths and octaves](https://en.wikipedia.org/wiki/Consecutive_fifths) is important in the study of [counterpoint](https://en.wikipedia.org/wiki/Counterpoint).
-}
containsParallelFifths : Voicing -> Voicing -> Bool
containsParallelFifths a b =
    ThreePart.containsParallelFifths a b


{-| Find out whether any two moving voices maintain a perfect octave interval between them.
-}
containsParallelOctaves : Voicing -> Voicing -> Bool
containsParallelOctaves a b =
    Voicing.containsParallelOctaves Voicing.root ThreePart.allFactors a b


{-| Given two voicings, find out how far in semitones all voices move.
-}
totalSemitoneDistance : Voicing -> Voicing -> Int
totalSemitoneDistance a b =
    Voicing.totalSemitoneDistance ThreePart.allVoices a b


{-| Given two voicings, find out how far in semitones the first (top) voice moves.
-}
semitoneDistanceVoiceOne : Voicing -> Voicing -> Int
semitoneDistanceVoiceOne a b =
    Voicing.semitoneDistance (ThreePart.getVoiceOne a) (ThreePart.getVoiceOne b)


{-| Given two voicings, find out how far in semitones the second voice moves.
-}
semitoneDistanceVoiceTwo : Voicing -> Voicing -> Int
semitoneDistanceVoiceTwo a b =
    Voicing.semitoneDistance (ThreePart.getVoiceTwo a) (ThreePart.getVoiceTwo b)


{-| Given two voicings, find out how far in semitones the third voice moves.
-}
semitoneDistanceVoiceThree : Voicing -> Voicing -> Int
semitoneDistanceVoiceThree a b =
    Voicing.semitoneDistance (ThreePart.getVoiceThree a) (ThreePart.getVoiceThree b)


{-| Compare voicings by how many pitches they have in common with a previous voicing:

    myVoicingList
        |> List.sortWith (commonTonesOrder previousVoicing)

-}
commonTonesOrder : Voicing -> (Voicing -> Voicing -> Order)
commonTonesOrder from =
    Voicing.compareByCommonTones ThreePart.allVoices from


{-| Compare voicings by whether they use contrary motion from a previous voicing:

    myVoicingList
        |> List.sortWith (contraryMotionOrder previousVoicing)

"Contrary motion" here is in respect to the top and bottom voices.

-}
contraryMotionOrder : Voicing -> (Voicing -> Voicing -> Order)
contraryMotionOrder from =
    Voicing.compareByContraryMotion ThreePart.getVoiceThree ThreePart.getVoiceOne from


{-| Compare voicings by absolute difference in semitones from a previous voicing:

    myVoicingList
        |> List.sortWith (totalSemitoneDistanceOrder previousVoicing)

-}
totalSemitoneDistanceOrder : Voicing -> (Voicing -> Voicing -> Order)
totalSemitoneDistanceOrder from =
    Voicing.compareByTotalSemitoneDistance ThreePart.allVoices from


{-| Compare voicings by absolute difference in semitones in voice one:

    myVoicingList
        |> List.sortWith (semitoneDistanceVoiceOneOrder previousVoicing)

-}
semitoneDistanceVoiceOneOrder : Voicing -> (Voicing -> Voicing -> Order)
semitoneDistanceVoiceOneOrder from =
    Voicing.compareByVoiceSemitoneDistance ThreePart.getVoiceOne from


{-| Compare voicings by absolute difference in semitones in voice two:

    myVoicingList
        |> List.sortWith (semitoneDistanceVoiceTwoOrder previousVoicing)

-}
semitoneDistanceVoiceTwoOrder : Voicing -> (Voicing -> Voicing -> Order)
semitoneDistanceVoiceTwoOrder from =
    Voicing.compareByVoiceSemitoneDistance ThreePart.getVoiceTwo from


{-| Compare voicings by absolute difference in semitones in voice three:

    myVoicingList
        |> List.sortWith (semitoneDistanceVoiceThreeOrder previousVoicing)

-}
semitoneDistanceVoiceThreeOrder : Voicing -> (Voicing -> Voicing -> Order)
semitoneDistanceVoiceThreeOrder from =
    Voicing.compareByVoiceSemitoneDistance ThreePart.getVoiceThree from


{-| Get all pitches contained in a voicing, as a stringified list:

    toPitchList myVoicing
        == "D5, A4, G4, E4, C4"

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
           }

-}
toPitches : Voicing -> Pitches
toPitches theVoicing =
    ThreePart.toPitches theVoicing


{-| Get all pitches contained in a voicing, as a `List`:

    toPitchList myVoicing
        == [ Pitch.d5
           , Pitch.a4
           , Pitch.g4
           ]

-}
toPitchList : Voicing -> List Pitch.Pitch
toPitchList theVoicing =
    ThreePart.toPitches theVoicing
        |> (\v ->
                [ v.voiceOne, v.voiceTwo, v.voiceThree ]
           )


{-| Get all intervals between each pitch in a voicing:

    toIntervals myVoicing
        == { threeToOne = Interval.majorSixth
           , threeToTwo = Interval.perfectFourth
           , twoToOne = Interval.majorThird
           }

-}
toIntervals : Voicing -> Intervals
toIntervals theVoicing =
    Voicing.voicingClass theVoicing
        |> ThreePart.allIntervals


{-| Get all intervals between each pitch in a voicing as a `List`:

    toIntervalList myVoicing
        == [ Interval.majorThird
           , Interval.perfectFourth
           , Interval.majorSixth
           ]

-}
toIntervalList : Voicing -> List Interval.Interval
toIntervalList theVoicing =
    toIntervals theVoicing
        |> (\i ->
                [ i.threeToOne
                , i.threeToTwo
                , i.twoToOne
                ]
           )
        |> List.sortBy Interval.semitones


{-| -}
type alias Intervals =
    { threeToOne : Interval.Interval
    , threeToTwo : Interval.Interval
    , twoToOne : Interval.Interval
    }


{-| A basic textbook method for voicing a chord in root position:

    Chord.voiceThreeParts
        { voiceOne = Range.sopranoVoice
        , voiceTwo = Range.altoVoice
        , voiceThree = Range.tenorVoice
        }
        [ basic ]
        (Chord.major PitchClass.c)
        |> List.map toString
        == [ "G3, E3, C3"
           , "G4, E4, C4"
           ]

-}
basic : VoicingMethod
basic =
    ThreePartBasic.basic


{-| Voice a chord with the "shell" method:

    Chord.voiceThreeParts
        { voiceOne = Range.sopranoVoice
        , voiceTwo = Range.altoVoice
        , voiceThree = Range.tenorVoice
        }
        [ shell ]
        (Chord.majorSeventh PitchClass.c)
        |> List.map toString
        == [ "E4, B3, C3"
           , -- 3 others...
           ]

Popularized by Bud Powell and Thelonious Monk, "shell" voicings include the essential pitches in jazz chords, and are useful for minimalistic accompaniment.

-}
shell : VoicingMethod
shell =
    ThreePartBasic.shell
