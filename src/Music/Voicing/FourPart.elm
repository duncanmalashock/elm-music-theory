module Music.Voicing.FourPart exposing
    ( Voicing
    , chord, span
    , voicing
    , voiceOne, voiceTwo, voiceThree, voiceFour
    , containsPitchInVoiceOne, containsPitchInVoiceTwo, containsPitchInVoiceThree, containsPitchInVoiceFour
    , sortWeighted
    , commonTones, usesContraryMotion, containsParallelFifths, containsParallelOctaves, totalSemitoneDistances
    , isWithinLowIntervalLimits
    , commonTonesOrder, contraryMotionOrder, totalSemitoneDistancesOrder
    , Pitches, toPitches, toPitchList, toString
    , Intervals, toIntervals, toIntervalList
    , VoicingMethod
    , custom
    , selectFactors, withFactor, withUniqueFactor, withFactorFrom, withUniqueFactorFrom, withTwoFactorsFrom, withUniqueTwoFactorsFrom, withThreeFactorsFrom, withUniqueThreeFactorsFrom
    , placeSelectedFactors, SpacingLimits
    , combineVoicingMethods
    , close, drop2, drop3, drop2and4, spread
    , rootPosition, firstInversion, secondInversion, thirdInversion
    )

{-| A chord voicing is an instance of a chord that can be played or sung.

This module allows for:

  - Getting properties of a voicing like its chord, its interval span, and its voices
  - Comparing voicings to determine common tones, contrary or parallel motion, and voice distance
  - Sorting voicings by various criteria such as voice leading concerns
  - Determining whether a voicing is within low interval limits
  - Using common voicing methods in the classical and jazz idioms
  - Defining custom voicing methods

@docs Voicing


# Helpers

@docs chord, span


# Constructor

@docs voicing


# Voices

@docs voiceOne, voiceTwo, voiceThree, voiceFour
@docs containsPitchInVoiceOne, containsPitchInVoiceTwo, containsPitchInVoiceThree, containsPitchInVoiceFour


# Sorting voicings

@docs sortWeighted


# Comparing voicings

@docs commonTones, usesContraryMotion, containsParallelFifths, containsParallelOctaves, totalSemitoneDistances


# Low interval limits

@docs isWithinLowIntervalLimits


# Ordering voicings

@docs commonTonesOrder, contraryMotionOrder, totalSemitoneDistancesOrder


# Conversion

@docs Pitches, toPitches, toPitchList, toString


## Intervals

@docs Intervals, toIntervals, toIntervalList


# Voicing methods


## Custom voicing methods

@docs VoicingMethod
@docs custom
@docs selectFactors, withFactor, withUniqueFactor, withFactorFrom, withUniqueFactorFrom, withTwoFactorsFrom, withUniqueTwoFactorsFrom, withThreeFactorsFrom, withUniqueThreeFactorsFrom
@docs placeSelectedFactors, SpacingLimits
@docs combineVoicingMethods


## Jazz

These methods have been used to harmonize brass and saxophone sections since at least the 1930s.

Notes:

  - Jazz voicing methods can involve substitutions within families of related chords, so don't be surprised when you see voicings that include a pitch class that isn't strictly in the chord you specified.
  - Not all chords are compatible with these methods; incompatible chord types will return an empty list.

These methods were adapted from [Jazz Arranging Techniques](http://lindsayjazz.com/jazz-arranging-techniques/) by Gary Lindsay.

@docs close, drop2, drop3, drop2and4, spread


## Classical

Note: these classical methods were developed before jazz harmony, and so chord extensions and added tones will not be included.

@docs rootPosition, firstInversion, secondInversion, thirdInversion

-}

import Music.Internal.Chord as Chord
import Music.Internal.ChordType as ChordType
import Music.Internal.Interval as Interval
import Music.Internal.Pitch as Pitch
import Music.Internal.Voicing as Voicing
import Music.Internal.Voicing.FourPart as FourPart
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


{-| -}
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


{-| -}
custom :
    (ChordType.ChordType -> Maybe categorized)
    -> (categorized -> List FourPart.VoicingClass)
    -> VoicingMethod
custom categorizeFn buildFromCategorized =
    FourPart.custom categorizeFn buildFromCategorized


{-| -}
withFactor :
    Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withFactor factor builder =
    FourPart.withFactor factor builder


{-| -}
withUniqueFactor :
    Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withUniqueFactor factor builder =
    FourPart.withUniqueFactor factor builder


{-| -}
withFactorFrom :
    List Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withFactorFrom options builder =
    FourPart.withFactorFrom options builder


{-| -}
withUniqueFactorFrom :
    List Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withUniqueFactorFrom options builder =
    FourPart.withUniqueFactorFrom options builder


{-| -}
withTwoFactorsFrom :
    List Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withTwoFactorsFrom options builder =
    FourPart.withTwoFactorsFrom options builder


{-| -}
withUniqueTwoFactorsFrom :
    List Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withUniqueTwoFactorsFrom options builder =
    FourPart.withUniqueTwoFactorsFrom options builder


{-| -}
withThreeFactorsFrom :
    List Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> Interval.Interval -> Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withThreeFactorsFrom options builder =
    FourPart.withThreeFactorsFrom options builder


{-| -}
withUniqueThreeFactorsFrom :
    List Interval.Interval
    -> VoicingClass.VoicingClassBuilder (Interval.Interval -> Interval.Interval -> Interval.Interval -> a)
    -> VoicingClass.VoicingClassBuilder a
withUniqueThreeFactorsFrom options builder =
    FourPart.withUniqueThreeFactorsFrom options builder


{-| -}
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


{-| -}
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


{-| -}
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


{-| -}
sortWeighted :
    List ( Voicing -> Voicing -> Order, Float )
    -> List Voicing
    -> List Voicing
sortWeighted weightedSortFns listToSort =
    FourPart.sortWeighted weightedSortFns listToSort


{-| -}
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


{-| Find out whether the first and fourth voices move in opposite directions.
-}
usesContraryMotion : Voicing -> Voicing -> Bool
usesContraryMotion a b =
    Voicing.usesContraryMotion FourPart.getVoiceFour FourPart.getVoiceOne a b


{-| Find out whether any two moving voices maintain a perfect fifth interval between them.

Avoiding parallel fifths and octaves was very important in Mozart's time! Checking for instances of them may help you if you want to write in a period style.

-}
containsParallelFifths : Voicing -> Voicing -> Bool
containsParallelFifths a b =
    Voicing.containsParallelFifths Voicing.root FourPart.allFactors a b


{-| Find out whether any two moving voices maintain a perfect octave interval between them.
-}
containsParallelOctaves : Voicing -> Voicing -> Bool
containsParallelOctaves a b =
    Voicing.containsParallelOctaves Voicing.root FourPart.allFactors a b


{-| Find out the absolute difference in semitones, in all voices, between two voicings.
-}
totalSemitoneDistances : Voicing -> Voicing -> Int
totalSemitoneDistances a b =
    Voicing.totalSemitoneDistance FourPart.allVoices a b


{-| Compare voicings by how many pitches they have in common with a previous voicing:

    myVoicingList
        |> List.sortWith (commonTonesOrder previousVoicing)

-}
commonTonesOrder : Voicing -> (Voicing -> Voicing -> Order)
commonTonesOrder from =
    Voicing.compareByCommonTones FourPart.allVoices from


{-| Compare voicings by whether they use contrary motion from a previous voicing:

    myVoicingList
        |> List.sortWith (commonTonesOrder previousVoicing)

"Contrary motion" here is in respect to the top and bottom voices.

-}
contraryMotionOrder : Voicing -> (Voicing -> Voicing -> Order)
contraryMotionOrder from =
    Voicing.compareByContraryMotion FourPart.getVoiceFour FourPart.getVoiceOne from


{-| Compare voicings by absolute difference in semitones from a previous voicing:

    myVoicingList
        |> List.sortWith (commonTonesOrder previousVoicing)

-}
totalSemitoneDistancesOrder : Voicing -> (Voicing -> Voicing -> Order)
totalSemitoneDistancesOrder from =
    Voicing.compareByTotalSemitoneDistance FourPart.allVoices from


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
