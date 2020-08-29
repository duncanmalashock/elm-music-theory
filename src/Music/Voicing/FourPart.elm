module Music.Voicing.FourPart exposing
    ( Voicing, VoicingMethod, Pitches
    , chord, span
    , voiceOne, voiceTwo, voiceThree, voiceFour
    , containsPitchInVoiceOne, containsPitchInVoiceTwo, containsPitchInVoiceThree, containsPitchInVoiceFour
    , commonTones, usesContraryMotion, containsParallelFifths, containsParallelOctaves, totalSemitoneDistances
    , commonTonesOrder, contraryMotionOrder, totalSemitoneDistancesOrder
    , toPitches, toPitchList, toString
    , Intervals, toIntervals, toIntervalList
    , close, drop2, drop3, drop2and4, spread
    , rootPosition, firstInversion, secondInversion, thirdInversion
    )

{-|

@docs Voicing, VoicingMethod, Pitches


# Helpers

@docs chord, span


# Voices

@docs voiceOne, voiceTwo, voiceThree, voiceFour
@docs containsPitchInVoiceOne, containsPitchInVoiceTwo, containsPitchInVoiceThree, containsPitchInVoiceFour


# Comparing voicings

@docs commonTones, usesContraryMotion, containsParallelFifths, containsParallelOctaves, totalSemitoneDistances


# Ordering voicings

@docs commonTonesOrder, contraryMotionOrder, totalSemitoneDistancesOrder


# Conversion

@docs toPitches, toPitchList, toString


## Intervals

@docs Intervals, toIntervals, toIntervalList


# Voicing methods


## Jazz

These methods have been used to harmonize brass and saxophone sections since at least the 1930s.

Note: jazz voicing methods can involve substitutions of related chords with the same function, so don't be surprised when you see voicings that include a pitch class that isn't strictly in the chord you specified.

These methods were adapted from [Jazz Arranging Techniques](http://lindsayjazz.com/jazz-arranging-techniques/) by Gary Lindsay.

@docs close, drop2, drop3, drop2and4, spread


## Classical

Note: these classical methods were developed before jazz harmony, and so chord extensions and added tones will not be included.

@docs rootPosition, firstInversion, secondInversion, thirdInversion


# Custom voicing methods

TK

-}

import Music.Internal.Chord as Chord
import Music.Internal.Interval as Interval
import Music.Internal.Pitch as Pitch
import Music.Internal.Voicing as Voicing
import Music.Internal.Voicing.FourPart as FourPart
import Music.Internal.Voicing.FourPart.Classical as FourPartClassical
import Music.Internal.Voicing.FourPart.Jazz as FourPartJazz


{-| -}
type alias Voicing =
    FourPart.Voicing


{-| -}
type alias VoicingMethod =
    --TODO: make this opaque and create helper functions for constructing it
    { ranges :
        { voiceOne : Pitch.Range
        , voiceTwo : Pitch.Range
        , voiceThree : Pitch.Range
        , voiceFour : Pitch.Range
        }
    , chord : Chord.Chord
    }
    -> List Voicing


{-| The pitches contained in a voicing.
-}
type alias Pitches =
    { voiceOne : Pitch.Pitch
    , voiceTwo : Pitch.Pitch
    , voiceThree : Pitch.Pitch
    , voiceFour : Pitch.Pitch
    }


{-| -}
chord : Voicing -> Chord.Chord
chord voicing =
    Voicing.chord voicing


{-| -}
span : Voicing -> Interval.Interval
span voicing =
    Voicing.range
        { getTopVoice = FourPart.getVoiceOne
        , getBottomVoice = FourPart.getVoiceFour
        }
        voicing


{-| -}
voiceOne : Voicing -> Pitch.Pitch
voiceOne voicing =
    FourPart.getVoiceOne voicing


{-| -}
voiceTwo : Voicing -> Pitch.Pitch
voiceTwo voicing =
    FourPart.getVoiceTwo voicing


{-| -}
voiceThree : Voicing -> Pitch.Pitch
voiceThree voicing =
    FourPart.getVoiceThree voicing


{-| -}
voiceFour : Voicing -> Pitch.Pitch
voiceFour voicing =
    FourPart.getVoiceFour voicing


{-| -}
containsPitchInVoiceOne : Pitch.Pitch -> Voicing -> Bool
containsPitchInVoiceOne pitch voicing =
    Voicing.containsPitchInVoice pitch FourPart.getVoiceOne voicing


{-| -}
containsPitchInVoiceTwo : Pitch.Pitch -> Voicing -> Bool
containsPitchInVoiceTwo pitch voicing =
    Voicing.containsPitchInVoice pitch FourPart.getVoiceTwo voicing


{-| -}
containsPitchInVoiceThree : Pitch.Pitch -> Voicing -> Bool
containsPitchInVoiceThree pitch voicing =
    Voicing.containsPitchInVoice pitch FourPart.getVoiceThree voicing


{-| -}
containsPitchInVoiceFour : Pitch.Pitch -> Voicing -> Bool
containsPitchInVoiceFour pitch voicing =
    Voicing.containsPitchInVoice pitch FourPart.getVoiceFour voicing


{-| -}
commonTones : Voicing -> Voicing -> List Pitch.Pitch
commonTones a b =
    Voicing.commonTones FourPart.allVoices a b


{-| -}
usesContraryMotion : Voicing -> Voicing -> Bool
usesContraryMotion a b =
    Voicing.usesContraryMotion FourPart.getVoiceFour FourPart.getVoiceOne a b


{-| -}
containsParallelFifths : Voicing -> Voicing -> Bool
containsParallelFifths a b =
    Voicing.containsParallelFifths Voicing.root FourPart.allFactors a b


{-| -}
containsParallelOctaves : Voicing -> Voicing -> Bool
containsParallelOctaves a b =
    Voicing.containsParallelOctaves Voicing.root FourPart.allFactors a b


{-| -}
totalSemitoneDistances : Voicing -> Voicing -> Int
totalSemitoneDistances a b =
    Voicing.totalSemitoneDistance FourPart.allVoices a b


{-| -}
commonTonesOrder : Voicing -> (Voicing -> Voicing -> Order)
commonTonesOrder from =
    Voicing.compareByCommonTones FourPart.allVoices from


{-| -}
contraryMotionOrder : Voicing -> (Voicing -> Voicing -> Order)
contraryMotionOrder from =
    Voicing.compareByContraryMotion FourPart.getVoiceFour FourPart.getVoiceOne from


{-| -}
totalSemitoneDistancesOrder : Voicing -> (Voicing -> Voicing -> Order)
totalSemitoneDistancesOrder from =
    Voicing.compareByTotalSemitoneDistance FourPart.allVoices from


{-| -}
toString : Voicing -> String
toString voicing =
    voicing
        |> toPitchList
        |> List.map Pitch.toString
        |> String.join ", "


{-| -}
toPitches : Voicing -> Pitches
toPitches voicing =
    FourPart.toPitches voicing


{-| -}
toPitchList : Voicing -> List Pitch.Pitch
toPitchList voicing =
    FourPart.toPitches voicing
        |> (\v ->
                [ v.voiceOne, v.voiceTwo, v.voiceThree, v.voiceFour ]
           )


{-| -}
toIntervals : Voicing -> Intervals
toIntervals voicing =
    Voicing.voicingClass voicing
        |> FourPart.allIntervals


{-| -}
toIntervalList : Voicing -> List Interval.Interval
toIntervalList voicing =
    toIntervals voicing
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
    , threeToOne : Interval.Interval
    , fourToThree : Interval.Interval
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
