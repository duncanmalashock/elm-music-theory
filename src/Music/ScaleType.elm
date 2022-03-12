module Music.ScaleType exposing
    ( ScaleType
    , mode
    , toList
    , major, minor
    , ionian, dorian, phrygian, lydian, mixolydian, aeolian, locrian
    , melodicMinor, dorianFlat2, lydianAugmented, acoustic, majorMinor, minorLocrian, superlocrian
    , harmonicMinor, locrianNatural6, majorAugmented, lydianDiminished, phrygianDominant, aeolianHarmonic, ultralocrian
    , diminishedWholeToneHalfTone, diminishedHalfToneWholeTone, wholeTone
    , majorPentatonic, minorPentatonic
    , customPentatonic, customHexatonic, customHeptatonic, customOctatonic
    , name
    )

{-| A scale type describes the intervals contained in a [scale](https://en.wikipedia.org/wiki/Scale_%28music%29), with no specific root pitch class. E.g. the "major scale."

@docs ScaleType


# Helpers

@docs mode


# Conversion

@docs toList


# Constructors

@docs major, minor


## Modes of the major scale

@docs ionian, dorian, phrygian, lydian, mixolydian, aeolian, locrian


## Modes of the melodic minor scale

@docs melodicMinor, dorianFlat2, lydianAugmented, acoustic, majorMinor, minorLocrian, superlocrian


## Modes of the harmonic minor scale

@docs harmonicMinor, locrianNatural6, majorAugmented, lydianDiminished, phrygianDominant, aeolianHarmonic, ultralocrian


## Symmetrical scales

@docs diminishedWholeToneHalfTone, diminishedHalfToneWholeTone, wholeTone


## Pentatonic scales

@docs majorPentatonic, minorPentatonic


# Custom scale types

@docs customPentatonic, customHexatonic, customHeptatonic, customOctatonic

-}

import Internal.Interval exposing (Interval)
import Internal.ScaleType as ScaleType


{-| -}
type alias ScaleType =
    ScaleType.ScaleType


{-| Get the intervals contained in the scale type:

    toList major
        == [ Interval.perfectUnison
           , Interval.majorSecond
           , Interval.majorThird
           , Interval.perfectFourth
           , Interval.perfectFifth
           , Interval.majorSixth
           , Interval.majorSeventh
           ]

-}
toList : ScaleType -> List Interval
toList scaleType =
    ScaleType.toList scaleType


{-| Get a [mode](https://en.wikipedia.org/wiki/Mode_%28music%29#Modern_modes) of a scale type by shifting the order of its intervals. For example, the Phrygian mode is the third mode of the major scale:

    mode 3 major == phrygian

Note: like many numbers in music theory, modes are 1-based; `2` means the second mode of the scale type. 0 and negative integers are ignored.

-}
mode : Int -> ScaleType -> ScaleType
mode modeShiftIndex scaleType =
    mode modeShiftIndex scaleType


name : ScaleType -> Maybe String
name scaleType =
    ScaleType.name scaleType


{-| -}
ionian : ScaleType
ionian =
    ScaleType.ionian


{-| Same as `ionian`
-}
major : ScaleType
major =
    ScaleType.major


{-| -}
dorian : ScaleType
dorian =
    ScaleType.dorian


{-| -}
phrygian : ScaleType
phrygian =
    ScaleType.phrygian


{-| -}
lydian : ScaleType
lydian =
    ScaleType.lydian


{-| -}
mixolydian : ScaleType
mixolydian =
    ScaleType.mixolydian


{-| -}
aeolian : ScaleType
aeolian =
    ScaleType.aeolian


{-| Also known as the "natural minor" scale. Same as `aeolian`
-}
minor : ScaleType
minor =
    ScaleType.aeolian


{-| -}
locrian : ScaleType
locrian =
    ScaleType.locrian


{-| -}
melodicMinor : ScaleType
melodicMinor =
    ScaleType.melodicMinor


{-| -}
dorianFlat2 : ScaleType
dorianFlat2 =
    ScaleType.dorianFlat2


{-| -}
lydianAugmented : ScaleType
lydianAugmented =
    ScaleType.lydianAugmented


{-| -}
acoustic : ScaleType
acoustic =
    ScaleType.acoustic


{-| -}
majorMinor : ScaleType
majorMinor =
    ScaleType.majorMinor


{-| -}
minorLocrian : ScaleType
minorLocrian =
    ScaleType.minorLocrian


{-| -}
superlocrian : ScaleType
superlocrian =
    ScaleType.superlocrian


{-| -}
harmonicMinor : ScaleType
harmonicMinor =
    ScaleType.harmonicMinor


{-| -}
locrianNatural6 : ScaleType
locrianNatural6 =
    ScaleType.locrianNatural6


{-| -}
majorAugmented : ScaleType
majorAugmented =
    ScaleType.majorAugmented


{-| -}
lydianDiminished : ScaleType
lydianDiminished =
    ScaleType.lydianDiminished


{-| -}
phrygianDominant : ScaleType
phrygianDominant =
    ScaleType.phrygianDominant


{-| -}
aeolianHarmonic : ScaleType
aeolianHarmonic =
    ScaleType.aeolianHarmonic


{-| -}
ultralocrian : ScaleType
ultralocrian =
    ScaleType.ultralocrian


{-| -}
diminishedWholeToneHalfTone : ScaleType
diminishedWholeToneHalfTone =
    ScaleType.diminishedWholeToneHalfTone


{-| -}
diminishedHalfToneWholeTone : ScaleType
diminishedHalfToneWholeTone =
    ScaleType.diminishedHalfToneWholeTone


{-| -}
wholeTone : ScaleType
wholeTone =
    ScaleType.wholeTone


{-| -}
majorPentatonic : ScaleType
majorPentatonic =
    ScaleType.majorPentatonic


{-| -}
minorPentatonic : ScaleType
minorPentatonic =
    ScaleType.minorPentatonic


{-| Define a custom scale type with 5 degrees:

    customPentatonic
        { rootToSecond = Interval.minorThird
        , rootToThird = Interval.perfectFourth
        , rootToFourth = Interval.perfectFifth
        , rootToFifth = Interval.minorSeventh
        }
        == minorPentatonic

-}
customPentatonic :
    { rootToSecond : Interval
    , rootToThird : Interval
    , rootToFourth : Interval
    , rootToFifth : Interval
    }
    -> ScaleType
customPentatonic intervals =
    ScaleType.customPentatonic intervals


{-| Define a custom scale type with 6 degrees:

    customHexatonic
        { rootToSecond = Interval.majorSecond
        , rootToThird = Interval.majorThird
        , rootToFourth = Interval.augmentedFourth
        , rootToFifth = Interval.minorSixth
        , rootToSixth = Interval.minorSeventh
        }
        == wholeTone

-}
customHexatonic :
    { rootToSecond : Interval
    , rootToThird : Interval
    , rootToFourth : Interval
    , rootToFifth : Interval
    , rootToSixth : Interval
    }
    -> ScaleType
customHexatonic intervals =
    ScaleType.customHexatonic intervals


{-| Define a custom scale type with 7 degrees:

    customHeptatonic
        { rootToSecond = Interval.majorSecond
        , rootToThird = Interval.minorThird
        , rootToFourth = Interval.perfectFourth
        , rootToFifth = Interval.perfectFifth
        , rootToSixth = Interval.majorSixth
        , rootToSeventh = Interval.minorSeventh
        }
        == dorian

-}
customHeptatonic :
    { rootToSecond : Interval
    , rootToThird : Interval
    , rootToFourth : Interval
    , rootToFifth : Interval
    , rootToSixth : Interval
    , rootToSeventh : Interval
    }
    -> ScaleType
customHeptatonic intervals =
    ScaleType.customHeptatonic intervals


{-| Define a custom scale type with 8 degrees:

    customOctatonic
        { rootToSecond = Interval.majorSecond
        , rootToThird = Interval.minorThird
        , rootToFourth = Interval.perfectFourth
        , rootToFifth = Interval.diminishedFifth
        , rootToSixth = Interval.minorSixth
        , rootToSeventh = Interval.majorSixth
        , rootToEighth = Interval.majorSeventh
        }
        == diminishedWholeToneHalfTone

-}
customOctatonic :
    { rootToSecond : Interval
    , rootToThird : Interval
    , rootToFourth : Interval
    , rootToFifth : Interval
    , rootToSixth : Interval
    , rootToSeventh : Interval
    , rootToEighth : Interval
    }
    -> ScaleType
customOctatonic intervals =
    ScaleType.customOctatonic intervals
