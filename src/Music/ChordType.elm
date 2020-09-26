module Music.ChordType exposing
    ( ChordType
    , factors, contains
    , categorizeFactors, CategorizedFactors
    , availableTensions, AvailableTensions
    , toString
    , all
    , triads, sixthAndSeventhChords
    , majorChords, minorChords, dominantChords, alteredDominantChords
    , major, minor, augmented, diminished, sus2, sus4
    , majorSix, majorSixNine, minorSix, minorSixNine, majorAddNine, minorAddNine
    , majorSeventh, majorSeventhSharpEleven, minorSeventh, dominantSeventh, diminishedSeventh, halfDiminishedSeventh, augmentedDominantSeventh, dominantSeventhSus4, minorMajorSeventh
    , majorNinth, minorNinth, dominantNinth, minorEleventh, dominantEleventh, dominantThirteenth
    , dominantSeventhFlatNine, dominantSeventhSharpNine, dominantSeventhFlatNineSharpNine, dominantSeventhFlatNineSharpEleven, dominantSeventhSharpNineSharpEleven, dominantSeventhSharpEleven, dominantSeventhFlatNineFlatThirteen, dominantSeventhSharpNineFlatThirteen, dominantSeventhSharpElevenFlatThirteen, dominantSeventhFlatThirteen
    , dominantNinthSharpEleven, dominantNinthFlatThirteen, dominantNinthSharpElevenFlatThirteen
    , dominantThirteenthFlatNine, dominantThirteenthSharpNine, dominantThirteenthFlatNineSharpNine, dominantThirteenthFlatNineSharpEleven, dominantThirteenthSharpNineSharpEleven, dominantThirteenthSharpEleven
    , custom
    , withMajorThird, withMinorThird, withSuspendedSecond, withSuspendedFourth, withFifth, withSharpFifth, withFlatFifth, withSixth, withMajorSeventh, withMinorSeventh, withDiminishedSeventh, withNinth, withSharpNinth, withFlatNinth, withEleventh, withSharpEleventh, withThirteenth, withFlatThirteenth
    , classify
    )

{-| A [chord type](https://en.wikipedia.org/wiki/Chord_%28music%29#Common_types_of_chords) describes the [factors](https://en.wikipedia.org/wiki/Factor_%28chord%29) contained in a chord, with no specific root pitch class. E.g. a "dominant seventh" chord.

@docs ChordType


# Chord factors

Chord [factors](https://en.wikipedia.org/wiki/Factor_%28chord%29) (such as the "third", "fifth", etc. of a chord) are represented as `Interval`s.

@docs factors, contains


## Categorizing factors

Categorize factors into certain useful data structures. Helpful when creating [custom voicing methods](Music-Voicing-FivePart#custom-voicing-methods).

@docs categorizeFactors, CategorizedFactors
@docs availableTensions, AvailableTensions


# Conversion

@docs toString


# Groups

Lists of commonly-grouped chord types. Note that these are incomplete, and only represent the chord types contained in this module, since there is no exhaustive list of all valid chord types in tonal music.

@docs all
@docs triads, sixthAndSeventhChords
@docs majorChords, minorChords, dominantChords, alteredDominantChords


# Constructors


## Triads

@docs major, minor, augmented, diminished, sus2, sus4


## Added-tone chords

@docs majorSix, majorSixNine, minorSix, minorSixNine, majorAddNine, minorAddNine


## Seventh chords

@docs majorSeventh, majorSeventhSharpEleven, minorSeventh, dominantSeventh, diminishedSeventh, halfDiminishedSeventh, augmentedDominantSeventh, dominantSeventhSus4, minorMajorSeventh


## Chords with extensions

@docs majorNinth, minorNinth, dominantNinth, minorEleventh, dominantEleventh, dominantThirteenth


## Altered dominant chords


### Seventh

@docs dominantSeventhFlatNine, dominantSeventhSharpNine, dominantSeventhFlatNineSharpNine, dominantSeventhFlatNineSharpEleven, dominantSeventhSharpNineSharpEleven, dominantSeventhSharpEleven, dominantSeventhFlatNineFlatThirteen, dominantSeventhSharpNineFlatThirteen, dominantSeventhSharpElevenFlatThirteen, dominantSeventhFlatThirteen


### Ninth

@docs dominantNinthSharpEleven, dominantNinthFlatThirteen, dominantNinthSharpElevenFlatThirteen


### Thirteenth

@docs dominantThirteenthFlatNine, dominantThirteenthSharpNine, dominantThirteenthFlatNineSharpNine, dominantThirteenthFlatNineSharpEleven, dominantThirteenthSharpNineSharpEleven, dominantThirteenthSharpEleven


# Custom chord types

@docs custom
@docs withMajorThird, withMinorThird, withSuspendedSecond, withSuspendedFourth, withFifth, withSharpFifth, withFlatFifth, withSixth, withMajorSeventh, withMinorSeventh, withDiminishedSeventh, withNinth, withSharpNinth, withFlatNinth, withEleventh, withSharpEleventh, withThirteenth, withFlatThirteenth


# Custom chord symbols

@docs classify

-}

import Music.Chord.Classification as Classification exposing (..)
import Music.Internal.ChordType as ChordType
import Music.Internal.Interval as Interval


{-| -}
type alias ChordType =
    ChordType.ChordType


{-| All the chord types listed in this module.
-}
all : List ChordType
all =
    [ major
    , minor
    , augmented
    , diminished
    , sus2
    , sus4
    , majorSix
    , majorSixNine
    , minorSix
    , minorSixNine
    , majorAddNine
    , minorAddNine
    , majorSeventh
    , majorSeventhSharpEleven
    , minorSeventh
    , dominantSeventh
    , diminishedSeventh
    , halfDiminishedSeventh
    , augmentedDominantSeventh
    , dominantSeventhSus4
    , minorMajorSeventh
    , majorNinth
    , minorNinth
    , dominantNinth
    , minorEleventh
    , dominantEleventh
    , dominantThirteenth
    , dominantSeventhFlatNine
    , dominantSeventhSharpNine
    , dominantSeventhFlatNineSharpNine
    , dominantSeventhFlatNineSharpEleven
    , dominantSeventhSharpNineSharpEleven
    , dominantSeventhSharpEleven
    , dominantSeventhFlatNineFlatThirteen
    , dominantSeventhSharpNineFlatThirteen
    , dominantSeventhSharpElevenFlatThirteen
    , dominantSeventhFlatThirteen
    , dominantNinthSharpEleven
    , dominantNinthFlatThirteen
    , dominantNinthSharpElevenFlatThirteen
    , dominantThirteenthFlatNine
    , dominantThirteenthSharpNine
    , dominantThirteenthFlatNineSharpNine
    , dominantThirteenthFlatNineSharpEleven
    , dominantThirteenthSharpNineSharpEleven
    , dominantThirteenthSharpEleven
    ]


{-| All chord types with only a root, third (or suspension), and fifth.
-}
triads : List ChordType
triads =
    [ major
    , minor
    , augmented
    , diminished
    , sus2
    , sus4
    ]


{-| All chord types with a sixth or seventh added to a triad.
-}
sixthAndSeventhChords : List ChordType
sixthAndSeventhChords =
    [ majorSix
    , minorSix
    , majorSeventh
    , minorSeventh
    , dominantSeventh
    , diminishedSeventh
    , halfDiminishedSeventh
    , augmentedDominantSeventh
    , dominantSeventhSus4
    , minorMajorSeventh
    ]


{-| All chord types with a major quality.
-}
majorChords : List ChordType
majorChords =
    [ major
    , majorSix
    , majorSixNine
    , majorAddNine
    , minorAddNine
    , majorSeventh
    , majorSeventhSharpEleven
    , majorNinth
    ]


{-| All chord types with a minor quality.
-}
minorChords : List ChordType
minorChords =
    [ minor
    , minorSix
    , minorSixNine
    , minorAddNine
    , minorSeventh
    , minorMajorSeventh
    , minorNinth
    , minorEleventh
    ]


{-| All chord types with a dominant quality.
-}
dominantChords : List ChordType
dominantChords =
    [ major
    , augmented
    , dominantSeventh
    , augmentedDominantSeventh
    , dominantSeventhSus4
    , dominantNinth
    , dominantEleventh
    , dominantThirteenth
    , dominantSeventhFlatNine
    , dominantSeventhSharpNine
    , dominantSeventhFlatNineSharpNine
    , dominantSeventhFlatNineSharpEleven
    , dominantSeventhSharpNineSharpEleven
    , dominantSeventhSharpEleven
    , dominantSeventhFlatNineFlatThirteen
    , dominantSeventhSharpNineFlatThirteen
    , dominantSeventhSharpElevenFlatThirteen
    , dominantSeventhFlatThirteen
    , dominantNinthSharpEleven
    , dominantNinthFlatThirteen
    , dominantNinthSharpElevenFlatThirteen
    , dominantThirteenthFlatNine
    , dominantThirteenthSharpNine
    , dominantThirteenthFlatNineSharpNine
    , dominantThirteenthFlatNineSharpEleven
    , dominantThirteenthSharpNineSharpEleven
    , dominantThirteenthSharpEleven
    ]


{-| All chord types with a dominant quality that also have [altered extensions](https://en.wikipedia.org/wiki/Altered_chord).
-}
alteredDominantChords : List ChordType
alteredDominantChords =
    [ dominantSeventhFlatNine
    , dominantSeventhSharpNine
    , dominantSeventhFlatNineSharpNine
    , dominantSeventhFlatNineSharpEleven
    , dominantSeventhSharpNineSharpEleven
    , dominantSeventhSharpEleven
    , dominantSeventhFlatNineFlatThirteen
    , dominantSeventhSharpNineFlatThirteen
    , dominantSeventhSharpElevenFlatThirteen
    , dominantSeventhFlatThirteen
    , dominantNinthSharpEleven
    , dominantNinthFlatThirteen
    , dominantNinthSharpElevenFlatThirteen
    , dominantThirteenthFlatNine
    , dominantThirteenthSharpNine
    , dominantThirteenthFlatNineSharpNine
    , dominantThirteenthFlatNineSharpEleven
    , dominantThirteenthSharpNineSharpEleven
    , dominantThirteenthSharpEleven
    ]


{-| Determine whether a chord type contains a given factor:

    contains Interval.diminishedFifth halfDiminishedSeventh == True

-}
contains : Interval.Interval -> ChordType.ChordType -> Bool
contains interval theChordType =
    ChordType.includes interval theChordType


{-| Get the factors contained in a chord type:

    factors minor
        == [ Interval.perfectUnison
           , Interval.minorThird
           , Interval.perfectFifth
           ]

-}
factors : ChordType -> List Interval.Interval
factors chordType =
    ChordType.toIntervals chordType


{-| Categorize the factors in a chord type:

    categorizeFactors minorSixth
        == { third = Interval.minorThird
           , fifth = Interval.perfectFifth
           , sixthOrSeventh = Just Interval.majorSixth
           , ninth = []
           , eleventh = Nothing
           , thirteenth = Nothing
           }

-}
categorizeFactors : ChordType -> Maybe CategorizedFactors
categorizeFactors chordType =
    ChordType.categorizeFactors chordType


{-| -}
type alias CategorizedFactors =
    { third : Interval.Interval
    , fifth : Interval.Interval
    , sixthOrSeventh : Maybe Interval.Interval
    , ninth : List Interval.Interval
    , eleventh : Maybe Interval.Interval
    , thirteenth : Maybe Interval.Interval
    }


{-| Get the chord symbol for a chord type:

    toString majorSeventh == "M7"

Want more control over chord symbols? See the [classify](Music-ChordType#classify) function.

-}
toString : ChordType -> String
toString chordType =
    ChordType.toString chordType


{-| Get a chord type's classification:

    classify dominantNinthFlatThirteen
        == Classification MajorTriad (Just MinorSeventh) Ninth [ MinorThirteenth ]

You can read more about how to use the `Classification` type in the [Chord.Classification](Music-Chord-Classification) module.

-}
classify : ChordType -> Classification
classify theChordType =
    ChordType.classify theChordType


{-| -}
major : ChordType
major =
    ChordType.major


{-| -}
minor : ChordType
minor =
    ChordType.minor


{-| -}
augmented : ChordType
augmented =
    ChordType.augmented


{-| -}
diminished : ChordType
diminished =
    ChordType.diminished


{-| -}
sus2 : ChordType
sus2 =
    ChordType.sus2


{-| -}
sus4 : ChordType
sus4 =
    ChordType.sus4


{-| -}
majorSix : ChordType
majorSix =
    ChordType.majorSix


{-| -}
majorSixNine : ChordType
majorSixNine =
    ChordType.majorSixNine


{-| -}
minorSix : ChordType
minorSix =
    ChordType.minorSix


{-| -}
minorSixNine : ChordType
minorSixNine =
    ChordType.minorSixNine


{-| -}
majorAddNine : ChordType
majorAddNine =
    ChordType.majorAddNine


{-| -}
minorAddNine : ChordType
minorAddNine =
    ChordType.minorAddNine


{-| -}
majorSeventh : ChordType
majorSeventh =
    ChordType.majorSeventh


{-| -}
minorSeventh : ChordType
minorSeventh =
    ChordType.minorSeventh


{-| -}
dominantSeventh : ChordType
dominantSeventh =
    ChordType.dominantSeventh


{-| -}
diminishedSeventh : ChordType
diminishedSeventh =
    ChordType.diminishedSeventh


{-| -}
halfDiminishedSeventh : ChordType
halfDiminishedSeventh =
    ChordType.halfDiminished


{-| -}
majorSeventhSharpEleven : ChordType
majorSeventhSharpEleven =
    ChordType.majorSeventhSharpEleven


{-| -}
minorMajorSeventh : ChordType
minorMajorSeventh =
    ChordType.minorMajorSeventh


{-| -}
majorNinth : ChordType
majorNinth =
    ChordType.majorNinth


{-| -}
minorNinth : ChordType
minorNinth =
    ChordType.minorNinth


{-| -}
dominantNinth : ChordType
dominantNinth =
    ChordType.dominantNinth


{-| -}
minorEleventh : ChordType
minorEleventh =
    ChordType.minorEleventh


{-| -}
dominantEleventh : ChordType
dominantEleventh =
    ChordType.dominantEleventh


{-| -}
dominantThirteenth : ChordType
dominantThirteenth =
    ChordType.dominantThirteenth


{-| -}
augmentedDominantSeventh : ChordType
augmentedDominantSeventh =
    ChordType.augmentedDominantSeventh


{-| -}
dominantSeventhSus4 : ChordType
dominantSeventhSus4 =
    ChordType.dominantSeventhSus4


{-| -}
dominantSeventhFlatNine : ChordType
dominantSeventhFlatNine =
    ChordType.dominantSeventhFlatNine


{-| -}
dominantSeventhSharpNine : ChordType
dominantSeventhSharpNine =
    ChordType.dominantSeventhSharpNine


{-| -}
dominantSeventhFlatNineSharpNine : ChordType
dominantSeventhFlatNineSharpNine =
    ChordType.dominantSeventhFlatNineSharpNine


{-| -}
dominantSeventhFlatNineSharpEleven : ChordType
dominantSeventhFlatNineSharpEleven =
    ChordType.dominantSeventhFlatNineSharpEleven


{-| -}
dominantSeventhSharpNineSharpEleven : ChordType
dominantSeventhSharpNineSharpEleven =
    ChordType.dominantSeventhSharpNineSharpEleven


{-| -}
dominantSeventhSharpEleven : ChordType
dominantSeventhSharpEleven =
    ChordType.dominantSeventhSharpEleven


{-| -}
dominantSeventhFlatNineFlatThirteen : ChordType
dominantSeventhFlatNineFlatThirteen =
    ChordType.dominantSeventhFlatNineFlatThirteen


{-| -}
dominantSeventhSharpNineFlatThirteen : ChordType
dominantSeventhSharpNineFlatThirteen =
    ChordType.dominantSeventhSharpNineFlatThirteen


{-| -}
dominantSeventhSharpElevenFlatThirteen : ChordType
dominantSeventhSharpElevenFlatThirteen =
    ChordType.dominantSeventhSharpElevenFlatThirteen


{-| -}
dominantSeventhFlatThirteen : ChordType
dominantSeventhFlatThirteen =
    ChordType.dominantSeventhFlatThirteen


{-| -}
dominantNinthSharpEleven : ChordType
dominantNinthSharpEleven =
    ChordType.dominantNinthSharpEleven


{-| -}
dominantNinthFlatThirteen : ChordType
dominantNinthFlatThirteen =
    ChordType.dominantNinthFlatThirteen


{-| -}
dominantNinthSharpElevenFlatThirteen : ChordType
dominantNinthSharpElevenFlatThirteen =
    ChordType.dominantNinthSharpElevenFlatThirteen


{-| -}
dominantThirteenthFlatNine : ChordType
dominantThirteenthFlatNine =
    ChordType.dominantThirteenthFlatNine


{-| -}
dominantThirteenthSharpNine : ChordType
dominantThirteenthSharpNine =
    ChordType.dominantThirteenthSharpNine


{-| -}
dominantThirteenthFlatNineSharpNine : ChordType
dominantThirteenthFlatNineSharpNine =
    ChordType.dominantThirteenthFlatNineSharpNine


{-| -}
dominantThirteenthFlatNineSharpEleven : ChordType
dominantThirteenthFlatNineSharpEleven =
    ChordType.dominantThirteenthFlatNineSharpEleven


{-| -}
dominantThirteenthSharpNineSharpEleven : ChordType
dominantThirteenthSharpNineSharpEleven =
    ChordType.dominantThirteenthSharpNineSharpEleven


{-| -}
dominantThirteenthSharpEleven : ChordType
dominantThirteenthSharpEleven =
    ChordType.dominantThirteenthSharpEleven



--


{-| Define a custom chord type. Start with `custom` and apply the chord factor functions below:

    custom
        |> withMinorThird
        |> withDiminishedFifth
        |> withMinorSeventh
    -- equivalent to `halfDiminishedSeventh`

Note: because there is no unified model for naming any given chord, your custom chord may not work with the [toString](Music-ChordType#toString) function in this module.

-}
custom : ChordType
custom =
    ChordType.custom


{-| -}
withMajorThird : ChordType -> ChordType
withMajorThird chordType =
    ChordType.withMajorThird chordType


{-| -}
withMinorThird : ChordType -> ChordType
withMinorThird chordType =
    ChordType.withMinorThird chordType


{-| -}
withSuspendedSecond : ChordType -> ChordType
withSuspendedSecond chordType =
    ChordType.withSuspendedSecond chordType


{-| -}
withSuspendedFourth : ChordType -> ChordType
withSuspendedFourth chordType =
    ChordType.withSuspendedFourth chordType


{-| -}
withFifth : ChordType -> ChordType
withFifth chordType =
    ChordType.withFifth chordType


{-| -}
withFlatFifth : ChordType -> ChordType
withFlatFifth chordType =
    ChordType.withFlatFifth chordType


{-| -}
withSharpFifth : ChordType -> ChordType
withSharpFifth chordType =
    ChordType.withSharpFifth chordType


{-| -}
withSixth : ChordType -> ChordType
withSixth chordType =
    ChordType.withSixth chordType


{-| -}
withDiminishedSeventh : ChordType -> ChordType
withDiminishedSeventh chordType =
    ChordType.withDiminishedSeventh chordType


{-| -}
withMinorSeventh : ChordType -> ChordType
withMinorSeventh chordType =
    ChordType.withMinorSeventh chordType


{-| -}
withMajorSeventh : ChordType -> ChordType
withMajorSeventh chordType =
    ChordType.withMajorSeventh chordType


{-| -}
withNinth : ChordType -> ChordType
withNinth chordType =
    ChordType.withNinth chordType


{-| -}
withFlatNinth : ChordType -> ChordType
withFlatNinth chordType =
    ChordType.withFlatNinth chordType


{-| -}
withSharpNinth : ChordType -> ChordType
withSharpNinth chordType =
    ChordType.withSharpNinth chordType


{-| -}
withEleventh : ChordType -> ChordType
withEleventh chordType =
    ChordType.withEleventh chordType


{-| -}
withSharpEleventh : ChordType -> ChordType
withSharpEleventh chordType =
    ChordType.withSharpEleventh chordType


{-| -}
withThirteenth : ChordType -> ChordType
withThirteenth chordType =
    ChordType.withThirteenth chordType


{-| -}
withFlatThirteenth : ChordType -> ChordType
withFlatThirteenth chordType =
    ChordType.withFlatThirteenth chordType


{-| -}
type alias AvailableTensions =
    { root :
        { true : Interval.Interval
        , substitutes : List Interval.Interval
        }
    , third :
        { true : Interval.Interval
        , substitutes : List Interval.Interval
        }
    , fifth :
        { true : Interval.Interval
        , substitutes : List Interval.Interval
        }
    , seventh :
        { true : Interval.Interval
        , substitutes : List Interval.Interval
        }
    }


{-| Organize the factors in a chord type into their jazz-theory category and their possible substitutes, often known referred to as "available tensions".

    availableTensions majorSeventh =
        { root =
            { true = Interval.perfectUnison
            , substitutes = [ Interval.majorSecond ]
            }
        , third =
            { true = Interval.majorThird
            , substitutes = []
            }
        , fifth =
            { true = Interval.perfectFifth
            , substitutes = [ Interval.augmentedEleventh ]
            }
        , seventh =
            { true = Interval.majorSeventh
            , substitutes = [ Interval.majorSixth ]
            }
        }

-}
availableTensions : ChordType -> Maybe AvailableTensions
availableTensions chordType =
    ChordType.availableTensions chordType
