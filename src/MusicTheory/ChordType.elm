module MusicTheory.ChordType exposing
    ( ChordType
    , toIntervals
    , major, minor, augmented, diminished, sus2, sus4
    , majorSix, majorSixNine, minorSix, minorSixNine, majorAddNine, minorAddNine
    , majorSeventh, majorSeventhSharpEleven, minorSeventh, dominantSeventh, diminishedSeventh, halfDiminished, dominantSeventhSus4, minorMajorSeventh
    , majorNinth, minorNinth, dominantNinth, minorEleventh, dominantEleventh, dominantThirteenth
    , dominantSeventhSharpFive, dominantSeventhFlatFive, dominantSeventhFlatNine, dominantSeventhFlatNineFlatThirteen, dominantSeventhFlatNineSharpNine, dominantSeventhFlatNineSharpNineFlatThirteen, dominantSeventhSharpNine, dominantSeventhSharpNineFlatThirteen, dominantThirteenthFlatNine, dominantThirteenthSharpNine, dominantThirteenthSharpNineFlatNine
    , diminishedSeventhElevenFlatThirteen
    )

{-| A [chord type](https://en.wikipedia.org/wiki/Chord_%28music%29#Common_types_of_chords) describes the intervals contained in a chord, with no specific root pitch class.

@docs ChordType


# Conversion

@docs toIntervals


# Constructors


## Triads

@docs major, minor, augmented, diminished, sus2, sus4


## Added-tone chords

@docs majorSix, majorSixNine, minorSix, minorSixNine, majorAddNine, minorAddNine


## Seventh chords

@docs majorSeventh, majorSeventhSharpEleven, minorSeventh, dominantSeventh, diminishedSeventh, halfDiminished, dominantSeventhSus4, minorMajorSeventh


## Chords with extensions

@docs majorNinth, minorNinth, dominantNinth, minorEleventh, dominantEleventh, dominantThirteenth


## Altered dominant chords

@docs dominantSeventhSharpFive, dominantSeventhFlatFive, dominantSeventhFlatNine, dominantSeventhFlatNineFlatThirteen, dominantSeventhFlatNineSharpNine, dominantSeventhFlatNineSharpNineFlatThirteen, dominantSeventhSharpNine, dominantSeventhSharpNineFlatThirteen, dominantThirteenthFlatNine, dominantThirteenthSharpNine, dominantThirteenthSharpNineFlatNine


## Altered diminished chords

@docs diminishedSeventhElevenFlatThirteen

-}

import MusicTheory.Internal.ChordType as ChordType
import MusicTheory.Internal.Interval as Interval


{-| -}
type alias ChordType =
    ChordType.ChordType


{-| Get the intervals contained in a chord type:

    toIntervals minor == [ Interval.perfectUnison, Interval.minorThird, Interval.perfectFifth ]

-}
toIntervals : ChordType -> List Interval.Interval
toIntervals chordType =
    ChordType.toIntervals chordType


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
halfDiminished : ChordType
halfDiminished =
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
dominantSeventhSharpFive : ChordType
dominantSeventhSharpFive =
    ChordType.dominantSeventhSharpFive


{-| -}
dominantSeventhFlatFive : ChordType
dominantSeventhFlatFive =
    ChordType.dominantSeventhFlatFive


{-| -}
dominantSeventhFlatNine : ChordType
dominantSeventhFlatNine =
    ChordType.dominantSeventhFlatNine


{-| -}
dominantSeventhFlatNineFlatThirteen : ChordType
dominantSeventhFlatNineFlatThirteen =
    ChordType.dominantSeventhFlatNineFlatThirteen


{-| -}
dominantSeventhFlatNineSharpNine : ChordType
dominantSeventhFlatNineSharpNine =
    ChordType.dominantSeventhFlatNineSharpNine


{-| -}
dominantSeventhFlatNineSharpNineFlatThirteen : ChordType
dominantSeventhFlatNineSharpNineFlatThirteen =
    ChordType.dominantSeventhFlatNineSharpNineFlatThirteen


{-| -}
dominantSeventhSharpNine : ChordType
dominantSeventhSharpNine =
    ChordType.dominantSeventhSharpNine


{-| -}
dominantSeventhSharpNineFlatThirteen : ChordType
dominantSeventhSharpNineFlatThirteen =
    ChordType.dominantSeventhSharpNineFlatThirteen


{-| -}
dominantSeventhSus4 : ChordType
dominantSeventhSus4 =
    ChordType.dominantSeventhSus4


{-| -}
dominantThirteenthFlatNine : ChordType
dominantThirteenthFlatNine =
    ChordType.dominantThirteenthFlatNine


{-| -}
dominantThirteenthSharpNine : ChordType
dominantThirteenthSharpNine =
    ChordType.dominantThirteenthSharpNine


{-| -}
dominantThirteenthSharpNineFlatNine : ChordType
dominantThirteenthSharpNineFlatNine =
    ChordType.dominantThirteenthSharpNineFlatNine


{-| -}
diminishedSeventhElevenFlatThirteen : ChordType
diminishedSeventhElevenFlatThirteen =
    ChordType.diminishedSeventhElevenFlatThirteen
