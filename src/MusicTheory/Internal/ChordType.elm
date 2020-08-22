module MusicTheory.Internal.ChordType exposing
    ( ChordType
    , all
    , augmented
    , diminished
    , diminishedSeventh
    , diminishedSeventhElevenFlatThirteen
    , dominantEleventh
    , dominantNinth
    , dominantSeventh
    , dominantSeventhFlatFive
    , dominantSeventhFlatNine
    , dominantSeventhFlatNineFlatThirteen
    , dominantSeventhFlatNineSharpNine
    , dominantSeventhFlatNineSharpNineFlatThirteen
    , dominantSeventhSharpFive
    , dominantSeventhSharpNine
    , dominantSeventhSharpNineFlatThirteen
    , dominantSeventhSus4
    , dominantThirteenth
    , dominantThirteenthFlatNine
    , dominantThirteenthSharpNine
    , dominantThirteenthSharpNineFlatNine
    , halfDiminished
    , isDiminished
    , isDominant
    , major
    , majorAddNine
    , majorSeventh
    , majorSeventhSharpEleven
    , majorSix
    , majorSixNine
    , minor
    , minorAddNine
    , minorMajorSeventh
    , minorNinth
    , minorSeventh
    , minorSix
    , minorSixNine
    , sus2
    , sus4
    , toIntervals
    )

import MusicTheory.Internal.Interval as Interval exposing (Interval)


type ChordType
    = ChordType (List Interval)


toIntervals : ChordType -> List Interval
toIntervals theChordType =
    case theChordType of
        ChordType chordFactors ->
            chordFactors


all : List ChordType
all =
    [ augmented
    , diminished
    , diminishedSeventh
    , diminishedSeventhElevenFlatThirteen
    , dominantEleventh
    , dominantNinth
    , dominantSeventh
    , dominantSeventhSharpFive
    , dominantSeventhFlatFive
    , dominantSeventhFlatNine
    , dominantSeventhFlatNineFlatThirteen
    , dominantSeventhFlatNineSharpNine
    , dominantSeventhFlatNineSharpNineFlatThirteen
    , dominantSeventhSharpNine
    , dominantSeventhSharpNineFlatThirteen
    , dominantSeventhSus4
    , dominantThirteenth
    , dominantThirteenthFlatNine
    , dominantThirteenthSharpNine
    , dominantThirteenthSharpNineFlatNine
    , halfDiminished
    , major
    , majorAddNine
    , majorSeventh
    , majorSix
    , majorSixNine
    , minorSeventh
    , minor
    , minorAddNine
    , minorMajorSeventh
    , minorNinth
    , minorSix
    , minorSixNine
    , sus2
    , sus4
    , majorSeventhSharpEleven
    ]


isDominant : ChordType -> Bool
isDominant theChordType =
    includesAll
        [ Interval.majorThird
        , Interval.minorSeventh
        ]
        theChordType
        || includesAll
            [ Interval.perfectFourth
            , Interval.minorSeventh
            ]
            theChordType


isDiminished : ChordType -> Bool
isDiminished theChordType =
    includesAll
        [ Interval.minorThird
        , Interval.diminishedFifth
        ]
        theChordType


includes : Interval.Interval -> ChordType -> Bool
includes theInterval (ChordType intervals) =
    List.member theInterval intervals


includesAll : List Interval.Interval -> ChordType -> Bool
includesAll intervals theChordType =
    List.all
        (\interval ->
            includes interval theChordType
        )
        intervals



-- Triads


major : ChordType
major =
    chordType
        |> withMajorThird
        |> withFifth


minor : ChordType
minor =
    chordType
        |> withMinorThird
        |> withFifth


augmented : ChordType
augmented =
    chordType
        |> withMajorThird
        |> withSharpFifth


diminished : ChordType
diminished =
    chordType
        |> withMinorThird
        |> withFlatFifth


sus2 : ChordType
sus2 =
    chordType
        |> withSuspendedSecond
        |> withFifth


sus4 : ChordType
sus4 =
    chordType
        |> withSuspendedFourth
        |> withFifth



-- Add9 Chords


majorAddNine : ChordType
majorAddNine =
    chordType
        |> withMajorThird
        |> withFifth
        |> withNinth


minorAddNine : ChordType
minorAddNine =
    chordType
        |> withMinorThird
        |> withFifth
        |> withNinth



-- Sixth Chords


majorSix : ChordType
majorSix =
    chordType
        |> withMajorThird
        |> withFifth
        |> withSixth


minorSix : ChordType
minorSix =
    chordType
        |> withMinorThird
        |> withFifth
        |> withSixth


majorSixNine : ChordType
majorSixNine =
    chordType
        |> withMajorThird
        |> withFifth
        |> withSixth
        |> withNinth


minorSixNine : ChordType
minorSixNine =
    chordType
        |> withMinorThird
        |> withFifth
        |> withSixth
        |> withNinth



-- Seventh Chords


majorSeventh : ChordType
majorSeventh =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMajorSeventh


majorSeventhSharpEleven : ChordType
majorSeventhSharpEleven =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMajorSeventh
        |> withSharpEleventh


minorSeventh : ChordType
minorSeventh =
    chordType
        |> withMinorThird
        |> withFifth
        |> withMinorSeventh


minorNinth : ChordType
minorNinth =
    chordType
        |> withMinorThird
        |> withFifth
        |> withMinorSeventh
        |> withNinth


dominantSeventh : ChordType
dominantSeventh =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh


dominantSeventhSharpFive : ChordType
dominantSeventhSharpFive =
    chordType
        |> withMajorThird
        |> withSharpFifth
        |> withMinorSeventh


dominantSeventhFlatFive : ChordType
dominantSeventhFlatFive =
    chordType
        |> withMajorThird
        |> withFlatFifth
        |> withMinorSeventh


dominantSeventhSus4 : ChordType
dominantSeventhSus4 =
    chordType
        |> withSuspendedFourth
        |> withFifth
        |> withMinorSeventh


minorMajorSeventh : ChordType
minorMajorSeventh =
    chordType
        |> withMinorThird
        |> withFifth
        |> withMajorSeventh


halfDiminished : ChordType
halfDiminished =
    chordType
        |> withMinorThird
        |> withFlatFifth
        |> withMinorSeventh


diminishedSeventh : ChordType
diminishedSeventh =
    chordType
        |> withMinorThird
        |> withFlatFifth
        |> withDiminishedSeventh


diminishedSeventhElevenFlatThirteen : ChordType
diminishedSeventhElevenFlatThirteen =
    chordType
        |> withMinorThird
        |> withFlatFifth
        |> withDiminishedSeventh
        |> withFlatThirteenth



-- Extended Dominants, no altered tones


dominantNinth : ChordType
dominantNinth =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withNinth


dominantEleventh : ChordType
dominantEleventh =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withNinth
        |> withEleventh


dominantThirteenth : ChordType
dominantThirteenth =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withNinth
        |> withEleventh
        |> withThirteenth



-- -- Dominant Seventh Chords, Altered


dominantSeventhSharpNine : ChordType
dominantSeventhSharpNine =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withSharpNinth


dominantSeventhFlatNine : ChordType
dominantSeventhFlatNine =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withFlatNinth


dominantSeventhFlatNineSharpNine : ChordType
dominantSeventhFlatNineSharpNine =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withFlatNinth
        |> withSharpNinth


dominantSeventhSharpNineFlatThirteen : ChordType
dominantSeventhSharpNineFlatThirteen =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withSharpNinth
        |> withFlatThirteenth


dominantSeventhFlatNineFlatThirteen : ChordType
dominantSeventhFlatNineFlatThirteen =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withFlatNinth
        |> withFlatThirteenth


dominantSeventhFlatNineSharpNineFlatThirteen : ChordType
dominantSeventhFlatNineSharpNineFlatThirteen =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withFlatNinth
        |> withSharpNinth
        |> withFlatThirteenth


dominantThirteenthFlatNine : ChordType
dominantThirteenthFlatNine =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withFlatNinth
        |> withThirteenth


dominantThirteenthSharpNine : ChordType
dominantThirteenthSharpNine =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withSharpNinth
        |> withThirteenth


dominantThirteenthSharpNineFlatNine : ChordType
dominantThirteenthSharpNineFlatNine =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withFlatNinth
        |> withSharpNinth
        |> withThirteenth



-- Builder functions for chord factors


withMajorThird : ChordType -> ChordType
withMajorThird factors =
    factors
        |> remove Interval.minorThird
        |> remove Interval.majorSecond
        |> remove Interval.perfectFourth
        |> add Interval.majorThird


withMinorThird : ChordType -> ChordType
withMinorThird factors =
    factors
        |> remove Interval.majorThird
        |> remove Interval.majorSecond
        |> remove Interval.perfectFourth
        |> add Interval.minorThird


withSuspendedSecond : ChordType -> ChordType
withSuspendedSecond factors =
    factors
        |> remove Interval.majorThird
        |> remove Interval.minorThird
        |> remove Interval.perfectFourth
        |> add Interval.majorSecond


withSuspendedFourth : ChordType -> ChordType
withSuspendedFourth factors =
    factors
        |> remove Interval.majorThird
        |> remove Interval.minorThird
        |> remove Interval.majorSecond
        |> add Interval.perfectFourth


withFifth : ChordType -> ChordType
withFifth factors =
    factors
        |> remove Interval.diminishedFifth
        |> remove Interval.augmentedFifth
        |> add Interval.perfectFifth


withFlatFifth : ChordType -> ChordType
withFlatFifth factors =
    factors
        |> remove Interval.perfectFifth
        |> add Interval.diminishedFifth


withSharpFifth : ChordType -> ChordType
withSharpFifth factors =
    factors
        |> remove Interval.perfectFifth
        |> add Interval.augmentedFifth


withSixth : ChordType -> ChordType
withSixth factors =
    factors
        |> remove Interval.minorThirteenth
        |> remove Interval.majorThirteenth
        |> add Interval.majorSixth


withDiminishedSeventh : ChordType -> ChordType
withDiminishedSeventh factors =
    factors
        |> remove Interval.majorSeventh
        |> remove Interval.minorSeventh
        |> remove Interval.majorSixth
        |> add Interval.diminishedSeventh


withMinorSeventh : ChordType -> ChordType
withMinorSeventh factors =
    factors
        |> remove Interval.majorSeventh
        |> remove Interval.diminishedSeventh
        |> remove Interval.majorSixth
        |> add Interval.minorSeventh


withMajorSeventh : ChordType -> ChordType
withMajorSeventh factors =
    factors
        |> remove Interval.minorSeventh
        |> remove Interval.diminishedSeventh
        |> remove Interval.majorSixth
        |> add Interval.majorSeventh


withNinth : ChordType -> ChordType
withNinth factors =
    factors
        |> remove Interval.augmentedNinth
        |> remove Interval.minorNinth
        |> add Interval.majorNinth


withFlatNinth : ChordType -> ChordType
withFlatNinth factors =
    factors
        |> remove Interval.majorNinth
        |> add Interval.minorNinth


withSharpNinth : ChordType -> ChordType
withSharpNinth factors =
    factors
        |> remove Interval.majorNinth
        |> add Interval.augmentedNinth


withEleventh : ChordType -> ChordType
withEleventh factors =
    factors
        |> remove Interval.augmentedEleventh
        |> add Interval.perfectEleventh


withSharpEleventh : ChordType -> ChordType
withSharpEleventh factors =
    factors
        |> remove Interval.perfectEleventh
        |> add Interval.augmentedEleventh


withThirteenth : ChordType -> ChordType
withThirteenth factors =
    factors
        |> remove Interval.majorSixth
        |> remove Interval.minorThirteenth
        |> add Interval.majorThirteenth


withFlatThirteenth : ChordType -> ChordType
withFlatThirteenth factors =
    factors
        |> remove Interval.majorSixth
        |> remove Interval.perfectFifth
        |> remove Interval.majorThirteenth
        |> add Interval.minorThirteenth


chordType : ChordType
chordType =
    ChordType [ Interval.perfectUnison ]


remove : Interval -> ChordType -> ChordType
remove factorToRemove (ChordType factorList) =
    List.filter ((==) factorToRemove >> not) factorList
        |> ChordType


add : Interval -> ChordType -> ChordType
add factorToAdd (ChordType factorList) =
    factorToAdd
        :: factorList
        |> ChordType
