module Internal.ChordClass exposing
    ( ChordClass
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

import Internal.Interval as Interval exposing (Interval)


type ChordClass
    = ChordClass (List Interval)


toIntervals : ChordClass -> List Interval
toIntervals theChordClass =
    case theChordClass of
        ChordClass chordFactors ->
            chordFactors


all : List ChordClass
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


isDominant : ChordClass -> Bool
isDominant theChordClass =
    includesAll
        [ Interval.majorThird
        , Interval.minorSeventh
        ]
        theChordClass
        || includesAll
            [ Interval.perfectFourth
            , Interval.minorSeventh
            ]
            theChordClass


isDiminished : ChordClass -> Bool
isDiminished theChordClass =
    includesAll
        [ Interval.minorThird
        , Interval.diminishedFifth
        ]
        theChordClass


includes : Interval.Interval -> ChordClass -> Bool
includes theInterval (ChordClass intervals) =
    List.member theInterval intervals


includesAll : List Interval.Interval -> ChordClass -> Bool
includesAll intervals theChordClass =
    List.all
        (\interval ->
            includes interval theChordClass
        )
        intervals



-- Triads


major : ChordClass
major =
    chordClass
        |> withMajorThird
        |> withFifth


minor : ChordClass
minor =
    chordClass
        |> withMinorThird
        |> withFifth


augmented : ChordClass
augmented =
    chordClass
        |> withMajorThird
        |> withSharpFifth


diminished : ChordClass
diminished =
    chordClass
        |> withMinorThird
        |> withFlatFifth


sus2 : ChordClass
sus2 =
    chordClass
        |> withSuspendedSecond
        |> withFifth


sus4 : ChordClass
sus4 =
    chordClass
        |> withSuspendedFourth
        |> withFifth



-- Add9 Chords


majorAddNine : ChordClass
majorAddNine =
    chordClass
        |> withMajorThird
        |> withFifth
        |> withNinth


minorAddNine : ChordClass
minorAddNine =
    chordClass
        |> withMinorThird
        |> withFifth
        |> withNinth



-- Sixth Chords


majorSix : ChordClass
majorSix =
    chordClass
        |> withMajorThird
        |> withFifth
        |> withSixth


minorSix : ChordClass
minorSix =
    chordClass
        |> withMinorThird
        |> withFifth
        |> withSixth


majorSixNine : ChordClass
majorSixNine =
    chordClass
        |> withMajorThird
        |> withFifth
        |> withSixth
        |> withNinth


minorSixNine : ChordClass
minorSixNine =
    chordClass
        |> withMinorThird
        |> withFifth
        |> withSixth
        |> withNinth



-- Seventh Chords


majorSeventh : ChordClass
majorSeventh =
    chordClass
        |> withMajorThird
        |> withFifth
        |> withMajorSeventh


majorSeventhSharpEleven : ChordClass
majorSeventhSharpEleven =
    chordClass
        |> withMajorThird
        |> withFifth
        |> withMajorSeventh
        |> withSharpEleventh


minorSeventh : ChordClass
minorSeventh =
    chordClass
        |> withMinorThird
        |> withFifth
        |> withMinorSeventh


minorNinth : ChordClass
minorNinth =
    chordClass
        |> withMinorThird
        |> withFifth
        |> withMinorSeventh
        |> withNinth


dominantSeventh : ChordClass
dominantSeventh =
    chordClass
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh


dominantSeventhSharpFive : ChordClass
dominantSeventhSharpFive =
    chordClass
        |> withMajorThird
        |> withSharpFifth
        |> withMinorSeventh


dominantSeventhFlatFive : ChordClass
dominantSeventhFlatFive =
    chordClass
        |> withMajorThird
        |> withFlatFifth
        |> withMinorSeventh


dominantSeventhSus4 : ChordClass
dominantSeventhSus4 =
    chordClass
        |> withSuspendedFourth
        |> withFifth
        |> withMinorSeventh


minorMajorSeventh : ChordClass
minorMajorSeventh =
    chordClass
        |> withMinorThird
        |> withFifth
        |> withMajorSeventh


halfDiminished : ChordClass
halfDiminished =
    chordClass
        |> withMinorThird
        |> withFlatFifth
        |> withMinorSeventh


diminishedSeventh : ChordClass
diminishedSeventh =
    chordClass
        |> withMinorThird
        |> withFlatFifth
        |> withDiminishedSeventh


diminishedSeventhElevenFlatThirteen : ChordClass
diminishedSeventhElevenFlatThirteen =
    chordClass
        |> withMinorThird
        |> withFlatFifth
        |> withDiminishedSeventh
        |> withFlatThirteenth



-- Extended Dominants, no altered tones


dominantNinth : ChordClass
dominantNinth =
    chordClass
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withNinth


dominantEleventh : ChordClass
dominantEleventh =
    chordClass
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withNinth
        |> withEleventh


dominantThirteenth : ChordClass
dominantThirteenth =
    chordClass
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withNinth
        |> withEleventh
        |> withThirteenth



-- -- Dominant Seventh Chords, Altered


dominantSeventhSharpNine : ChordClass
dominantSeventhSharpNine =
    chordClass
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withSharpNinth


dominantSeventhFlatNine : ChordClass
dominantSeventhFlatNine =
    chordClass
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withFlatNinth


dominantSeventhFlatNineSharpNine : ChordClass
dominantSeventhFlatNineSharpNine =
    chordClass
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withFlatNinth
        |> withSharpNinth


dominantSeventhSharpNineFlatThirteen : ChordClass
dominantSeventhSharpNineFlatThirteen =
    chordClass
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withSharpNinth
        |> withFlatThirteenth


dominantSeventhFlatNineFlatThirteen : ChordClass
dominantSeventhFlatNineFlatThirteen =
    chordClass
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withFlatNinth
        |> withFlatThirteenth


dominantSeventhFlatNineSharpNineFlatThirteen : ChordClass
dominantSeventhFlatNineSharpNineFlatThirteen =
    chordClass
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withFlatNinth
        |> withSharpNinth
        |> withFlatThirteenth


dominantThirteenthFlatNine : ChordClass
dominantThirteenthFlatNine =
    chordClass
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withFlatNinth
        |> withThirteenth


dominantThirteenthSharpNine : ChordClass
dominantThirteenthSharpNine =
    chordClass
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withSharpNinth
        |> withThirteenth


dominantThirteenthSharpNineFlatNine : ChordClass
dominantThirteenthSharpNineFlatNine =
    chordClass
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withFlatNinth
        |> withSharpNinth
        |> withThirteenth



-- Builder functions for chord factors


withMajorThird : ChordClass -> ChordClass
withMajorThird factors =
    factors
        |> remove Interval.minorThird
        |> remove Interval.majorSecond
        |> remove Interval.perfectFourth
        |> add Interval.majorThird


withMinorThird : ChordClass -> ChordClass
withMinorThird factors =
    factors
        |> remove Interval.majorThird
        |> remove Interval.majorSecond
        |> remove Interval.perfectFourth
        |> add Interval.minorThird


withSuspendedSecond : ChordClass -> ChordClass
withSuspendedSecond factors =
    factors
        |> remove Interval.majorThird
        |> remove Interval.minorThird
        |> remove Interval.perfectFourth
        |> add Interval.majorSecond


withSuspendedFourth : ChordClass -> ChordClass
withSuspendedFourth factors =
    factors
        |> remove Interval.majorThird
        |> remove Interval.minorThird
        |> remove Interval.majorSecond
        |> add Interval.perfectFourth


withFifth : ChordClass -> ChordClass
withFifth factors =
    factors
        |> remove Interval.diminishedFifth
        |> remove Interval.augmentedFifth
        |> add Interval.perfectFifth


withFlatFifth : ChordClass -> ChordClass
withFlatFifth factors =
    factors
        |> remove Interval.perfectFifth
        |> add Interval.diminishedFifth


withSharpFifth : ChordClass -> ChordClass
withSharpFifth factors =
    factors
        |> remove Interval.perfectFifth
        |> add Interval.augmentedFifth


withSixth : ChordClass -> ChordClass
withSixth factors =
    factors
        |> remove Interval.minorThirteenth
        |> remove Interval.majorThirteenth
        |> add Interval.majorSixth


withDiminishedSeventh : ChordClass -> ChordClass
withDiminishedSeventh factors =
    factors
        |> remove Interval.majorSeventh
        |> remove Interval.minorSeventh
        |> remove Interval.majorSixth
        |> add Interval.diminishedSeventh


withMinorSeventh : ChordClass -> ChordClass
withMinorSeventh factors =
    factors
        |> remove Interval.majorSeventh
        |> remove Interval.diminishedSeventh
        |> remove Interval.majorSixth
        |> add Interval.minorSeventh


withMajorSeventh : ChordClass -> ChordClass
withMajorSeventh factors =
    factors
        |> remove Interval.minorSeventh
        |> remove Interval.diminishedSeventh
        |> remove Interval.majorSixth
        |> add Interval.majorSeventh


withNinth : ChordClass -> ChordClass
withNinth factors =
    factors
        |> remove Interval.augmentedNinth
        |> remove Interval.minorNinth
        |> add Interval.majorNinth


withFlatNinth : ChordClass -> ChordClass
withFlatNinth factors =
    factors
        |> remove Interval.majorNinth
        |> add Interval.minorNinth


withSharpNinth : ChordClass -> ChordClass
withSharpNinth factors =
    factors
        |> remove Interval.majorNinth
        |> add Interval.augmentedNinth


withEleventh : ChordClass -> ChordClass
withEleventh factors =
    factors
        |> remove Interval.augmentedEleventh
        |> add Interval.perfectEleventh


withSharpEleventh : ChordClass -> ChordClass
withSharpEleventh factors =
    factors
        |> remove Interval.perfectEleventh
        |> add Interval.augmentedEleventh


withThirteenth : ChordClass -> ChordClass
withThirteenth factors =
    factors
        |> remove Interval.majorSixth
        |> remove Interval.minorThirteenth
        |> add Interval.majorThirteenth


withFlatThirteenth : ChordClass -> ChordClass
withFlatThirteenth factors =
    factors
        |> remove Interval.majorSixth
        |> remove Interval.perfectFifth
        |> remove Interval.majorThirteenth
        |> add Interval.minorThirteenth


chordClass : ChordClass
chordClass =
    ChordClass [ Interval.perfectUnison ]


remove : Interval -> ChordClass -> ChordClass
remove factorToRemove (ChordClass factorList) =
    List.filter ((==) factorToRemove >> not) factorList
        |> ChordClass


add : Interval -> ChordClass -> ChordClass
add factorToAdd (ChordClass factorList) =
    factorToAdd
        :: factorList
        |> ChordClass
