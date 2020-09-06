module Music.Internal.ChordType exposing
    ( AvailableTensions
    , CategorizedFactors
    , ChordType
    , all
    , augmented
    , augmentedDominantSeventh
    , availableTensions
    , categorizeFactors
    , classify
    , custom
    , diminished
    , diminishedSeventh
    , diminishedSeventhElevenFlatThirteen
    , dominantEleventh
    , dominantNinth
    , dominantNinthFlatThirteen
    , dominantNinthSharpEleven
    , dominantNinthSharpElevenFlatThirteen
    , dominantSeventh
    , dominantSeventhFlatFive
    , dominantSeventhFlatNine
    , dominantSeventhFlatNineFlatThirteen
    , dominantSeventhFlatNineSharpEleven
    , dominantSeventhFlatNineSharpNine
    , dominantSeventhFlatNineSharpNineFlatThirteen
    , dominantSeventhFlatThirteen
    , dominantSeventhSharpEleven
    , dominantSeventhSharpElevenFlatThirteen
    , dominantSeventhSharpNine
    , dominantSeventhSharpNineFlatThirteen
    , dominantSeventhSharpNineSharpEleven
    , dominantSeventhSus4
    , dominantThirteenth
    , dominantThirteenthFlatNine
    , dominantThirteenthFlatNineSharpEleven
    , dominantThirteenthFlatNineSharpNine
    , dominantThirteenthSharpEleven
    , dominantThirteenthSharpNine
    , dominantThirteenthSharpNineFlatNine
    , dominantThirteenthSharpNineSharpEleven
    , halfDiminished
    , includes
    , isDiminished
    , isDominant
    , isMajor
    , major
    , majorAddNine
    , majorNinth
    , majorSeventh
    , majorSeventhSharpEleven
    , majorSix
    , majorSixNine
    , minor
    , minorAddNine
    , minorEleventh
    , minorMajorSeventh
    , minorNinth
    , minorSeventh
    , minorSix
    , minorSixNine
    , sus2
    , sus4
    , symbol
    , toIntervals
    , withDiminishedSeventh
    , withEleventh
    , withFifth
    , withFlatFifth
    , withFlatNinth
    , withFlatThirteenth
    , withMajorSeventh
    , withMajorThird
    , withMinorSeventh
    , withMinorThird
    , withNinth
    , withSharpEleventh
    , withSharpFifth
    , withSharpNinth
    , withSixth
    , withSuspendedFourth
    , withSuspendedSecond
    , withThirteenth
    )

import Music.Chord.Classification as Classification exposing (..)
import Music.Internal.Interval as Interval exposing (Interval)


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
    , augmentedDominantSeventh
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


isMajor : ChordType -> Bool
isMajor theChordType =
    includes Interval.majorThird theChordType
        && (not <| includes Interval.minorSeventh theChordType)


isDiminished : ChordType -> Bool
isDiminished theChordType =
    includesAll
        [ Interval.minorThird
        , Interval.diminishedFifth
        ]
        theChordType


isSeventh : ChordType -> Bool
isSeventh theChordType =
    includesAny
        [ Interval.majorSeventh
        , Interval.minorSeventh
        , Interval.diminishedSeventh
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


includesAny : List Interval.Interval -> ChordType -> Bool
includesAny intervals theChordType =
    List.any
        (\interval ->
            includes interval theChordType
        )
        intervals


pickFirst : List Interval.Interval -> ChordType -> Maybe Interval
pickFirst choices theChordType =
    choices
        |> List.filter
            (\factor ->
                includes factor theChordType
            )
        |> List.head


type alias CategorizedFactors =
    { third : Interval
    , fifth : Interval
    , sixthOrSeventh : Maybe Interval
    , ninth : List Interval
    , eleventh : Maybe Interval
    , thirteenth : Maybe Interval
    }


categorizeFactors : ChordType -> Maybe CategorizedFactors
categorizeFactors theChordType =
    let
        maybeThird =
            theChordType
                |> pickFirst
                    [ Interval.majorThird
                    , Interval.minorThird
                    , Interval.majorSecond
                    , Interval.perfectFourth
                    ]

        maybeFifth =
            theChordType
                |> pickFirst
                    [ Interval.perfectFifth
                    , Interval.augmentedFifth
                    , Interval.diminishedFifth
                    ]
    in
    Maybe.map2
        (\third fifth ->
            { third = third
            , fifth = fifth
            , sixthOrSeventh =
                theChordType
                    |> pickFirst
                        [ Interval.majorSixth
                        , Interval.majorSeventh
                        , Interval.minorSeventh
                        , Interval.diminishedSeventh
                        ]
            , ninth =
                [ Interval.majorNinth
                , Interval.minorNinth
                , Interval.augmentedNinth
                ]
                    |> List.filter (\interval -> includes interval theChordType)
            , eleventh =
                theChordType
                    |> pickFirst
                        [ Interval.perfectEleventh
                        , Interval.augmentedEleventh
                        ]
            , thirteenth =
                theChordType
                    |> pickFirst
                        [ Interval.majorThirteenth
                        , Interval.minorThirteenth
                        ]
            }
        )
        maybeThird
        maybeFifth


classify : ChordType -> Classification
classify theChordType =
    let
        triad =
            if includesAll [ Interval.majorThird, Interval.perfectFifth ] theChordType then
                Just MajorTriad

            else if includesAll [ Interval.minorThird, Interval.perfectFifth ] theChordType then
                Just MinorTriad

            else if includesAll [ Interval.majorThird, Interval.augmentedFifth ] theChordType then
                Just AugmentedTriad

            else if includesAll [ Interval.minorThird, Interval.diminishedFifth ] theChordType then
                Just DiminishedTriad

            else if includesAll [ Interval.majorSecond, Interval.perfectFifth ] theChordType then
                Just Sus2Triad

            else if includesAll [ Interval.perfectFourth, Interval.perfectFifth ] theChordType then
                Just Sus4Triad

            else
                Nothing

        sixthOrSeventh =
            if includes Interval.majorSixth theChordType then
                Just Sixth

            else if includes Interval.majorSeventh theChordType then
                Just MajorSeventh

            else if includes Interval.minorSeventh theChordType then
                Just MinorSeventh

            else if includes Interval.diminishedSeventh theChordType then
                Just DiminishedSeventh

            else
                Nothing

        unalteredExtension =
            if includes Interval.majorThirteenth theChordType then
                Just Thirteenth

            else if includes Interval.perfectEleventh theChordType then
                Just Eleventh

            else if includes Interval.majorNinth theChordType then
                Just Ninth

            else
                Nothing

        alteredExtensions =
            List.filterMap
                (\( interval, alteration ) ->
                    if includes interval theChordType then
                        Just alteration

                    else
                        Nothing
                )
                [ ( Interval.minorNinth, MinorNinth )
                , ( Interval.augmentedNinth, AugmentedNinth )
                , ( Interval.augmentedEleventh, AugmentedEleventh )
                , ( Interval.minorThirteenth, MinorThirteenth )
                ]
    in
    case triad of
        Just theTriad ->
            Classification theTriad sixthOrSeventh unalteredExtension alteredExtensions

        Nothing ->
            Unclassifiable (toIntervals theChordType)


sixOrSixNineSymbol : ChordType -> String
sixOrSixNineSymbol theChordType =
    if includesAll [ Interval.majorSixth, Interval.majorNinth ] theChordType then
        "6/9"

    else if includes Interval.majorSixth theChordType then
        "6"

    else if includes Interval.majorNinth theChordType then
        "(add9)"

    else
        ""


unknownChordSymbol : String
unknownChordSymbol =
    "?"


highestUnalteredExtensionSymbol : Maybe UnalteredExtension -> String
highestUnalteredExtensionSymbol maybeUnalteredExtension =
    case maybeUnalteredExtension of
        Just Thirteenth ->
            "13"

        Just Eleventh ->
            "11"

        Just Ninth ->
            "9"

        Nothing ->
            "7"


alterationsSymbols : List SpecialCaseAlterations -> List AlteredExtension -> String
alterationsSymbols specialCaseAlterations alterations =
    (List.map specialCaseAlterationsToSymbol specialCaseAlterations
        ++ List.map alteredExtensionToSymbol alterations
    )
        |> (\symbols ->
                if List.isEmpty symbols then
                    ""

                else
                    "(" ++ String.join "," symbols ++ ")"
           )


type SpecialCaseAlterations
    = SharpFive
    | FlatFive
    | AddNine


specialCaseAlterationsToSymbol : SpecialCaseAlterations -> String
specialCaseAlterationsToSymbol alteration =
    case alteration of
        FlatFive ->
            "♭5"

        SharpFive ->
            "♯5"

        AddNine ->
            "add9"


alteredExtensionToSymbol : AlteredExtension -> String
alteredExtensionToSymbol alteration =
    case alteration of
        MinorNinth ->
            "♭9"

        AugmentedNinth ->
            "♯9"

        AugmentedEleventh ->
            "♯11"

        MinorThirteenth ->
            "♭13"


symbol : ChordType -> String
symbol theChordType =
    case classify theChordType of
        Classification triad maybeSixthOrSeventh maybeUnalteredExtension alteredExtensions ->
            case triad of
                MajorTriad ->
                    case maybeSixthOrSeventh of
                        Just Sixth ->
                            sixOrSixNineSymbol theChordType
                                ++ alterationsSymbols [] alteredExtensions

                        Just MajorSeventh ->
                            "M" ++ highestUnalteredExtensionSymbol maybeUnalteredExtension ++ alterationsSymbols [] alteredExtensions

                        Just MinorSeventh ->
                            highestUnalteredExtensionSymbol maybeUnalteredExtension ++ alterationsSymbols [] alteredExtensions

                        Just DiminishedSeventh ->
                            unknownChordSymbol

                        Nothing ->
                            sixOrSixNineSymbol theChordType ++ alterationsSymbols [] alteredExtensions

                AugmentedTriad ->
                    case maybeSixthOrSeventh of
                        Just Sixth ->
                            sixOrSixNineSymbol theChordType ++ alterationsSymbols [ SharpFive ] alteredExtensions

                        Just MajorSeventh ->
                            "M" ++ highestUnalteredExtensionSymbol maybeUnalteredExtension ++ alterationsSymbols [ SharpFive ] alteredExtensions

                        Just MinorSeventh ->
                            highestUnalteredExtensionSymbol maybeUnalteredExtension ++ alterationsSymbols [ SharpFive ] alteredExtensions

                        Just DiminishedSeventh ->
                            unknownChordSymbol

                        Nothing ->
                            "aug"

                MinorTriad ->
                    case maybeSixthOrSeventh of
                        Just Sixth ->
                            "m"
                                ++ sixOrSixNineSymbol theChordType
                                ++ alterationsSymbols [] alteredExtensions

                        Just MajorSeventh ->
                            "m/M" ++ highestUnalteredExtensionSymbol maybeUnalteredExtension ++ alterationsSymbols [] alteredExtensions

                        Just MinorSeventh ->
                            "m" ++ highestUnalteredExtensionSymbol maybeUnalteredExtension ++ alterationsSymbols [] alteredExtensions

                        Just DiminishedSeventh ->
                            unknownChordSymbol

                        Nothing ->
                            "m" ++ sixOrSixNineSymbol theChordType ++ alterationsSymbols [] alteredExtensions

                DiminishedTriad ->
                    case maybeSixthOrSeventh of
                        Just Sixth ->
                            unknownChordSymbol

                        Just MajorSeventh ->
                            "m/M" ++ highestUnalteredExtensionSymbol maybeUnalteredExtension ++ alterationsSymbols [ FlatFive ] alteredExtensions

                        Just MinorSeventh ->
                            "ø" ++ highestUnalteredExtensionSymbol maybeUnalteredExtension ++ alterationsSymbols [] alteredExtensions

                        Just DiminishedSeventh ->
                            "o7"

                        Nothing ->
                            "dim" ++ alterationsSymbols [] alteredExtensions

                Sus2Triad ->
                    case maybeSixthOrSeventh of
                        Just Sixth ->
                            sixOrSixNineSymbol theChordType ++ "sus2" ++ alterationsSymbols [] alteredExtensions

                        Just MajorSeventh ->
                            "M" ++ highestUnalteredExtensionSymbol maybeUnalteredExtension ++ "sus2" ++ alterationsSymbols [] alteredExtensions

                        Just MinorSeventh ->
                            highestUnalteredExtensionSymbol maybeUnalteredExtension ++ "sus2" ++ alterationsSymbols [] alteredExtensions

                        Just DiminishedSeventh ->
                            unknownChordSymbol

                        Nothing ->
                            "sus2" ++ alterationsSymbols [] alteredExtensions

                Sus4Triad ->
                    case maybeSixthOrSeventh of
                        Just Sixth ->
                            sixOrSixNineSymbol theChordType ++ "sus4" ++ alterationsSymbols [] alteredExtensions

                        Just MajorSeventh ->
                            "M" ++ highestUnalteredExtensionSymbol maybeUnalteredExtension ++ "sus4" ++ alterationsSymbols [] alteredExtensions

                        Just MinorSeventh ->
                            highestUnalteredExtensionSymbol maybeUnalteredExtension ++ "sus4" ++ alterationsSymbols [] alteredExtensions

                        Just DiminishedSeventh ->
                            unknownChordSymbol

                        Nothing ->
                            "sus4" ++ alterationsSymbols [] alteredExtensions

        Unclassifiable list ->
            unknownChordSymbol



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


dominantSeventh : ChordType
dominantSeventh =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh


augmentedDominantSeventh : ChordType
augmentedDominantSeventh =
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


majorNinth : ChordType
majorNinth =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMajorSeventh
        |> withNinth


minorNinth : ChordType
minorNinth =
    chordType
        |> withMinorThird
        |> withFifth
        |> withMinorSeventh
        |> withNinth


dominantNinth : ChordType
dominantNinth =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withNinth


minorEleventh : ChordType
minorEleventh =
    chordType
        |> withMinorThird
        |> withFifth
        |> withMinorSeventh
        |> withNinth
        |> withEleventh


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


dominantSeventhFlatNine : ChordType
dominantSeventhFlatNine =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withFlatNinth


dominantSeventhSharpNine : ChordType
dominantSeventhSharpNine =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withSharpNinth


dominantSeventhFlatNineSharpNine : ChordType
dominantSeventhFlatNineSharpNine =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withFlatNinth
        |> withSharpNinth


dominantSeventhFlatNineSharpEleven : ChordType
dominantSeventhFlatNineSharpEleven =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withFlatNinth
        |> withSharpEleventh


dominantSeventhSharpNineSharpEleven : ChordType
dominantSeventhSharpNineSharpEleven =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withSharpNinth
        |> withSharpEleventh


dominantSeventhSharpEleven : ChordType
dominantSeventhSharpEleven =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withSharpEleventh


dominantSeventhFlatNineFlatThirteen : ChordType
dominantSeventhFlatNineFlatThirteen =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withFlatNinth
        |> withFlatThirteenth


dominantSeventhSharpNineFlatThirteen : ChordType
dominantSeventhSharpNineFlatThirteen =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withSharpNinth
        |> withFlatThirteenth


dominantSeventhSharpElevenFlatThirteen : ChordType
dominantSeventhSharpElevenFlatThirteen =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withSharpEleventh
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


dominantSeventhFlatThirteen : ChordType
dominantSeventhFlatThirteen =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withFlatThirteenth



-- Dominant ninth chords, altered


dominantNinthSharpEleven : ChordType
dominantNinthSharpEleven =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withNinth
        |> withSharpEleventh


dominantNinthFlatThirteen : ChordType
dominantNinthFlatThirteen =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withNinth
        |> withFlatThirteenth


dominantNinthSharpElevenFlatThirteen : ChordType
dominantNinthSharpElevenFlatThirteen =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withNinth
        |> withSharpEleventh
        |> withFlatThirteenth



-- Dominant thirteenth chords, altered


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


dominantThirteenthFlatNineSharpNine : ChordType
dominantThirteenthFlatNineSharpNine =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withFlatNinth
        |> withSharpNinth
        |> withThirteenth


dominantThirteenthFlatNineSharpEleven : ChordType
dominantThirteenthFlatNineSharpEleven =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withFlatNinth
        |> withSharpEleventh
        |> withThirteenth


dominantThirteenthSharpNineSharpEleven : ChordType
dominantThirteenthSharpNineSharpEleven =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withSharpNinth
        |> withSharpEleventh
        |> withThirteenth


dominantThirteenthSharpEleven : ChordType
dominantThirteenthSharpEleven =
    chordType
        |> withMajorThird
        |> withFifth
        |> withMinorSeventh
        |> withNinth
        |> withSharpEleventh
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
        |> remove Interval.majorThirteenth
        |> add Interval.minorThirteenth


custom : ChordType
custom =
    chordType


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



-- Jazz analysis


type VoiceCategory
    = Root
    | Third
    | Fifth
    | Seventh


type JazzChordQuality
    = Major6
    | Major7
    | Minor6
    | Minor7
    | HalfDiminished
    | Dominant7
    | Dominant7Sus
    | Diminished7


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


availableTensions : ChordType -> Maybe AvailableTensions
availableTensions theChordType =
    let
        maybeQuality =
            determineJazzChordQuality theChordType

        maybeRoot =
            Maybe.andThen (chordToneForClass theChordType Root) maybeQuality

        maybeThird =
            Maybe.andThen (chordToneForClass theChordType Third) maybeQuality

        maybeFifth =
            Maybe.andThen (chordToneForClass theChordType Fifth) maybeQuality

        maybeSeventh =
            Maybe.andThen (chordToneForClass theChordType Seventh) maybeQuality

        chordToneWithSubstitutes :
            VoiceCategory
            -> Interval.Interval
            -> JazzChordQuality
            ->
                { true : Interval.Interval
                , substitutes : List Interval.Interval
                }
        chordToneWithSubstitutes voiceCategory chordTone chordQuality =
            { true = chordTone
            , substitutes =
                availableTensionsForChordQuality theChordType voiceCategory chordQuality
                    |> List.filter ((==) chordTone >> not)
            }

        toAvailables :
            JazzChordQuality
            -> Interval.Interval
            -> Interval.Interval
            -> Interval.Interval
            -> Interval.Interval
            -> AvailableTensions
        toAvailables chordQuality root third fifth seventh =
            { root = chordToneWithSubstitutes Root root chordQuality
            , third = chordToneWithSubstitutes Third third chordQuality
            , fifth = chordToneWithSubstitutes Fifth fifth chordQuality
            , seventh = chordToneWithSubstitutes Seventh seventh chordQuality
            }
    in
    case maybeQuality of
        Nothing ->
            Nothing

        Just quality ->
            Maybe.map4
                (toAvailables quality)
                maybeRoot
                maybeThird
                maybeFifth
                maybeSeventh


availableTensionsForChordQuality :
    ChordType
    -> VoiceCategory
    -> JazzChordQuality
    -> List Interval.Interval
availableTensionsForChordQuality theChordType voiceCategory jazzChordQuality =
    let
        chordIntervals =
            toIntervals theChordType

        chordTypeIncludesAll intervals =
            List.map
                (\interval -> List.member interval chordIntervals)
                intervals
                |> List.all identity
    in
    case jazzChordQuality of
        Major6 ->
            case voiceCategory of
                Root ->
                    [ Interval.perfectUnison
                    , Interval.majorNinth
                    ]

                Third ->
                    [ Interval.majorThird
                    ]

                Fifth ->
                    [ Interval.perfectFifth
                    , Interval.augmentedEleventh
                    ]

                Seventh ->
                    [ Interval.majorSixth
                    , Interval.majorSeventh
                    ]

        Major7 ->
            case voiceCategory of
                Root ->
                    [ Interval.perfectUnison
                    , Interval.majorNinth
                    ]

                Third ->
                    [ Interval.majorThird
                    ]

                Fifth ->
                    [ Interval.perfectFifth
                    , Interval.augmentedEleventh
                    ]

                Seventh ->
                    [ Interval.majorSeventh
                    , Interval.majorSixth
                    ]

        Minor6 ->
            case voiceCategory of
                Root ->
                    [ Interval.perfectUnison
                    , Interval.majorNinth
                    ]

                Third ->
                    [ Interval.minorThird
                    ]

                Fifth ->
                    [ Interval.perfectFifth
                    , Interval.perfectEleventh
                    ]

                Seventh ->
                    [ Interval.majorSixth
                    ]

        Minor7 ->
            case voiceCategory of
                Root ->
                    [ Interval.perfectUnison
                    , Interval.majorNinth
                    ]

                Third ->
                    [ Interval.minorThird
                    ]

                Fifth ->
                    [ Interval.perfectFifth
                    , Interval.perfectEleventh
                    ]

                Seventh ->
                    [ Interval.minorSeventh
                    ]

        HalfDiminished ->
            case voiceCategory of
                Root ->
                    [ Interval.perfectUnison
                    , Interval.majorNinth
                    , Interval.perfectEleventh
                    ]

                Third ->
                    [ Interval.minorThird
                    ]

                Fifth ->
                    [ Interval.diminishedFifth
                    , Interval.minorThirteenth
                    ]

                Seventh ->
                    [ Interval.minorSeventh
                    ]

        Dominant7 ->
            case voiceCategory of
                Root ->
                    Interval.perfectUnison
                        :: (if chordTypeIncludesAll [ Interval.majorNinth ] then
                                [ Interval.majorNinth
                                ]

                            else if chordTypeIncludesAll [ Interval.minorNinth ] then
                                [ Interval.minorNinth
                                , Interval.augmentedNinth
                                ]

                            else if chordTypeIncludesAll [ Interval.augmentedNinth ] then
                                [ Interval.augmentedNinth
                                , Interval.minorNinth
                                ]

                            else
                                [ Interval.majorNinth ]
                           )

                Third ->
                    [ Interval.majorThird
                    , Interval.perfectFourth
                    ]

                Fifth ->
                    if chordTypeIncludesAll [ Interval.augmentedEleventh, Interval.minorThirteenth ] then
                        [ Interval.augmentedEleventh
                        , Interval.minorThirteenth
                        ]

                    else if chordTypeIncludesAll [ Interval.augmentedEleventh, Interval.majorThirteenth ] then
                        [ Interval.augmentedEleventh
                        , Interval.majorThirteenth
                        ]

                    else if chordTypeIncludesAll [ Interval.augmentedEleventh ] then
                        [ Interval.augmentedEleventh
                        , Interval.minorThirteenth
                        ]

                    else if chordTypeIncludesAll [ Interval.minorThirteenth ] then
                        [ Interval.minorThirteenth
                        , Interval.augmentedEleventh
                        ]

                    else
                        [ Interval.perfectFifth
                        , Interval.majorThirteenth
                        ]

                Seventh ->
                    [ Interval.minorSeventh
                    ]

        Dominant7Sus ->
            case voiceCategory of
                Root ->
                    [ Interval.perfectUnison
                    , Interval.majorNinth
                    , Interval.minorNinth
                    , Interval.augmentedNinth
                    ]

                Third ->
                    [ Interval.perfectFourth
                    , Interval.majorThird
                    ]

                Fifth ->
                    [ Interval.perfectFifth
                    , Interval.augmentedEleventh
                    , Interval.majorThirteenth
                    , Interval.minorThirteenth
                    ]

                Seventh ->
                    [ Interval.minorSeventh
                    ]

        Diminished7 ->
            case voiceCategory of
                Root ->
                    [ Interval.perfectUnison
                    , Interval.majorNinth
                    ]

                Third ->
                    [ Interval.minorThird
                    , Interval.perfectFourth
                    ]

                Fifth ->
                    [ Interval.diminishedFifth
                    , Interval.minorThirteenth
                    ]

                Seventh ->
                    [ Interval.diminishedSeventh
                    , Interval.diminishedOctave
                    ]


determineJazzChordQuality : ChordType -> Maybe JazzChordQuality
determineJazzChordQuality theChordType =
    let
        intervals =
            toIntervals theChordType

        containsAll intervalsToCheck =
            List.map
                (\interval ->
                    List.member interval intervals
                )
                intervalsToCheck
                |> List.all identity
    in
    if containsAll (minimumIntervalsForJazzChordQuality Major6) then
        Just Major6

    else if containsAll (minimumIntervalsForJazzChordQuality Major7) then
        Just Major7

    else if containsAll (minimumIntervalsForJazzChordQuality Minor6) then
        Just Minor6

    else if containsAll (minimumIntervalsForJazzChordQuality Minor7) then
        Just Minor7

    else if containsAll (minimumIntervalsForJazzChordQuality HalfDiminished) then
        Just HalfDiminished

    else if containsAll (minimumIntervalsForJazzChordQuality Dominant7) then
        Just Dominant7

    else if containsAll (minimumIntervalsForJazzChordQuality Dominant7Sus) then
        Just Dominant7Sus

    else if containsAll (minimumIntervalsForJazzChordQuality Diminished7) then
        Just Diminished7

    else
        Nothing


minimumIntervalsForJazzChordQuality : JazzChordQuality -> List Interval.Interval
minimumIntervalsForJazzChordQuality jazzChordQuality =
    case jazzChordQuality of
        Major6 ->
            [ Interval.majorThird
            , Interval.perfectFifth
            , Interval.majorSixth
            ]

        Major7 ->
            [ Interval.majorThird
            , Interval.perfectFifth
            , Interval.majorSeventh
            ]

        Minor6 ->
            [ Interval.minorThird
            , Interval.perfectFifth
            , Interval.majorSixth
            ]

        Minor7 ->
            [ Interval.minorThird
            , Interval.perfectFifth
            , Interval.minorSeventh
            ]

        HalfDiminished ->
            [ Interval.minorThird
            , Interval.diminishedFifth
            , Interval.minorSeventh
            ]

        Dominant7 ->
            [ Interval.majorThird
            , Interval.minorSeventh
            ]

        Dominant7Sus ->
            [ Interval.perfectFourth
            , Interval.minorSeventh
            ]

        Diminished7 ->
            [ Interval.minorThird
            , Interval.diminishedFifth
            , Interval.diminishedSeventh
            ]


chordToneForClass :
    ChordType
    -> VoiceCategory
    -> JazzChordQuality
    -> Maybe Interval.Interval
chordToneForClass theChordType voiceCategory jazzChordQuality =
    let
        intervals =
            toIntervals theChordType

        takeFirst available =
            List.filter (\item -> List.member item intervals) available
                |> List.head
    in
    takeFirst (availableTensionsForChordQuality theChordType voiceCategory jazzChordQuality)
