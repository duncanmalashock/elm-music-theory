module Internal.ChordType exposing
    ( ChordType
    , toString
    , all
    , major, minor, augmented, diminished, sus2, sus4
    , majorSix, majorSixNine, minorSix, minorSixNine, majorAddNine, minorAddNine
    , majorSeventh, majorSeventhSharpEleven, minorSeventh, dominantSeventh, diminishedSeventh, halfDiminishedSeventh, augmentedDominantSeventh, dominantSeventhSus4, minorMajorSeventh
    , majorNinth, minorNinth, dominantNinth, minorEleventh, dominantEleventh, dominantThirteenth
    , dominantSeventhFlatNine, dominantSeventhSharpNine, dominantSeventhFlatNineSharpNine, dominantSeventhFlatNineSharpEleven, dominantSeventhSharpNineSharpEleven, dominantSeventhSharpEleven, dominantSeventhFlatNineFlatThirteen, dominantSeventhSharpNineFlatThirteen, dominantSeventhSharpElevenFlatThirteen, dominantSeventhFlatThirteen, dominantSeventhFlatFive, dominantSeventhFlatNineSharpNineFlatThirteen, diminishedSeventhElevenFlatThirteen
    , dominantNinthSharpEleven, dominantNinthFlatThirteen, dominantNinthSharpElevenFlatThirteen
    , dominantThirteenthFlatNine, dominantThirteenthSharpNine, dominantThirteenthFlatNineSharpNine, dominantThirteenthFlatNineSharpEleven, dominantThirteenthSharpNineSharpEleven, dominantThirteenthSharpEleven, dominantThirteenthSharpNineFlatNine
    , custom
    , withMajorThird, withMinorThird
    , withSuspendedSecond
    , withSuspendedFourth
    , withFifth, withSharpFifth, withFlatFifth
    , withSixth
    , withMajorSeventh, withMinorSeventh, withDiminishedSeventh
    , withNinth, withSharpNinth, withFlatNinth
    , withEleventh, withSharpEleventh
    , withThirteenth, withFlatThirteenth
    , classify
    , includes, includesAll, includesAny
    , isDiminished, isDominant, isMajor
    , toIntervals
    , Serial, toSerial
    )

{-|

@docs ChordType

@docs toString

@docs all

@docs major, minor, augmented, diminished, sus2, sus4
@docs majorSix, majorSixNine, minorSix, minorSixNine, majorAddNine, minorAddNine
@docs majorSeventh, majorSeventhSharpEleven, minorSeventh, dominantSeventh, diminishedSeventh, halfDiminishedSeventh, augmentedDominantSeventh, dominantSeventhSus4, minorMajorSeventh
@docs majorNinth, minorNinth, dominantNinth, minorEleventh, dominantEleventh, dominantThirteenth
@docs dominantSeventhFlatNine, dominantSeventhSharpNine, dominantSeventhFlatNineSharpNine, dominantSeventhFlatNineSharpEleven, dominantSeventhSharpNineSharpEleven, dominantSeventhSharpEleven, dominantSeventhFlatNineFlatThirteen, dominantSeventhSharpNineFlatThirteen, dominantSeventhSharpElevenFlatThirteen, dominantSeventhFlatThirteen, dominantSeventhFlatFive, dominantSeventhFlatNineSharpNineFlatThirteen, diminishedSeventhElevenFlatThirteen
@docs dominantNinthSharpEleven, dominantNinthFlatThirteen, dominantNinthSharpElevenFlatThirteen
@docs dominantThirteenthFlatNine, dominantThirteenthSharpNine, dominantThirteenthFlatNineSharpNine, dominantThirteenthFlatNineSharpEleven, dominantThirteenthSharpNineSharpEleven, dominantThirteenthSharpEleven, dominantThirteenthSharpNineFlatNine

@docs custom
@docs withMajorThird, withMinorThird
@docs withSuspendedSecond
@docs withSuspendedFourth
@docs withFifth, withSharpFifth, withFlatFifth
@docs withSixth
@docs withMajorSeventh, withMinorSeventh, withDiminishedSeventh
@docs withNinth, withSharpNinth, withFlatNinth
@docs withEleventh, withSharpEleventh
@docs withThirteenth, withFlatThirteenth

@docs classify

@docs includes, includesAll, includesAny

@docs isDiminished, isDominant, isMajor

@docs toIntervals

@docs Serial, toSerial

-}

import Internal.Interval as Interval exposing (Interval)
import Music.Chord.Classification exposing (..)


type ChordType
    = ChordType (List Interval)


type alias Serial =
    { intervals : List Interval.Serial
    }


toSerial : ChordType -> Serial
toSerial (ChordType intervals) =
    { intervals = List.map Interval.toSerial intervals
    }


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
    , halfDiminishedSeventh
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


toString : ChordType -> String
toString theChordType =
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


halfDiminishedSeventh : ChordType
halfDiminishedSeventh =
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
