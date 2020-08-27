module Music.Internal.ChordType exposing
    ( ChordType
    , all
    , augmented
    , augmentedDominantSeventh
    , custom
    , diminished
    , diminishedSeventh
    , diminishedSeventhElevenFlatThirteen
    , dominantEleventh
    , dominantNinth
    , dominantSeventh
    , dominantSeventhFlatFive
    , dominantSeventhFlatNine
    , dominantSeventhFlatNineFlatThirteen
    , dominantSeventhFlatNineSharpEleven
    , dominantSeventhFlatNineSharpNine
    , dominantSeventhFlatNineSharpNineFlatThirteen
    , dominantSeventhSharpEleven
    , dominantSeventhSharpElevenFlatThirteen
    , dominantSeventhSharpNine
    , dominantSeventhSharpNineFlatThirteen
    , dominantSeventhSharpNineSharpEleven
    , dominantSeventhSus4
    , dominantThirteenth
    , dominantThirteenthFlatNine
    , dominantThirteenthSharpNine
    , dominantThirteenthSharpNineFlatNine
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


symbol : ChordType -> String
symbol ((ChordType intervals) as theChordType) =
    case premadeSymbol theChordType of
        Just theSymbol ->
            theSymbol

        Nothing ->
            case seventhSymbol theChordType of
                Just theSymbol ->
                    theSymbol

                Nothing ->
                    "?"


type Classification
    = Classification ThirdType FifthType (Maybe SixthOrSeventh) (List Extension)


type ThirdType
    = Major
    | Minor
    | Sus2
    | Sus4


type FifthType
    = Perfect
    | Augmented
    | Diminished


type SixthOrSeventh
    = Sixth
    | MajorSeventh
    | MinorSeventh


type Extension
    = MajorNinth
    | MinorNinth
    | AugmentedNinth
    | PerfectEleventh
    | AugmentedEleventh
    | MajorThirteenth
    | MinorThirteenth


premadeSymbol : ChordType -> Maybe String
premadeSymbol theChordType =
    let
        allSymbols =
            [ ( major, "" )
            , ( minor, "m" )
            , ( augmented, "aug" )
            , ( diminished, "dim" )
            , ( sus2, "sus2" )
            , ( sus4, "sus4" )
            , ( majorSix, "6" )
            , ( majorSixNine, "6/9" )
            , ( minorSix, "m6" )
            , ( minorSixNine, "m6/9" )
            , ( majorAddNine, "(add9)" )
            , ( minorAddNine, "m(add9)" )
            ]
    in
    allSymbols
        |> List.filterMap
            (\( key, value ) ->
                if theChordType == key then
                    Just value

                else
                    Nothing
            )
        |> List.head


seventhSymbol : ChordType -> Maybe String
seventhSymbol theChordType =
    if isSeventh theChordType then
        if includesAll [ Interval.majorThird, Interval.majorSeventh ] theChordType then
            Just <| "M" ++ highestUnalteredExtensionSymbol theChordType ++ alterationSymbols theChordType

        else if includesAll [ Interval.minorThird, Interval.perfectFifth, Interval.minorSeventh ] theChordType then
            Just <| "m" ++ highestUnalteredExtensionSymbol theChordType ++ alterationSymbols theChordType

        else if
            includesAll [ Interval.majorThird, Interval.minorSeventh ] theChordType
                || includesAll [ Interval.perfectFourth, Interval.minorSeventh ] theChordType
        then
            Just <| "" ++ highestUnalteredExtensionSymbol theChordType ++ alterationSymbols theChordType

        else if includesAll [ Interval.minorThird, Interval.diminishedFifth, Interval.diminishedSeventh ] theChordType then
            Just <| "o" ++ highestUnalteredExtensionSymbol theChordType ++ alterationSymbols theChordType

        else if includesAll [ Interval.minorThird, Interval.diminishedFifth, Interval.minorSeventh ] theChordType then
            Just <| "ø" ++ highestUnalteredExtensionSymbol theChordType ++ alterationSymbols theChordType

        else if includesAll [ Interval.minorThird, Interval.majorSeventh ] theChordType then
            Just <| "m/M" ++ highestUnalteredExtensionSymbol theChordType ++ alterationSymbols theChordType

        else
            Nothing

    else
        Nothing


highestUnalteredExtensionSymbol : ChordType -> String
highestUnalteredExtensionSymbol theChordType =
    if includes Interval.majorThirteenth theChordType then
        "13"

    else if includes Interval.perfectEleventh theChordType then
        "11"

    else if includes Interval.majorNinth theChordType then
        "9"

    else
        "7"


alterationSymbols : ChordType -> String
alterationSymbols theChordType =
    let
        alterations =
            [ ( Interval.perfectFourth, "sus4" )
            , ( Interval.augmentedFifth, "♯5" )
            , ( Interval.minorNinth, "♭9" )
            , ( Interval.augmentedNinth, "♯9" )
            , ( Interval.augmentedEleventh, "♯11" )
            , ( Interval.minorThirteenth, "♭13" )
            ]
                |> List.filterMap
                    (\( key, value ) ->
                        if includes key theChordType then
                            Just value

                        else
                            Nothing
                    )
    in
    if not <| List.isEmpty alterations then
        "(" ++ String.join "," alterations ++ ")"

    else
        ""



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
