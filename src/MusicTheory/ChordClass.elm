module MusicTheory.ChordClass exposing
    ( ChordClass
    , augmented
    , diminished
    , diminishedSeventh
    , diminishedSeventhElevenFlatThirteen
    , dominantEleventh
    , dominantNinth
    , dominantSeventh
    , dominantSeventhFlatNine
    , dominantSeventhFlatNineFlatThirteen
    , dominantSeventhFlatNineSharpNine
    , dominantSeventhFlatNineSharpNineFlatThirteen
    , dominantSeventhSharpNine
    , dominantSeventhSharpNineFlatThirteen
    , dominantSeventhSus4
    , dominantThirteenth
    , halfDiminished
    , major
    , majorAddNine
    , majorSeventh
    , majorSix
    , majorSixNine
    , minor
    , minorAddNine
    , minorMajorSeventh
    , minorSeventh
    , minorSix
    , minorSixNine
    , sus2
    , sus4
    , toIntervals
    , toString
    , toTertianFactors
    )

import MusicTheory.Interval as Interval exposing (Interval)


type ChordClass
    = Triad Triad
    | TriadAdd9 MajorOrMinorThird
    | SixthChord MajorOrMinorThird { ninthAdded : Bool }
    | SeventhChord SeventhChord


type alias TertianFactors =
    { root : Interval.Interval
    , thirdOrSus : Interval.Interval
    , fifth : List Interval.Interval
    , seventh : Maybe Interval.Interval
    , ninth : List Interval.Interval
    , eleventh : Maybe Interval.Interval
    , thirteenthOrSixth : Maybe Interval.Interval
    }


type Triad
    = MajorTriad
    | MinorTriad
    | AugmentedTriad
    | DiminishedTriad
    | Sus2Triad
    | Sus4Triad


type MajorOrMinorThird
    = MajorThird
    | MinorThird


type SeventhChord
    = MajorSeventh (Maybe Extension) Alterations
    | MinorSeventh (Maybe Extension) Alterations
    | DominantSeventh (Maybe Extension) Alterations
    | DominantSeventhSus4 (Maybe Extension) Alterations
    | DiminishedSeventh (Maybe Extension) Alterations
    | MinorMajorSeventh (Maybe Extension) Alterations


type Extension
    = Ninth
    | Eleventh
    | Thirteenth


type alias Alterations =
    { flatFifth : Bool
    , sharpFifth : Bool
    , flatNinth : Bool
    , sharpNinth : Bool
    , sharpEleventh : Bool
    , flatThirteenth : Bool
    }


noAlterations : Alterations
noAlterations =
    { flatFifth = False
    , sharpFifth = False
    , flatNinth = False
    , sharpNinth = False
    , sharpEleventh = False
    , flatThirteenth = False
    }


flatFive : Alterations -> Alterations
flatFive alterations =
    { alterations
        | flatFifth = True
    }


sharpNine : Alterations -> Alterations
sharpNine alterations =
    { alterations
        | sharpNinth = True
    }


flatNine : Alterations -> Alterations
flatNine alterations =
    { alterations
        | flatNinth = True
    }


flatThirteen : Alterations -> Alterations
flatThirteen alterations =
    { alterations
        | flatThirteenth = True
    }


seventhChordToString : SeventhChord -> String
seventhChordToString seventhChord =
    case seventhChord of
        MajorSeventh maybeExtension alterations ->
            "Maj"
                ++ maybeExtensionToString maybeExtension
                ++ alterationsToString alterations

        MinorSeventh maybeExtension alterations ->
            "Min"
                ++ maybeExtensionToString maybeExtension
                ++ alterationsToString alterations

        DominantSeventh maybeExtension alterations ->
            maybeExtensionToString maybeExtension
                ++ alterationsToString alterations

        DominantSeventhSus4 maybeExtension alterations ->
            maybeExtensionToString maybeExtension
                ++ "sus4"
                ++ alterationsToString alterations

        DiminishedSeventh maybeExtension alterations ->
            "Dim"
                ++ maybeExtensionToString maybeExtension
                ++ alterationsToString alterations

        MinorMajorSeventh maybeExtension alterations ->
            "MinMaj"
                ++ maybeExtensionToString maybeExtension
                ++ alterationsToString alterations


maybeExtensionToString : Maybe Extension -> String
maybeExtensionToString maybeExtension =
    case maybeExtension of
        Just extension ->
            case extension of
                Ninth ->
                    "9"

                Eleventh ->
                    "11"

                Thirteenth ->
                    "13"

        Nothing ->
            "7"


alterationsToString : Alterations -> String
alterationsToString { flatFifth, sharpFifth, flatNinth, sharpNinth, sharpEleventh, flatThirteenth } =
    [ if flatFifth then
        Just "♭5"

      else
        Nothing
    , if sharpFifth then
        Just "♯5"

      else
        Nothing
    , if flatNinth then
        Just "♭9"

      else
        Nothing
    , if sharpNinth then
        Just "♯9"

      else
        Nothing
    , if sharpEleventh then
        Just "♯11"

      else
        Nothing
    , if flatThirteenth then
        Just "♭13"

      else
        Nothing
    ]
        |> List.filterMap identity
        |> (\stringList ->
                if List.isEmpty stringList then
                    ""

                else
                    stringList
                        |> String.join ","
                        |> (\alterationStrings ->
                                "(" ++ alterationStrings ++ ")"
                           )
           )


toString : ChordClass -> String
toString chordClass =
    case chordClass of
        Triad triad ->
            case triad of
                MajorTriad ->
                    "Maj"

                MinorTriad ->
                    "Min"

                AugmentedTriad ->
                    "Aug"

                DiminishedTriad ->
                    "Dim"

                Sus2Triad ->
                    "Sus2"

                Sus4Triad ->
                    "Sus4"

        TriadAdd9 majorOrMinorThird ->
            case majorOrMinorThird of
                MajorThird ->
                    "Maj(add9)"

                MinorThird ->
                    "Min(add9)"

        SixthChord majorOrMinorThird { ninthAdded } ->
            case majorOrMinorThird of
                MajorThird ->
                    if ninthAdded then
                        "Maj6/9"

                    else
                        "Maj6"

                MinorThird ->
                    if ninthAdded then
                        "Min6/9"

                    else
                        "Min6"

        SeventhChord seventhChord ->
            seventhChordToString seventhChord



-- To TertianFactors


triadToTertianFactors :
    { root : Interval.Interval
    , thirdOrSus : Interval.Interval
    , fifth : Interval.Interval
    }
    -> TertianFactors
triadToTertianFactors { root, thirdOrSus, fifth } =
    { root = root
    , thirdOrSus = thirdOrSus
    , fifth = [ fifth ]
    , seventh = Nothing
    , ninth = []
    , eleventh = Nothing
    , thirteenthOrSixth = Nothing
    }


tertianFactorsWithNinth : Interval -> TertianFactors -> TertianFactors
tertianFactorsWithNinth newNinth factors =
    { factors
        | ninth = factors.ninth ++ [ newNinth ]
    }


tertianFactorsWithSixth : Interval -> TertianFactors -> TertianFactors
tertianFactorsWithSixth newSixth factors =
    { factors
        | thirteenthOrSixth = Just newSixth
    }


toTertianFactors : ChordClass -> TertianFactors
toTertianFactors chordClass =
    case chordClass of
        Triad triad ->
            case triad of
                MajorTriad ->
                    triadToTertianFactors
                        { root = Interval.perfectUnison
                        , thirdOrSus = Interval.majorThird
                        , fifth = Interval.perfectFifth
                        }

                MinorTriad ->
                    triadToTertianFactors
                        { root = Interval.perfectUnison
                        , thirdOrSus = Interval.minorThird
                        , fifth = Interval.perfectFifth
                        }

                AugmentedTriad ->
                    triadToTertianFactors
                        { root = Interval.perfectUnison
                        , thirdOrSus = Interval.majorThird
                        , fifth = Interval.augmentedFifth
                        }

                DiminishedTriad ->
                    triadToTertianFactors
                        { root = Interval.perfectUnison
                        , thirdOrSus = Interval.minorThird
                        , fifth = Interval.diminishedFifth
                        }

                Sus2Triad ->
                    triadToTertianFactors
                        { root = Interval.perfectUnison
                        , thirdOrSus = Interval.majorSecond
                        , fifth = Interval.perfectFifth
                        }

                Sus4Triad ->
                    triadToTertianFactors
                        { root = Interval.perfectUnison
                        , thirdOrSus = Interval.perfectFourth
                        , fifth = Interval.perfectFifth
                        }

        TriadAdd9 majorOrMinorThird ->
            triadToTertianFactors
                { root = Interval.perfectUnison
                , thirdOrSus = majorOrMinorThirdToInterval majorOrMinorThird
                , fifth = Interval.perfectFifth
                }
                |> tertianFactorsWithNinth Interval.majorSecond

        SixthChord majorOrMinorThird { ninthAdded } ->
            triadToTertianFactors
                { root = Interval.perfectUnison
                , thirdOrSus = majorOrMinorThirdToInterval majorOrMinorThird
                , fifth = Interval.perfectFifth
                }
                |> tertianFactorsWithSixth Interval.majorSixth
                |> (if ninthAdded then
                        tertianFactorsWithNinth Interval.majorSecond

                    else
                        identity
                   )

        SeventhChord seventhChord ->
            seventhChordToTertianFactors seventhChord


seventhChordToTertianFactors : SeventhChord -> List Interval.Interval
seventhChordToTertianFactors seventhChord =
    case seventhChord of
        MajorSeventh maybeExtension alterations ->
            [ Interval.perfectUnison
            , Interval.majorThird
            , Interval.majorSeventh
            ]
                ++ extensionAndAlterationsToTertianFactors maybeExtension alterations

        MinorSeventh maybeExtension alterations ->
            [ Interval.perfectUnison
            , Interval.minorThird
            , Interval.minorSeventh
            ]
                ++ extensionAndAlterationsToTertianFactors maybeExtension alterations

        DominantSeventh maybeExtension alterations ->
            [ Interval.perfectUnison
            , Interval.majorThird
            , Interval.minorSeventh
            ]
                ++ extensionAndAlterationsToTertianFactors maybeExtension alterations

        DominantSeventhSus4 maybeExtension alterations ->
            [ Interval.perfectUnison
            , Interval.perfectFourth
            , Interval.minorSeventh
            ]
                ++ extensionAndAlterationsToTertianFactors maybeExtension alterations

        DiminishedSeventh maybeExtension alterations ->
            [ Interval.perfectUnison
            , Interval.minorThird
            , Interval.diminishedSeventh
            ]
                ++ extensionAndAlterationsToTertianFactors maybeExtension alterations

        MinorMajorSeventh maybeExtension alterations ->
            [ Interval.perfectUnison
            , Interval.minorThird
            , Interval.majorSeventh
            ]
                ++ extensionAndAlterationsToTertianFactors maybeExtension alterations


extensionAndAlterationsToTertianFactors : Maybe Extension -> Alterations -> List Interval.Interval
extensionAndAlterationsToTertianFactors maybeExtension ({ flatFifth, sharpFifth, flatNinth, sharpNinth, sharpEleventh, flatThirteenth } as alterations) =
    let
        maybeNaturalFifth =
            if not sharpFifth || flatFifth then
                [ Interval.perfectFifth ]

            else
                []

        maybeNaturalNinth =
            if not (sharpNinth || flatNinth) then
                [ Interval.majorSecond ]

            else
                []

        maybeNaturalEleventh =
            if not sharpEleventh then
                [ Interval.perfectFourth ]

            else
                []

        maybeNaturalThirteenth =
            if not flatThirteenth then
                [ Interval.majorSixth ]

            else
                []
    in
    case maybeExtension of
        Nothing ->
            maybeNaturalFifth
                ++ alterationsToTertianFactors alterations

        Just Ninth ->
            maybeNaturalNinth
                ++ alterationsToTertianFactors alterations

        Just Eleventh ->
            maybeNaturalEleventh
                ++ maybeNaturalNinth
                ++ alterationsToTertianFactors alterations

        Just Thirteenth ->
            maybeNaturalThirteenth
                ++ maybeNaturalEleventh
                ++ maybeNaturalNinth
                ++ alterationsToTertianFactors alterations


alterationsToTertianFactors : Alterations -> List Interval.Interval
alterationsToTertianFactors { flatFifth, sharpFifth, flatNinth, sharpNinth, sharpEleventh, flatThirteenth } =
    let
        boolToMaybe condition valueToJust =
            if condition then
                Just valueToJust

            else
                Nothing
    in
    List.filterMap identity
        [ boolToMaybe flatFifth Interval.diminishedFifth
        , boolToMaybe sharpFifth Interval.augmentedFifth
        , boolToMaybe flatNinth Interval.minorSecond
        , boolToMaybe sharpNinth Interval.augmentedSecond
        , boolToMaybe sharpEleventh Interval.augmentedFourth
        , boolToMaybe flatThirteenth Interval.minorSixth
        ]



-- To Intervals


toIntervals : ChordClass -> List Interval
toIntervals chordClass =
    case chordClass of
        Triad triad ->
            case triad of
                MajorTriad ->
                    [ Interval.perfectUnison
                    , Interval.majorThird
                    , Interval.perfectFifth
                    ]

                MinorTriad ->
                    [ Interval.perfectUnison
                    , Interval.minorThird
                    , Interval.perfectFifth
                    ]

                AugmentedTriad ->
                    [ Interval.perfectUnison
                    , Interval.majorThird
                    , Interval.augmentedFifth
                    ]

                DiminishedTriad ->
                    [ Interval.perfectUnison
                    , Interval.minorThird
                    , Interval.diminishedFifth
                    ]

                Sus2Triad ->
                    [ Interval.perfectUnison
                    , Interval.majorSecond
                    , Interval.perfectFifth
                    ]

                Sus4Triad ->
                    [ Interval.perfectUnison
                    , Interval.perfectFourth
                    , Interval.perfectFifth
                    ]

        TriadAdd9 majorOrMinorThird ->
            [ Interval.perfectUnison
            , majorOrMinorThirdToInterval majorOrMinorThird
            , Interval.perfectFifth
            ]

        SixthChord majorOrMinorThird { ninthAdded } ->
            [ Interval.perfectUnison
            , majorOrMinorThirdToInterval majorOrMinorThird
            , Interval.perfectFifth
            , Interval.majorSixth
            ]
                ++ (if ninthAdded then
                        [ Interval.majorSecond ]

                    else
                        []
                   )

        SeventhChord seventhChord ->
            seventhChordToIntervals seventhChord


seventhChordToIntervals : SeventhChord -> List Interval.Interval
seventhChordToIntervals seventhChord =
    case seventhChord of
        MajorSeventh maybeExtension alterations ->
            [ Interval.perfectUnison
            , Interval.majorThird
            , Interval.majorSeventh
            ]
                ++ extensionAndAlterationsToIntervals maybeExtension alterations

        MinorSeventh maybeExtension alterations ->
            [ Interval.perfectUnison
            , Interval.minorThird
            , Interval.minorSeventh
            ]
                ++ extensionAndAlterationsToIntervals maybeExtension alterations

        DominantSeventh maybeExtension alterations ->
            [ Interval.perfectUnison
            , Interval.majorThird
            , Interval.minorSeventh
            ]
                ++ extensionAndAlterationsToIntervals maybeExtension alterations

        DominantSeventhSus4 maybeExtension alterations ->
            [ Interval.perfectUnison
            , Interval.perfectFourth
            , Interval.minorSeventh
            ]
                ++ extensionAndAlterationsToIntervals maybeExtension alterations

        DiminishedSeventh maybeExtension alterations ->
            [ Interval.perfectUnison
            , Interval.minorThird
            , Interval.diminishedSeventh
            ]
                ++ extensionAndAlterationsToIntervals maybeExtension alterations

        MinorMajorSeventh maybeExtension alterations ->
            [ Interval.perfectUnison
            , Interval.minorThird
            , Interval.majorSeventh
            ]
                ++ extensionAndAlterationsToIntervals maybeExtension alterations


extensionAndAlterationsToIntervals : Maybe Extension -> Alterations -> List Interval.Interval
extensionAndAlterationsToIntervals maybeExtension ({ flatFifth, sharpFifth, flatNinth, sharpNinth, sharpEleventh, flatThirteenth } as alterations) =
    let
        maybeNaturalFifth =
            if not sharpFifth || flatFifth then
                [ Interval.perfectFifth ]

            else
                []

        maybeNaturalNinth =
            if not (sharpNinth || flatNinth) then
                [ Interval.majorSecond ]

            else
                []

        maybeNaturalEleventh =
            if not sharpEleventh then
                [ Interval.perfectFourth ]

            else
                []

        maybeNaturalThirteenth =
            if not flatThirteenth then
                [ Interval.majorSixth ]

            else
                []
    in
    case maybeExtension of
        Nothing ->
            maybeNaturalFifth
                ++ alterationsToIntervals alterations

        Just Ninth ->
            maybeNaturalNinth
                ++ alterationsToIntervals alterations

        Just Eleventh ->
            maybeNaturalEleventh
                ++ maybeNaturalNinth
                ++ alterationsToIntervals alterations

        Just Thirteenth ->
            maybeNaturalThirteenth
                ++ maybeNaturalEleventh
                ++ maybeNaturalNinth
                ++ alterationsToIntervals alterations


alterationsToIntervals : Alterations -> List Interval.Interval
alterationsToIntervals { flatFifth, sharpFifth, flatNinth, sharpNinth, sharpEleventh, flatThirteenth } =
    let
        boolToMaybe condition valueToJust =
            if condition then
                Just valueToJust

            else
                Nothing
    in
    List.filterMap identity
        [ boolToMaybe flatFifth Interval.diminishedFifth
        , boolToMaybe sharpFifth Interval.augmentedFifth
        , boolToMaybe flatNinth Interval.minorSecond
        , boolToMaybe sharpNinth Interval.augmentedSecond
        , boolToMaybe sharpEleventh Interval.augmentedFourth
        , boolToMaybe flatThirteenth Interval.minorSixth
        ]


majorOrMinorThirdToInterval : MajorOrMinorThird -> Interval
majorOrMinorThirdToInterval majorOrMinorThird =
    case majorOrMinorThird of
        MajorThird ->
            Interval.majorThird

        MinorThird ->
            Interval.minorThird



-- Triads


major : ChordClass
major =
    Triad MajorTriad


minor : ChordClass
minor =
    Triad MinorTriad


augmented : ChordClass
augmented =
    Triad AugmentedTriad


diminished : ChordClass
diminished =
    Triad DiminishedTriad


sus2 : ChordClass
sus2 =
    Triad Sus2Triad


sus4 : ChordClass
sus4 =
    Triad Sus4Triad



-- Add9 Chords


majorAddNine : ChordClass
majorAddNine =
    TriadAdd9
        MajorThird


minorAddNine : ChordClass
minorAddNine =
    TriadAdd9
        MinorThird



-- Sixth Chords


majorSix : ChordClass
majorSix =
    SixthChord
        MajorThird
        { ninthAdded = False }


minorSix : ChordClass
minorSix =
    SixthChord
        MinorThird
        { ninthAdded = False }


majorSixNine : ChordClass
majorSixNine =
    SixthChord
        MajorThird
        { ninthAdded = True }


minorSixNine : ChordClass
minorSixNine =
    SixthChord
        MinorThird
        { ninthAdded = True }



-- Seventh Chords


majorSeventh : ChordClass
majorSeventh =
    SeventhChord <|
        MajorSeventh
            Nothing
            noAlterations


minorSeventh : ChordClass
minorSeventh =
    SeventhChord <|
        MinorSeventh
            Nothing
            noAlterations


dominantSeventh : ChordClass
dominantSeventh =
    SeventhChord <|
        DominantSeventh
            Nothing
            noAlterations


dominantSeventhSus4 : ChordClass
dominantSeventhSus4 =
    SeventhChord <|
        DominantSeventhSus4
            Nothing
            noAlterations


minorMajorSeventh : ChordClass
minorMajorSeventh =
    SeventhChord <|
        MinorMajorSeventh
            Nothing
            noAlterations


halfDiminished : ChordClass
halfDiminished =
    SeventhChord <|
        MinorSeventh
            Nothing
            noAlterations


diminishedSeventh : ChordClass
diminishedSeventh =
    SeventhChord <|
        DiminishedSeventh
            Nothing
            (noAlterations
                |> flatFive
            )


diminishedSeventhElevenFlatThirteen : ChordClass
diminishedSeventhElevenFlatThirteen =
    SeventhChord <|
        DiminishedSeventh
            (Just Eleventh)
            (noAlterations
                |> flatFive
                |> flatThirteen
            )



-- Extended Dominants, no altered tones


dominantNinth : ChordClass
dominantNinth =
    SeventhChord <|
        DominantSeventh
            (Just Ninth)
            noAlterations


dominantEleventh : ChordClass
dominantEleventh =
    SeventhChord <|
        DominantSeventh
            (Just Eleventh)
            noAlterations


dominantThirteenth : ChordClass
dominantThirteenth =
    SeventhChord <|
        DominantSeventh
            (Just Thirteenth)
            noAlterations



-- Dominant Seventh Chords, Altered


dominantSeventhSharpNine : ChordClass
dominantSeventhSharpNine =
    SeventhChord <|
        DominantSeventh
            Nothing
            (noAlterations
                |> sharpNine
            )


dominantSeventhFlatNine : ChordClass
dominantSeventhFlatNine =
    SeventhChord <|
        DominantSeventh
            Nothing
            (noAlterations
                |> flatNine
            )


dominantSeventhFlatNineSharpNine : ChordClass
dominantSeventhFlatNineSharpNine =
    SeventhChord <|
        DominantSeventh
            Nothing
            (noAlterations
                |> flatNine
                |> sharpNine
            )


dominantSeventhSharpNineFlatThirteen : ChordClass
dominantSeventhSharpNineFlatThirteen =
    SeventhChord <|
        DominantSeventh
            Nothing
            (noAlterations
                |> sharpNine
                |> flatThirteen
            )


dominantSeventhFlatNineFlatThirteen : ChordClass
dominantSeventhFlatNineFlatThirteen =
    SeventhChord <|
        DominantSeventh
            Nothing
            (noAlterations
                |> flatNine
                |> flatThirteen
            )


dominantSeventhFlatNineSharpNineFlatThirteen : ChordClass
dominantSeventhFlatNineSharpNineFlatThirteen =
    SeventhChord <|
        DominantSeventh
            Nothing
            (noAlterations
                |> flatNine
                |> sharpNine
                |> flatThirteen
            )
