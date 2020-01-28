module MusicTheory.ChordClass exposing (..)

import MusicTheory.Interval as Interval exposing (Interval)


type ChordClass
    = Triad Triad
    | TriadAdd9 MajorOrMinorThird
    | SixthChord MajorOrMinorThird { ninthAdded : Bool }
    | SeventhChord SeventhChord


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


seventhChordToString : SeventhChord -> String
seventhChordToString seventhChord =
    case seventhChord of
        MajorSeventh maybeExtension alterations ->
            "Maj"
                ++ maybeExtensionToString maybeExtension alterations
                ++ alterationsToString alterations

        MinorSeventh maybeExtension alterations ->
            "Min"
                ++ maybeExtensionToString maybeExtension alterations
                ++ alterationsToString alterations

        DominantSeventh maybeExtension alterations ->
            maybeExtensionToString maybeExtension alterations
                ++ alterationsToString alterations

        DominantSeventhSus4 maybeExtension alterations ->
            maybeExtensionToString maybeExtension alterations
                ++ "sus4"
                ++ alterationsToString alterations

        DiminishedSeventh maybeExtension alterations ->
            "Dim"
                ++ maybeExtensionToString maybeExtension alterations
                ++ alterationsToString alterations

        MinorMajorSeventh maybeExtension alterations ->
            "MinMaj"
                ++ maybeExtensionToString maybeExtension alterations
                ++ alterationsToString alterations


maybeExtensionToString : Maybe Extension -> Alterations -> String
maybeExtensionToString maybeExtension alterations =
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
extensionAndAlterationsToIntervals maybeExtension ({ sharpFifth, flatNinth, sharpNinth, sharpEleventh, flatThirteenth } as alterations) =
    let
        maybeNaturalFifth =
            if not sharpFifth then
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
alterationsToIntervals { sharpFifth, flatNinth, sharpNinth, sharpEleventh, flatThirteenth } =
    let
        boolToMaybe condition valueToJust =
            if condition then
                Just valueToJust

            else
                Nothing
    in
    List.filterMap identity
        [ boolToMaybe sharpFifth Interval.augmentedFifth
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


dominantNine : ChordClass
dominantNine =
    SeventhChord <|
        DominantSeventh
            (Just Ninth)
            noAlterations


dominantSharpNine : ChordClass
dominantSharpNine =
    SeventhChord <|
        DominantSeventh
            Nothing
            (noAlterations
                |> sharpNine
            )


dominantFlatNine : ChordClass
dominantFlatNine =
    SeventhChord <|
        DominantSeventh
            Nothing
            (noAlterations
                |> flatNine
            )
