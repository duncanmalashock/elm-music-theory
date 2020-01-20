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
                    []

                MinorTriad ->
                    []

                AugmentedTriad ->
                    []

                DiminishedTriad ->
                    []

                Sus2Triad ->
                    []

                Sus4Triad ->
                    []

        TriadAdd9 majorOrMinorThird ->
            case majorOrMinorThird of
                MajorThird ->
                    []

                MinorThird ->
                    []

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
            []


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
