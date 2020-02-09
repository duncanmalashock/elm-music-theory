module MusicTheory.ChordClass exposing
    ( ChordClass
    , augmented
    , chordNameToString
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
    , dominantThirteenthFlatNine
    , dominantThirteenthSharpNine
    , dominantThirteenthSharpNineFlatNine
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
    , nonTertian
    , sus2
    , sus4
    , toChordName
    , toIntervals
    )

import MusicTheory.Interval exposing (Interval)
import MusicTheory.TertianFactors as TertianFactors
    exposing
        ( Alteration(..)
        , Extension(..)
        , TertianFactors
        )


type ChordClass
    = Tertian TertianFactors
    | NonTertian (List Interval)


type ChordName
    = Triad Triad
    | AddedToneChord AddedToneChord
    | SeventhChord SeventhChord


type Triad
    = MajorTriad
    | MinorTriad
    | AugmentedTriad
    | DiminishedTriad
    | Sus2Triad
    | Sus4Triad


type AddedToneChord
    = MajorAddSix
    | MinorAddSix
    | MajorAddNine
    | MinorAddNine
    | MajorSixNine
    | MinorSixNine


type SeventhChord
    = MajorSeventh (Maybe Extension) (List Alteration)
    | MinorSeventh (Maybe Extension) (List Alteration)
    | DominantSeventh (Maybe Extension) (List Alteration)
    | DominantSeventhSus4 (Maybe Extension) (List Alteration)
    | HalfDiminished (Maybe Extension) (List Alteration)
    | DiminishedSeventh (Maybe Extension) (List Alteration)
    | MinorMajorSeventh (Maybe Extension) (List Alteration)


chordNameToString : ChordName -> String
chordNameToString chordName =
    case chordName of
        Triad triadName ->
            case triadName of
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

        AddedToneChord addedToneChordName ->
            case addedToneChordName of
                MajorAddSix ->
                    "Maj6"

                MinorAddSix ->
                    "Min6"

                MajorAddNine ->
                    "Maj(add9)"

                MinorAddNine ->
                    "Min(add9)"

                MajorSixNine ->
                    "Maj6/9"

                MinorSixNine ->
                    "Min6/9"

        SeventhChord seventhChordName ->
            seventhChordToString seventhChordName


toChordName : ChordClass -> Maybe ChordName
toChordName chordClass =
    case chordClass of
        NonTertian _ ->
            Nothing

        Tertian factors ->
            if TertianFactors.isTriad factors then
                toTriadChordName factors

            else if TertianFactors.isSixthChord factors then
                toSixthChordName factors

            else if TertianFactors.isAddNinthChord factors then
                toAddNineChordName factors

            else if TertianFactors.isSixNineChord factors then
                toSixNineChordName factors

            else if TertianFactors.isSeventhChord factors then
                toSeventhChordName factors

            else
                Nothing


toTriadChordName : TertianFactors -> Maybe ChordName
toTriadChordName factors =
    if TertianFactors.hasMajorThird factors then
        if TertianFactors.hasPerfectFifth factors then
            Just (Triad MajorTriad)

        else if TertianFactors.hasSharpFifth factors then
            Just (Triad AugmentedTriad)

        else
            Nothing

    else if TertianFactors.hasMinorThird factors then
        if TertianFactors.hasPerfectFifth factors then
            Just (Triad MinorTriad)

        else if TertianFactors.hasFlatFifth factors then
            Just (Triad DiminishedTriad)

        else
            Nothing

    else if TertianFactors.hasSuspendedFourth factors then
        if TertianFactors.hasPerfectFifth factors then
            Just (Triad Sus4Triad)

        else if TertianFactors.hasSuspendedSecond factors then
            if TertianFactors.hasPerfectFifth factors then
                Just (Triad Sus2Triad)

            else
                Nothing

        else
            Nothing

    else
        Nothing


toSixthChordName : TertianFactors -> Maybe ChordName
toSixthChordName factors =
    if TertianFactors.hasMajorThird factors then
        Just (AddedToneChord MajorAddSix)

    else if TertianFactors.hasMinorThird factors then
        Just (AddedToneChord MinorAddSix)

    else
        Nothing


toAddNineChordName : TertianFactors -> Maybe ChordName
toAddNineChordName factors =
    if TertianFactors.hasMajorThird factors then
        Just (AddedToneChord MajorAddNine)

    else if TertianFactors.hasMinorThird factors then
        Just (AddedToneChord MinorAddNine)

    else
        Nothing


toSixNineChordName : TertianFactors -> Maybe ChordName
toSixNineChordName factors =
    if TertianFactors.hasMajorThird factors then
        Just (AddedToneChord MajorSixNine)

    else if TertianFactors.hasMinorThird factors then
        Just (AddedToneChord MinorSixNine)

    else
        Nothing


toSeventhChordName : TertianFactors -> Maybe ChordName
toSeventhChordName factors =
    if TertianFactors.hasMajorThird factors then
        if TertianFactors.hasMajorSeventh factors then
            Just <|
                SeventhChord <|
                    MajorSeventh
                        (TertianFactors.extension factors)
                        (TertianFactors.alterations factors)

        else if TertianFactors.hasMinorSeventh factors then
            Just <|
                SeventhChord <|
                    DominantSeventh
                        (TertianFactors.extension factors)
                        (TertianFactors.alterations factors)

        else
            Nothing

    else if TertianFactors.hasMinorThird factors then
        if TertianFactors.hasMajorSeventh factors then
            Just <|
                SeventhChord <|
                    MinorMajorSeventh
                        (TertianFactors.extension factors)
                        (TertianFactors.alterations factors)

        else if TertianFactors.hasMinorSeventh factors then
            if TertianFactors.hasPerfectFifth factors then
                Just <|
                    SeventhChord <|
                        MinorSeventh
                            (TertianFactors.extension factors)
                            (TertianFactors.alterations factors)

            else if TertianFactors.hasFlatFifth factors then
                if TertianFactors.hasMinorSeventh factors then
                    Just <|
                        SeventhChord <|
                            HalfDiminished
                                (TertianFactors.extension factors)
                                (TertianFactors.alterations factors)

                else if TertianFactors.hasDiminishedSeventh factors then
                    Just <|
                        SeventhChord <|
                            DiminishedSeventh
                                (TertianFactors.extension factors)
                                (TertianFactors.alterations factors)

                else
                    Nothing

            else
                Nothing

        else
            Nothing

    else if TertianFactors.hasSuspendedFourth factors then
        if TertianFactors.hasMinorSeventh factors then
            Just <|
                SeventhChord <|
                    DominantSeventhSus4
                        (TertianFactors.extension factors)
                        (TertianFactors.alterations factors)

        else
            Nothing

    else
        Nothing


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

        HalfDiminished maybeExtension alterations ->
            "ø"
                ++ maybeExtensionToString maybeExtension
                ++ alterationsToString alterations


maybeExtensionToString : Maybe Extension -> String
maybeExtensionToString maybeExtension =
    case maybeExtension of
        Just extension ->
            case extension of
                ExtensionNinth ->
                    "9"

                ExtensionEleventh ->
                    "11"

                ExtensionThirteenth ->
                    "13"

        Nothing ->
            "7"


alterationToString : Alteration -> String
alterationToString alteration =
    case alteration of
        AlterationSharpFifth ->
            "♯5"

        AlterationFlatFifth ->
            "♭5"

        AlterationFlatNinth ->
            "♭9"

        AlterationSharpNinth ->
            "♯9"

        AlterationSharpEleventh ->
            "♯11"

        AlterationFlatThirteenth ->
            "♭13"


alterationsToString : List Alteration -> String
alterationsToString alterations =
    if List.isEmpty alterations then
        ""

    else
        List.map alterationToString alterations
            |> String.join ","
            |> (\alterationStrings ->
                    "(" ++ alterationStrings ++ ")"
               )


toIntervals : ChordClass -> List Interval
toIntervals chordClass =
    case chordClass of
        Tertian tertianFactors ->
            TertianFactors.toIntervals tertianFactors

        NonTertian intervals ->
            intervals



-- Triads


major : ChordClass
major =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> Tertian


minor : ChordClass
minor =
    TertianFactors.tertianFactors
        |> TertianFactors.withMinorThird
        |> TertianFactors.withFifth
        |> Tertian


augmented : ChordClass
augmented =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withSharpFifth
        |> Tertian


diminished : ChordClass
diminished =
    TertianFactors.tertianFactors
        |> TertianFactors.withMinorThird
        |> TertianFactors.withFlatFifth
        |> Tertian


sus2 : ChordClass
sus2 =
    TertianFactors.tertianFactors
        |> TertianFactors.withSuspendedSecond
        |> TertianFactors.withFifth
        |> Tertian


sus4 : ChordClass
sus4 =
    TertianFactors.tertianFactors
        |> TertianFactors.withSuspendedFourth
        |> TertianFactors.withFifth
        |> Tertian



-- Add9 Chords


majorAddNine : ChordClass
majorAddNine =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withNinth
        |> Tertian


minorAddNine : ChordClass
minorAddNine =
    TertianFactors.tertianFactors
        |> TertianFactors.withMinorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withNinth
        |> Tertian



-- Sixth Chords


majorSix : ChordClass
majorSix =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withSixth
        |> Tertian


minorSix : ChordClass
minorSix =
    TertianFactors.tertianFactors
        |> TertianFactors.withMinorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withSixth
        |> Tertian


majorSixNine : ChordClass
majorSixNine =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withSixth
        |> TertianFactors.withNinth
        |> Tertian


minorSixNine : ChordClass
minorSixNine =
    TertianFactors.tertianFactors
        |> TertianFactors.withMinorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withSixth
        |> TertianFactors.withNinth
        |> Tertian



-- Seventh Chords


majorSeventh : ChordClass
majorSeventh =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMajorSeventh
        |> Tertian


minorSeventh : ChordClass
minorSeventh =
    TertianFactors.tertianFactors
        |> TertianFactors.withMinorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> Tertian


dominantSeventh : ChordClass
dominantSeventh =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> Tertian


dominantSeventhSus4 : ChordClass
dominantSeventhSus4 =
    TertianFactors.tertianFactors
        |> TertianFactors.withSuspendedFourth
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> Tertian


minorMajorSeventh : ChordClass
minorMajorSeventh =
    TertianFactors.tertianFactors
        |> TertianFactors.withMinorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMajorSeventh
        |> Tertian


halfDiminished : ChordClass
halfDiminished =
    TertianFactors.tertianFactors
        |> TertianFactors.withMinorThird
        |> TertianFactors.withFlatFifth
        |> TertianFactors.withMinorSeventh
        |> Tertian


diminishedSeventh : ChordClass
diminishedSeventh =
    TertianFactors.tertianFactors
        |> TertianFactors.withMinorThird
        |> TertianFactors.withFlatFifth
        |> TertianFactors.withDiminishedSeventh
        |> Tertian


diminishedSeventhElevenFlatThirteen : ChordClass
diminishedSeventhElevenFlatThirteen =
    TertianFactors.tertianFactors
        |> TertianFactors.withMinorThird
        |> TertianFactors.withFlatFifth
        |> TertianFactors.withDiminishedSeventh
        |> TertianFactors.withFlatThirteenth
        |> Tertian



-- -- Extended Dominants, no altered tones


dominantNinth : ChordClass
dominantNinth =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> TertianFactors.withNinth
        |> Tertian


dominantEleventh : ChordClass
dominantEleventh =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> TertianFactors.withNinth
        |> TertianFactors.withEleventh
        |> Tertian


dominantThirteenth : ChordClass
dominantThirteenth =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> TertianFactors.withNinth
        |> TertianFactors.withEleventh
        |> TertianFactors.withThirteenth
        |> Tertian



-- -- Dominant Seventh Chords, Altered


dominantSeventhSharpNine : ChordClass
dominantSeventhSharpNine =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> TertianFactors.withSharpNinth
        |> Tertian


dominantSeventhFlatNine : ChordClass
dominantSeventhFlatNine =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> TertianFactors.withFlatNinth
        |> Tertian


dominantSeventhFlatNineSharpNine : ChordClass
dominantSeventhFlatNineSharpNine =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> TertianFactors.withFlatNinth
        |> TertianFactors.withSharpNinth
        |> Tertian


dominantSeventhSharpNineFlatThirteen : ChordClass
dominantSeventhSharpNineFlatThirteen =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> TertianFactors.withSharpNinth
        |> TertianFactors.withFlatThirteenth
        |> Tertian


dominantSeventhFlatNineFlatThirteen : ChordClass
dominantSeventhFlatNineFlatThirteen =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> TertianFactors.withFlatNinth
        |> TertianFactors.withFlatThirteenth
        |> Tertian


dominantSeventhFlatNineSharpNineFlatThirteen : ChordClass
dominantSeventhFlatNineSharpNineFlatThirteen =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> TertianFactors.withFlatNinth
        |> TertianFactors.withSharpNinth
        |> TertianFactors.withFlatThirteenth
        |> Tertian


dominantThirteenthFlatNine : ChordClass
dominantThirteenthFlatNine =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> TertianFactors.withFlatNinth
        |> TertianFactors.withThirteenth
        |> Tertian


dominantThirteenthSharpNine : ChordClass
dominantThirteenthSharpNine =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> TertianFactors.withSharpNinth
        |> TertianFactors.withThirteenth
        |> Tertian


dominantThirteenthSharpNineFlatNine : ChordClass
dominantThirteenthSharpNineFlatNine =
    TertianFactors.tertianFactors
        |> TertianFactors.withMajorThird
        |> TertianFactors.withFifth
        |> TertianFactors.withMinorSeventh
        |> TertianFactors.withFlatNinth
        |> TertianFactors.withSharpNinth
        |> TertianFactors.withThirteenth
        |> Tertian


nonTertian : List Interval -> ChordClass
nonTertian intervals =
    NonTertian intervals
