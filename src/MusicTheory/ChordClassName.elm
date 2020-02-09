module MusicTheory.ChordClassName exposing
    ( AddedToneChord(..)
    , ChordName(..)
    , SeventhChord(..)
    , Triad(..)
    , fromChordClass
    , toString
    )

import MusicTheory.Internal.ChordClass exposing (ChordClass(..))
import MusicTheory.Internal.TertianFactors as TertianFactors
    exposing
        ( Alteration(..)
        , Extension(..)
        , TertianFactors
        )


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


toString : ChordName -> String
toString chordName =
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
            seventhChordNameToString seventhChordName


fromChordClass : ChordClass -> Maybe ChordName
fromChordClass chordClass =
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


seventhChordNameToString : SeventhChord -> String
seventhChordNameToString seventhChord =
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
