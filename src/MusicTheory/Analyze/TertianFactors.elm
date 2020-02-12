module MusicTheory.Analyze.TertianFactors exposing
    ( isFifthToneCategory
    , isRootToneCategory
    , isSeventhToneCategory
    , isThirdToneCategory
    , nextToneCategory
    )

import MusicTheory.TertianFactors exposing (TertianFactor(..))


type JazzChordToneCategory
    = RootCategory
    | ThirdCategory
    | FifthCategory
    | SeventhCategory


isRootToneCategory : TertianFactor -> Bool
isRootToneCategory factor =
    tertianFactorToJazzChordToneCategory factor == RootCategory


isThirdToneCategory : TertianFactor -> Bool
isThirdToneCategory factor =
    tertianFactorToJazzChordToneCategory factor == ThirdCategory


isFifthToneCategory : TertianFactor -> Bool
isFifthToneCategory factor =
    tertianFactorToJazzChordToneCategory factor == FifthCategory


isSeventhToneCategory : TertianFactor -> Bool
isSeventhToneCategory factor =
    tertianFactorToJazzChordToneCategory factor == SeventhCategory


nextToneCategory : JazzChordToneCategory -> JazzChordToneCategory
nextToneCategory toneCategory =
    case toneCategory of
        RootCategory ->
            ThirdCategory

        ThirdCategory ->
            FifthCategory

        FifthCategory ->
            SeventhCategory

        SeventhCategory ->
            RootCategory


tertianFactorToJazzChordToneCategory : TertianFactor -> JazzChordToneCategory
tertianFactorToJazzChordToneCategory factor =
    case factor of
        Root ->
            RootCategory

        FlatNinth ->
            RootCategory

        Ninth ->
            RootCategory

        SharpNinth ->
            RootCategory

        MajorThird ->
            ThirdCategory

        MinorThird ->
            ThirdCategory

        SuspendedFourth ->
            ThirdCategory

        SuspendedSecond ->
            ThirdCategory

        Eleventh ->
            FifthCategory

        SharpEleventh ->
            FifthCategory

        Fifth ->
            FifthCategory

        SharpFifth ->
            FifthCategory

        FlatFifth ->
            FifthCategory

        Thirteenth ->
            FifthCategory

        FlatThirteenth ->
            FifthCategory

        Sixth ->
            SeventhCategory

        DiminishedSeventh ->
            SeventhCategory

        MinorSeventh ->
            SeventhCategory

        MajorSeventh ->
            SeventhCategory
