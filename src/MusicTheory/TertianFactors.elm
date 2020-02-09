module MusicTheory.TertianFactors exposing
    ( Alteration(..)
    , Extension(..)
    , TertianFactors
    , alterations
    , extension
    , hasDiminishedSeventh
    , hasFlatFifth
    , hasMajorSeventh
    , hasMajorThird
    , hasMinorSeventh
    , hasMinorThird
    , hasPerfectFifth
    , hasSharpFifth
    , hasSuspendedFourth
    , hasSuspendedSecond
    , isAddNinthChord
    , isSeventhChord
    , isSixNineChord
    , isSixthChord
    , isTriad
    , tertianFactors
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

import MusicTheory.Interval as Interval exposing (Interval)


type TertianFactors
    = TertianFactors (List TertianFactor)


tertianFactors : TertianFactors
tertianFactors =
    TertianFactors [ Root ]


isTriad : TertianFactors -> Bool
isTriad ((TertianFactors factorList) as factors) =
    includes Root factors
        && includesAny thirdsOrSuspensions factors
        && includesAny fifths factors
        && doesNotIncludeAny sevenths factors
        && doesNotIncludeAny ninths factors
        && doesNotIncludeAny elevenths factors
        && doesNotIncludeAny thirteenths factors
        && (List.length factorList == 3)


hasMajorThird : TertianFactors -> Bool
hasMajorThird factors =
    includes MajorThird factors


hasMinorThird : TertianFactors -> Bool
hasMinorThird factors =
    includes MinorThird factors


hasSuspendedSecond : TertianFactors -> Bool
hasSuspendedSecond factors =
    includes SuspendedSecond factors


hasSuspendedFourth : TertianFactors -> Bool
hasSuspendedFourth factors =
    includes SuspendedFourth factors


hasPerfectFifth : TertianFactors -> Bool
hasPerfectFifth factors =
    includes Fifth factors


hasSharpFifth : TertianFactors -> Bool
hasSharpFifth factors =
    includes SharpFifth factors


hasFlatFifth : TertianFactors -> Bool
hasFlatFifth factors =
    includes FlatFifth factors


hasMajorSeventh : TertianFactors -> Bool
hasMajorSeventh factors =
    includes MajorSeventh factors


hasMinorSeventh : TertianFactors -> Bool
hasMinorSeventh factors =
    includes MinorSeventh factors


hasDiminishedSeventh : TertianFactors -> Bool
hasDiminishedSeventh factors =
    includes DiminishedSeventh factors


isAddNinthChord : TertianFactors -> Bool
isAddNinthChord factors =
    includes Root factors
        && includesAny thirdsOrSuspensions factors
        && includesAny fifths factors
        && doesNotIncludeAny sevenths factors
        && doesNotInclude Sixth factors
        && includes Ninth factors


isSixthChord : TertianFactors -> Bool
isSixthChord factors =
    includes Root factors
        && includesAny thirds factors
        && includesAny fifths factors
        && includes Sixth factors
        && doesNotIncludeAny sevenths factors
        && doesNotInclude Ninth factors


isSixNineChord : TertianFactors -> Bool
isSixNineChord factors =
    includes Root factors
        && includesAny thirds factors
        && includesAny fifths factors
        && includes Sixth factors
        && doesNotIncludeAny sevenths factors
        && includes Ninth factors


isSeventhChord : TertianFactors -> Bool
isSeventhChord factors =
    includes Root factors
        && includesAny thirdsOrSuspensions factors
        && includesAny fifths factors
        && includesAny sevenths factors


alterations : TertianFactors -> List Alteration
alterations (TertianFactors factors) =
    factors
        |> List.map toAlteration
        |> List.filterMap identity
        |> sortAlterations


extension : TertianFactors -> Maybe Extension
extension ((TertianFactors factors) as theTertianFactors) =
    if isSeventhChord theTertianFactors then
        List.filterMap toExtension factors
            |> sortExtensions
            |> List.reverse
            |> List.head

    else
        Nothing


thirds : List TertianFactor
thirds =
    [ MajorThird
    , MinorThird
    ]


suspensions : List TertianFactor
suspensions =
    [ SuspendedSecond
    , SuspendedFourth
    ]


thirdsOrSuspensions : List TertianFactor
thirdsOrSuspensions =
    thirds ++ suspensions


fifths : List TertianFactor
fifths =
    [ Fifth
    , FlatFifth
    , SharpFifth
    ]


sevenths : List TertianFactor
sevenths =
    [ DiminishedSeventh
    , MinorSeventh
    , MajorSeventh
    ]


ninths : List TertianFactor
ninths =
    [ Ninth
    , FlatNinth
    , SharpNinth
    ]


elevenths : List TertianFactor
elevenths =
    [ Eleventh
    , SharpEleventh
    ]


thirteenths : List TertianFactor
thirteenths =
    [ Thirteenth
    , FlatThirteenth
    ]


toIntervals : TertianFactors -> List Interval
toIntervals (TertianFactors factors) =
    factors
        |> sort
        |> List.map toInterval


toInterval : TertianFactor -> Interval
toInterval factor =
    case factor of
        Root ->
            Interval.perfectUnison

        MajorThird ->
            Interval.majorThird

        MinorThird ->
            Interval.minorThird

        SuspendedFourth ->
            Interval.perfectFourth

        SuspendedSecond ->
            Interval.majorSecond

        Fifth ->
            Interval.perfectFifth

        SharpFifth ->
            Interval.augmentedFifth

        FlatFifth ->
            Interval.diminishedFifth

        Sixth ->
            Interval.majorSixth

        DiminishedSeventh ->
            Interval.diminishedSeventh

        MinorSeventh ->
            Interval.minorSeventh

        MajorSeventh ->
            Interval.majorSeventh

        FlatNinth ->
            Interval.minorSecond

        Ninth ->
            Interval.majorSecond

        SharpNinth ->
            Interval.augmentedSecond

        Eleventh ->
            Interval.perfectFourth

        SharpEleventh ->
            Interval.augmentedFourth

        Thirteenth ->
            Interval.majorSixth

        FlatThirteenth ->
            Interval.minorSixth


sort : List TertianFactor -> List TertianFactor
sort factors =
    List.sortBy toComparable factors


toComparable : TertianFactor -> Int
toComparable factor =
    case factor of
        Root ->
            0

        MajorThird ->
            1

        MinorThird ->
            1

        SuspendedFourth ->
            1

        SuspendedSecond ->
            1

        Fifth ->
            2

        FlatFifth ->
            2

        SharpFifth ->
            3

        Sixth ->
            4

        DiminishedSeventh ->
            5

        MinorSeventh ->
            5

        MajorSeventh ->
            5

        FlatNinth ->
            6

        Ninth ->
            6

        SharpNinth ->
            7

        Eleventh ->
            8

        SharpEleventh ->
            8

        Thirteenth ->
            9

        FlatThirteenth ->
            9


withMajorThird : TertianFactors -> TertianFactors
withMajorThird factors =
    factors
        |> remove MinorThird
        |> remove SuspendedSecond
        |> remove SuspendedFourth
        |> add MajorThird


withMinorThird : TertianFactors -> TertianFactors
withMinorThird factors =
    factors
        |> remove MajorThird
        |> remove SuspendedSecond
        |> remove SuspendedFourth
        |> add MinorThird


withSuspendedSecond : TertianFactors -> TertianFactors
withSuspendedSecond factors =
    factors
        |> remove MajorThird
        |> remove MinorThird
        |> remove SuspendedFourth
        |> add SuspendedSecond


withSuspendedFourth : TertianFactors -> TertianFactors
withSuspendedFourth factors =
    factors
        |> remove MajorThird
        |> remove MinorThird
        |> remove SuspendedSecond
        |> add SuspendedFourth


withFifth : TertianFactors -> TertianFactors
withFifth factors =
    factors
        |> remove FlatFifth
        |> remove SharpFifth
        |> add Fifth


withFlatFifth : TertianFactors -> TertianFactors
withFlatFifth factors =
    factors
        |> remove Fifth
        |> add FlatFifth


withSharpFifth : TertianFactors -> TertianFactors
withSharpFifth factors =
    factors
        |> remove Fifth
        |> add SharpFifth


withSixth : TertianFactors -> TertianFactors
withSixth factors =
    factors
        |> remove FlatThirteenth
        |> remove Thirteenth
        |> add Sixth


withDiminishedSeventh : TertianFactors -> TertianFactors
withDiminishedSeventh factors =
    factors
        |> remove MajorSeventh
        |> remove MinorSeventh
        |> remove Sixth
        |> add DiminishedSeventh


withMinorSeventh : TertianFactors -> TertianFactors
withMinorSeventh factors =
    factors
        |> remove MajorSeventh
        |> remove DiminishedSeventh
        |> remove Sixth
        |> add MinorSeventh


withMajorSeventh : TertianFactors -> TertianFactors
withMajorSeventh factors =
    factors
        |> remove MinorSeventh
        |> remove DiminishedSeventh
        |> remove Sixth
        |> add MajorSeventh


withNinth : TertianFactors -> TertianFactors
withNinth factors =
    factors
        |> remove SharpNinth
        |> remove FlatNinth
        |> add Ninth


withFlatNinth : TertianFactors -> TertianFactors
withFlatNinth factors =
    factors
        |> remove Ninth
        |> add FlatNinth


withSharpNinth : TertianFactors -> TertianFactors
withSharpNinth factors =
    factors
        |> remove Ninth
        |> add SharpNinth


withEleventh : TertianFactors -> TertianFactors
withEleventh factors =
    factors
        |> remove SharpEleventh
        |> add Eleventh


withSharpEleventh : TertianFactors -> TertianFactors
withSharpEleventh factors =
    factors
        |> remove Eleventh
        |> add SharpEleventh


withThirteenth : TertianFactors -> TertianFactors
withThirteenth factors =
    factors
        |> remove Sixth
        |> remove FlatThirteenth
        |> add Thirteenth


withFlatThirteenth : TertianFactors -> TertianFactors
withFlatThirteenth factors =
    factors
        |> remove Sixth
        |> remove Thirteenth
        |> add FlatThirteenth


type TertianFactor
    = Root
    | MajorThird
    | MinorThird
    | SuspendedFourth
    | SuspendedSecond
    | Fifth
    | SharpFifth
    | FlatFifth
    | Sixth
    | DiminishedSeventh
    | MinorSeventh
    | MajorSeventh
    | FlatNinth
    | Ninth
    | SharpNinth
    | Eleventh
    | SharpEleventh
    | Thirteenth
    | FlatThirteenth


type Alteration
    = AlterationSharpFifth
    | AlterationFlatFifth
    | AlterationFlatNinth
    | AlterationSharpNinth
    | AlterationSharpEleventh
    | AlterationFlatThirteenth


type Extension
    = ExtensionNinth
    | ExtensionEleventh
    | ExtensionThirteenth


sortExtensions : List Extension -> List Extension
sortExtensions theExtensions =
    List.sortBy extensionToComparable theExtensions


extensionToComparable : Extension -> Int
extensionToComparable theExtension =
    case theExtension of
        ExtensionNinth ->
            0

        ExtensionEleventh ->
            1

        ExtensionThirteenth ->
            2


toExtension : TertianFactor -> Maybe Extension
toExtension factor =
    case factor of
        Ninth ->
            Just ExtensionNinth

        Eleventh ->
            Just ExtensionEleventh

        Thirteenth ->
            Just ExtensionThirteenth

        _ ->
            Nothing


sortAlterations : List Alteration -> List Alteration
sortAlterations theAlterations =
    List.sortBy alterationToComparable theAlterations


alterationToComparable : Alteration -> Int
alterationToComparable alteration =
    case alteration of
        AlterationFlatFifth ->
            0

        AlterationSharpFifth ->
            1

        AlterationFlatNinth ->
            2

        AlterationSharpNinth ->
            3

        AlterationSharpEleventh ->
            4

        AlterationFlatThirteenth ->
            5


toAlteration : TertianFactor -> Maybe Alteration
toAlteration factor =
    case factor of
        FlatFifth ->
            Just AlterationFlatFifth

        SharpFifth ->
            Just AlterationSharpFifth

        FlatNinth ->
            Just AlterationFlatNinth

        SharpNinth ->
            Just AlterationSharpNinth

        SharpEleventh ->
            Just AlterationSharpEleventh

        FlatThirteenth ->
            Just AlterationFlatThirteenth

        _ ->
            Nothing


remove : TertianFactor -> TertianFactors -> TertianFactors
remove factorToRemove (TertianFactors factorList) =
    List.filter ((==) factorToRemove >> not) factorList
        |> TertianFactors


add : TertianFactor -> TertianFactors -> TertianFactors
add factorToAdd (TertianFactors factorList) =
    factorToAdd
        :: factorList
        |> TertianFactors


includes : TertianFactor -> TertianFactors -> Bool
includes factor (TertianFactors factorList) =
    List.member factor factorList


doesNotInclude : TertianFactor -> TertianFactors -> Bool
doesNotInclude factor (TertianFactors factorList) =
    List.member factor factorList
        |> not


includesAny : List TertianFactor -> TertianFactors -> Bool
includesAny factors (TertianFactors factorList) =
    List.any
        (\factor -> List.member factor factorList)
        factors


doesNotIncludeAny : List TertianFactor -> TertianFactors -> Bool
doesNotIncludeAny factors (TertianFactors factorList) =
    List.any
        (\factor -> List.member factor factorList)
        factors
        |> not
