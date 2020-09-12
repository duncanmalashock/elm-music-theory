module Music.Internal.Analysis exposing
    ( Analysis
    , DefaultChordTypes
    , fromChord
    , i
    , iFlat
    , iSharp
    , ii
    , iiFlat
    , iiSharp
    , iii
    , iiiFlat
    , iiiSharp
    , iv
    , ivFlat
    , ivSharp
    , seventhsByDefault
    , symbol
    , toChord
    , triadsByDefault
    , v
    , vFlat
    , vOfii
    , vOfiii
    , vOfiv
    , vOfv
    , vOfvi
    , vSharp
    , vi
    , viFlat
    , viSharp
    , vii
    , viiFlat
    , viiSharp
    , withChordType
    )

import Music.Chord as Chord
import Music.Internal.Chord as InternalChord
import Music.Internal.ChordType as ChordType
import Music.Internal.Interval as Interval
import Music.Internal.Key as Key
import Music.Internal.PitchClass as PitchClass
import Music.Internal.Scale as Scale


type Analysis
    = Degree RomanNumeral PitchClass.Offset (Maybe ChordType.ChordType)
    | SecondaryDominant RomanNumeral (Maybe ChordType.ChordType)


i : Analysis
i =
    Degree I (PitchClass.offsetFromInt 0) Nothing


iSharp : ChordType.ChordType -> Analysis
iSharp chordType =
    Degree I (PitchClass.offsetFromInt 1) (Just chordType)


iFlat : ChordType.ChordType -> Analysis
iFlat chordType =
    Degree I (PitchClass.offsetFromInt -1) (Just chordType)


ii : Analysis
ii =
    Degree II (PitchClass.offsetFromInt 0) Nothing


iiSharp : ChordType.ChordType -> Analysis
iiSharp chordType =
    Degree II (PitchClass.offsetFromInt 1) (Just chordType)


iiFlat : ChordType.ChordType -> Analysis
iiFlat chordType =
    Degree II (PitchClass.offsetFromInt -1) (Just chordType)


iii : Analysis
iii =
    Degree III (PitchClass.offsetFromInt 0) Nothing


iiiSharp : ChordType.ChordType -> Analysis
iiiSharp chordType =
    Degree III (PitchClass.offsetFromInt 1) (Just chordType)


iiiFlat : ChordType.ChordType -> Analysis
iiiFlat chordType =
    Degree III (PitchClass.offsetFromInt -1) (Just chordType)


iv : Analysis
iv =
    Degree IV (PitchClass.offsetFromInt 0) Nothing


ivSharp : ChordType.ChordType -> Analysis
ivSharp chordType =
    Degree IV (PitchClass.offsetFromInt 1) (Just chordType)


ivFlat : ChordType.ChordType -> Analysis
ivFlat chordType =
    Degree IV (PitchClass.offsetFromInt -1) (Just chordType)


v : Analysis
v =
    Degree V (PitchClass.offsetFromInt 0) Nothing


vSharp : ChordType.ChordType -> Analysis
vSharp chordType =
    Degree V (PitchClass.offsetFromInt 1) (Just chordType)


vFlat : ChordType.ChordType -> Analysis
vFlat chordType =
    Degree V (PitchClass.offsetFromInt -1) (Just chordType)


vi : Analysis
vi =
    Degree VI (PitchClass.offsetFromInt 0) Nothing


viSharp : ChordType.ChordType -> Analysis
viSharp chordType =
    Degree VI (PitchClass.offsetFromInt 1) (Just chordType)


viFlat : ChordType.ChordType -> Analysis
viFlat chordType =
    Degree VI (PitchClass.offsetFromInt -1) (Just chordType)


vii : Analysis
vii =
    Degree VII (PitchClass.offsetFromInt 0) Nothing


viiFlat : ChordType.ChordType -> Analysis
viiFlat chordType =
    Degree VII (PitchClass.offsetFromInt -1) (Just chordType)


viiSharp : ChordType.ChordType -> Analysis
viiSharp chordType =
    Degree VII (PitchClass.offsetFromInt 1) (Just chordType)


vOfii : Analysis
vOfii =
    SecondaryDominant II Nothing


vOfiii : Analysis
vOfiii =
    SecondaryDominant III Nothing


vOfiv : Analysis
vOfiv =
    SecondaryDominant IV Nothing


vOfv : Analysis
vOfv =
    SecondaryDominant V Nothing


vOfvi : Analysis
vOfvi =
    SecondaryDominant VI Nothing


withChordType : ChordType.ChordType -> Analysis -> Analysis
withChordType chordType analysis =
    case analysis of
        Degree numeral offset _ ->
            Degree numeral offset (Just chordType)

        SecondaryDominant romanNumeral _ ->
            SecondaryDominant romanNumeral (Just chordType)


symbol : Key.Key -> Analysis -> String
symbol key analysis =
    let
        keyIsMajor =
            Key.isMajor key

        maybeChordType =
            case analysis of
                Degree _ _ maybeChordType_ ->
                    maybeChordType_

                SecondaryDominant _ maybeChordType_ ->
                    maybeChordType_

        numeral =
            case analysis of
                Degree romanNumeral _ _ ->
                    romanNumeral

                SecondaryDominant romanNumeral _ ->
                    romanNumeral

        chordType =
            case analysis of
                Degree _ _ _ ->
                    case maybeChordType of
                        Just theChordType ->
                            theChordType

                        Nothing ->
                            if keyIsMajor then
                                defaultChordType triadsByDefault.major numeral

                            else
                                defaultChordType triadsByDefault.minor numeral

                SecondaryDominant _ _ ->
                    case maybeChordType of
                        Just theChordType ->
                            theChordType

                        Nothing ->
                            ChordType.major

        usingDefaultChordType =
            case maybeChordType of
                Just theChordType ->
                    False

                Nothing ->
                    True
    in
    case analysis of
        Degree _ offset _ ->
            PitchClass.offsetToString offset
                ++ numeralToString numeral chordType
                ++ (if usingDefaultChordType then
                        ""

                    else
                        ChordType.toString chordType
                   )

        SecondaryDominant _ _ ->
            ("V" ++ ChordType.toString chordType)
                ++ "/"
                ++ numeralToString numeral
                    (defaultChordType
                        (if keyIsMajor then
                            triadsByDefault.major

                         else
                            triadsByDefault.minor
                        )
                        numeral
                    )


numeralToString : RomanNumeral -> ChordType.ChordType -> String
numeralToString numeral chordType =
    let
        isMajorOrDominant =
            ChordType.isDominant chordType
                || ChordType.isMajor chordType
    in
    case numeral of
        I ->
            if isMajorOrDominant then
                "I"

            else
                "i"

        II ->
            if isMajorOrDominant then
                "II"

            else
                "ii"

        III ->
            if isMajorOrDominant then
                "III"

            else
                "iii"

        IV ->
            if isMajorOrDominant then
                "IV"

            else
                "iv"

        V ->
            if isMajorOrDominant then
                "V"

            else
                "v"

        VI ->
            if isMajorOrDominant then
                "VI"

            else
                "vi"

        VII ->
            if isMajorOrDominant then
                "VII"

            else
                "vii"


type RomanNumeral
    = I
    | II
    | III
    | IV
    | V
    | VI
    | VII


fromChord : Key.Key -> Chord.Chord -> Analysis
fromChord key chord =
    let
        ( numeral, offset ) =
            numeralAndOffset key (Chord.root chord)
    in
    case secondaryDominant key chord of
        Just theSecondaryDominant ->
            theSecondaryDominant

        Nothing ->
            Degree numeral offset (maybeChordTypeFromContext key numeral offset (Chord.chordType chord))


secondaryDominant : Key.Key -> Chord.Chord -> Maybe Analysis
secondaryDominant key chord =
    let
        noOffset =
            PitchClass.offsetFromInt 0

        secondaryDominantInfo =
            [ ( II, pitchClassFromNumeralAndOffset key VI noOffset, canFunctionAsSecondaryDominant )
            , ( III, pitchClassFromNumeralAndOffset key VII noOffset, canFunctionAsSecondaryDominant )
            , ( IV, pitchClassFromNumeralAndOffset key I noOffset, canFunctionAsVofIV key )
            , ( V, pitchClassFromNumeralAndOffset key II noOffset, canFunctionAsSecondaryDominant )
            , ( VI, pitchClassFromNumeralAndOffset key III noOffset, canFunctionAsSecondaryDominant )
            ]

        matching =
            List.filter
                (\triplet ->
                    case triplet of
                        ( secDomOf, root, canFunction ) ->
                            (Chord.root chord == root)
                                && canFunction (Chord.chordType chord)
                )
                secondaryDominantInfo
                |> List.head

        maybeChordType =
            if Chord.chordType chord == ChordType.major then
                Nothing

            else
                Just (Chord.chordType chord)
    in
    case matching of
        Just ( secDomOf, _, _ ) ->
            Just (SecondaryDominant secDomOf maybeChordType)

        Nothing ->
            Nothing


canFunctionAsSecondaryDominant : ChordType.ChordType -> Bool
canFunctionAsSecondaryDominant chordType =
    ChordType.includes Interval.majorThird chordType
        && (not <| ChordType.includes Interval.majorSeventh chordType)


canFunctionAsVofIV : Key.Key -> ChordType.ChordType -> Bool
canFunctionAsVofIV key chordType =
    if Key.isMajor key then
        ChordType.includes Interval.majorThird chordType
            && ChordType.includes Interval.minorSeventh chordType

    else
        ChordType.includes Interval.majorThird chordType
            && (not <| ChordType.includes Interval.majorSeventh chordType)


pitchClassFromNumeralAndOffset : Key.Key -> RomanNumeral -> PitchClass.Offset -> PitchClass.PitchClass
pitchClassFromNumeralAndOffset key numeral offset =
    let
        intervalToTranspose =
            if Key.isMajor key then
                case numeral of
                    I ->
                        Interval.perfectUnison

                    II ->
                        Interval.majorSecond

                    III ->
                        Interval.majorThird

                    IV ->
                        Interval.perfectFourth

                    V ->
                        Interval.perfectFifth

                    VI ->
                        Interval.majorSixth

                    VII ->
                        Interval.majorSeventh

            else
                case numeral of
                    I ->
                        Interval.perfectUnison

                    II ->
                        Interval.majorSecond

                    III ->
                        Interval.minorThird

                    IV ->
                        Interval.perfectFourth

                    V ->
                        Interval.perfectFifth

                    VI ->
                        Interval.minorSixth

                    VII ->
                        Interval.minorSeventh
    in
    PitchClass.transpose intervalToTranspose (Key.tonic key)
        |> PitchClass.addOffset offset


toChord : DefaultChordTypes -> Key.Key -> Analysis -> Chord.Chord
toChord defaults key analysis =
    let
        root : PitchClass.PitchClass
        root =
            case analysis of
                Degree romanNumeral offset maybeChordType ->
                    pitchClassFromNumeralAndOffset key romanNumeral offset

                SecondaryDominant romanNumeral maybeChordType ->
                    pitchClassFromNumeralAndOffset key romanNumeral (PitchClass.offsetFromInt 0)
                        |> PitchClass.transpose Interval.perfectFifth

        newChordType : ChordType.ChordType
        newChordType =
            case analysis of
                Degree romanNumeral offset maybeChordType ->
                    case maybeChordType of
                        Just chordType ->
                            chordType

                        Nothing ->
                            defaults
                                |> (if Key.isMajor key then
                                        .major

                                    else
                                        .minor
                                   )
                                |> (case romanNumeral of
                                        I ->
                                            .i

                                        II ->
                                            .ii

                                        III ->
                                            .iii

                                        IV ->
                                            .iv

                                        V ->
                                            .v

                                        VI ->
                                            .vi

                                        VII ->
                                            .vii
                                   )

                SecondaryDominant romanNumeral maybeChordType ->
                    case maybeChordType of
                        Just chordType ->
                            chordType

                        Nothing ->
                            defaults
                                |> (if Key.isMajor key then
                                        .major

                                    else
                                        .minor
                                   )
                                |> .secondaryDominant
    in
    InternalChord.chord root newChordType


type alias DefaultChordTypes =
    { major : DefaultChordTypesForMode
    , minor : DefaultChordTypesForMode
    }


type alias DefaultChordTypesForMode =
    { i : ChordType.ChordType
    , ii : ChordType.ChordType
    , iii : ChordType.ChordType
    , iv : ChordType.ChordType
    , v : ChordType.ChordType
    , vi : ChordType.ChordType
    , vii : ChordType.ChordType
    , secondaryDominant : ChordType.ChordType
    }


triadsByDefault : DefaultChordTypes
triadsByDefault =
    { major =
        { i = ChordType.major
        , ii = ChordType.minor
        , iii = ChordType.minor
        , iv = ChordType.major
        , v = ChordType.major
        , vi = ChordType.minor
        , vii = ChordType.diminished
        , secondaryDominant = ChordType.major
        }
    , minor =
        { i = ChordType.minor
        , ii = ChordType.diminished
        , iii = ChordType.major
        , iv = ChordType.minor
        , v = ChordType.minor
        , vi = ChordType.major
        , vii = ChordType.major
        , secondaryDominant = ChordType.major
        }
    }


seventhsByDefault : DefaultChordTypes
seventhsByDefault =
    { major =
        { i = ChordType.majorSeventh
        , ii = ChordType.minorSeventh
        , iii = ChordType.minorSeventh
        , iv = ChordType.majorSeventh
        , v = ChordType.dominantSeventh
        , vi = ChordType.minorSeventh
        , vii = ChordType.halfDiminished
        , secondaryDominant = ChordType.dominantSeventh
        }
    , minor =
        { i = ChordType.minorSeventh
        , ii = ChordType.halfDiminished
        , iii = ChordType.majorSeventh
        , iv = ChordType.minorSeventh
        , v = ChordType.minorSeventh
        , vi = ChordType.majorSeventh
        , vii = ChordType.dominantSeventh
        , secondaryDominant = ChordType.dominantSeventh
        }
    }


defaultChordType : DefaultChordTypesForMode -> RomanNumeral -> ChordType.ChordType
defaultChordType defaultChordTypes numeral =
    case numeral of
        I ->
            .i defaultChordTypes

        II ->
            .ii defaultChordTypes

        III ->
            .iii defaultChordTypes

        IV ->
            .iv defaultChordTypes

        V ->
            .v defaultChordTypes

        VI ->
            .vi defaultChordTypes

        VII ->
            .vii defaultChordTypes


isDefaultChordType : List DefaultChordTypesForMode -> ChordType.ChordType -> RomanNumeral -> Bool
isDefaultChordType defaultChordTypes chordType numeral =
    let
        allDefaultChordTypes =
            case numeral of
                I ->
                    List.map .i defaultChordTypes

                II ->
                    List.map .ii defaultChordTypes

                III ->
                    List.map .iii defaultChordTypes

                IV ->
                    List.map .iv defaultChordTypes

                V ->
                    List.map .v defaultChordTypes

                VI ->
                    List.map .vi defaultChordTypes

                VII ->
                    List.map .vii defaultChordTypes
    in
    List.member chordType allDefaultChordTypes


maybeChordTypeFromContext : Key.Key -> RomanNumeral -> PitchClass.Offset -> ChordType.ChordType -> Maybe ChordType.ChordType
maybeChordTypeFromContext key numeral offset chordType =
    if offset /= PitchClass.offsetFromInt 0 then
        Just chordType

    else if Key.isMajor key then
        if isDefaultChordType [ triadsByDefault.major, seventhsByDefault.major ] chordType numeral then
            Nothing

        else
            Just chordType

    else if isDefaultChordType [ triadsByDefault.minor, seventhsByDefault.minor ] chordType numeral then
        Nothing

    else
        Just chordType


numeralAndOffset : Key.Key -> PitchClass.PitchClass -> ( RomanNumeral, PitchClass.Offset )
numeralAndOffset key root =
    Key.scale key
        |> Scale.toList
        |> List.indexedMap
            (\index pc ->
                ( index, pc )
            )
        |> List.filterMap
            (\( index, pc ) ->
                if PitchClass.letter pc == PitchClass.letter root then
                    Just
                        ( numeralFromInt index
                        , PitchClass.offsetFromInt
                            (PitchClass.offset root - PitchClass.offset pc)
                        )

                else
                    Nothing
            )
        |> List.head
        -- this won't happen
        |> Maybe.withDefault ( I, PitchClass.offsetFromInt 99 )


numeralFromInt : Int -> RomanNumeral
numeralFromInt int =
    case int of
        0 ->
            I

        1 ->
            II

        2 ->
            III

        3 ->
            IV

        4 ->
            V

        5 ->
            VI

        6 ->
            VII

        _ ->
            I
