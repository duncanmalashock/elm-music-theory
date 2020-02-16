module MusicTheory.Pitch exposing
    ( Pitch
    , PitchError(..)
    , a0
    , a1
    , a2
    , a3
    , a4
    , a5
    , a6
    , a7
    , a8
    , aDoubleFlat0
    , aDoubleFlat1
    , aDoubleFlat2
    , aDoubleFlat3
    , aDoubleFlat4
    , aDoubleFlat5
    , aDoubleFlat6
    , aDoubleFlat7
    , aDoubleFlat8
    , aDoubleSharp0
    , aDoubleSharp1
    , aDoubleSharp2
    , aDoubleSharp3
    , aDoubleSharp4
    , aDoubleSharp5
    , aDoubleSharp6
    , aDoubleSharp7
    , aDoubleSharp8
    , aFlat0
    , aFlat1
    , aFlat2
    , aFlat3
    , aFlat4
    , aFlat5
    , aFlat6
    , aFlat7
    , aFlat8
    , aSharp0
    , aSharp1
    , aSharp2
    , aSharp3
    , aSharp4
    , aSharp5
    , aSharp6
    , aSharp7
    , aSharp8
    , allForPitchClass
    , areEnharmonicEquivalents
    , b0
    , b1
    , b2
    , b3
    , b4
    , b5
    , b6
    , b7
    , b8
    , bDoubleFlat0
    , bDoubleFlat1
    , bDoubleFlat2
    , bDoubleFlat3
    , bDoubleFlat4
    , bDoubleFlat5
    , bDoubleFlat6
    , bDoubleFlat7
    , bDoubleFlat8
    , bDoubleSharp0
    , bDoubleSharp1
    , bDoubleSharp2
    , bDoubleSharp3
    , bDoubleSharp4
    , bDoubleSharp5
    , bDoubleSharp6
    , bDoubleSharp7
    , bFlat0
    , bFlat1
    , bFlat2
    , bFlat3
    , bFlat4
    , bFlat5
    , bFlat6
    , bFlat7
    , bFlat8
    , bSharp0
    , bSharp1
    , bSharp2
    , bSharp3
    , bSharp4
    , bSharp5
    , bSharp6
    , bSharp7
    , bSharp8
    , c0
    , c1
    , c2
    , c3
    , c4
    , c5
    , c6
    , c7
    , c8
    , cDoubleFlat1
    , cDoubleFlat2
    , cDoubleFlat3
    , cDoubleFlat4
    , cDoubleFlat5
    , cDoubleFlat6
    , cDoubleFlat7
    , cDoubleFlat8
    , cDoubleSharp0
    , cDoubleSharp1
    , cDoubleSharp2
    , cDoubleSharp3
    , cDoubleSharp4
    , cDoubleSharp5
    , cDoubleSharp6
    , cDoubleSharp7
    , cDoubleSharp8
    , cFlat1
    , cFlat2
    , cFlat3
    , cFlat4
    , cFlat5
    , cFlat6
    , cFlat7
    , cFlat8
    , cSharp0
    , cSharp1
    , cSharp2
    , cSharp3
    , cSharp4
    , cSharp5
    , cSharp6
    , cSharp7
    , cSharp8
    , d0
    , d1
    , d2
    , d3
    , d4
    , d5
    , d6
    , d7
    , d8
    , dDoubleFlat0
    , dDoubleFlat1
    , dDoubleFlat2
    , dDoubleFlat3
    , dDoubleFlat4
    , dDoubleFlat5
    , dDoubleFlat6
    , dDoubleFlat7
    , dDoubleFlat8
    , dDoubleSharp0
    , dDoubleSharp1
    , dDoubleSharp2
    , dDoubleSharp3
    , dDoubleSharp4
    , dDoubleSharp5
    , dDoubleSharp6
    , dDoubleSharp7
    , dDoubleSharp8
    , dFlat0
    , dFlat1
    , dFlat2
    , dFlat3
    , dFlat4
    , dFlat5
    , dFlat6
    , dFlat7
    , dFlat8
    , dSharp0
    , dSharp1
    , dSharp2
    , dSharp3
    , dSharp4
    , dSharp5
    , dSharp6
    , dSharp7
    , dSharp8
    , doubleFlat
    , doubleSharp
    , e0
    , e1
    , e2
    , e3
    , e4
    , e5
    , e6
    , e7
    , e8
    , eDoubleFlat0
    , eDoubleFlat1
    , eDoubleFlat2
    , eDoubleFlat3
    , eDoubleFlat4
    , eDoubleFlat5
    , eDoubleFlat6
    , eDoubleFlat7
    , eDoubleFlat8
    , eDoubleSharp0
    , eDoubleSharp1
    , eDoubleSharp2
    , eDoubleSharp3
    , eDoubleSharp4
    , eDoubleSharp5
    , eDoubleSharp6
    , eDoubleSharp7
    , eDoubleSharp8
    , eFlat0
    , eFlat1
    , eFlat2
    , eFlat3
    , eFlat4
    , eFlat5
    , eFlat6
    , eFlat7
    , eFlat8
    , eSharp0
    , eSharp1
    , eSharp2
    , eSharp3
    , eSharp4
    , eSharp5
    , eSharp6
    , eSharp7
    , eSharp8
    , errorToString
    , f0
    , f1
    , f2
    , f3
    , f4
    , f5
    , f6
    , f7
    , f8
    , fDoubleFlat0
    , fDoubleFlat1
    , fDoubleFlat2
    , fDoubleFlat3
    , fDoubleFlat4
    , fDoubleFlat5
    , fDoubleFlat6
    , fDoubleFlat7
    , fDoubleFlat8
    , fDoubleSharp0
    , fDoubleSharp1
    , fDoubleSharp2
    , fDoubleSharp3
    , fDoubleSharp4
    , fDoubleSharp5
    , fDoubleSharp6
    , fDoubleSharp7
    , fDoubleSharp8
    , fFlat0
    , fFlat1
    , fFlat2
    , fFlat3
    , fFlat4
    , fFlat5
    , fFlat6
    , fFlat7
    , fFlat8
    , fSharp0
    , fSharp1
    , fSharp2
    , fSharp3
    , fSharp4
    , fSharp5
    , fSharp6
    , fSharp7
    , fSharp8
    , firstAbove
    , firstBelow
    , flat
    , fromPitchClass
    , g0
    , g1
    , g2
    , g3
    , g4
    , g5
    , g6
    , g7
    , g8
    , gDoubleFlat0
    , gDoubleFlat1
    , gDoubleFlat2
    , gDoubleFlat3
    , gDoubleFlat4
    , gDoubleFlat5
    , gDoubleFlat6
    , gDoubleFlat7
    , gDoubleFlat8
    , gDoubleSharp0
    , gDoubleSharp1
    , gDoubleSharp2
    , gDoubleSharp3
    , gDoubleSharp4
    , gDoubleSharp5
    , gDoubleSharp6
    , gDoubleSharp7
    , gDoubleSharp8
    , gFlat0
    , gFlat1
    , gFlat2
    , gFlat3
    , gFlat4
    , gFlat5
    , gFlat6
    , gFlat7
    , gFlat8
    , gSharp0
    , gSharp1
    , gSharp2
    , gSharp3
    , gSharp4
    , gSharp5
    , gSharp6
    , gSharp7
    , gSharp8
    , natural
    , octave
    , pitch
    , pitchClass
    , semitones
    , sharp
    , sort
    , transposeDown
    , transposeUp
    , tripleFlat
    , tripleSharp
    )

import MusicTheory.Interval as Interval exposing (Interval)
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.Octave as Octave exposing (Octave, OctaveError(..))
import MusicTheory.PitchClass as PitchClass exposing (Offset, PitchClass)


semitonesUpperLimit : Int
semitonesUpperLimit =
    120


semitonesLowerLimit : Int
semitonesLowerLimit =
    0


type Pitch
    = Pitch PitchClass Octave


type PitchError
    = InvalidOctave OctaveError
    | SemitonesOutOfRange Int
    | ValidPitchNotFound
    | InternalError


pitch : Letter -> Offset -> Octave -> Result PitchError Pitch
pitch l os o =
    fromPitchClass o (PitchClass.pitchClass l os)


tripleFlat : Offset
tripleFlat =
    PitchClass.tripleFlat


doubleFlat : Offset
doubleFlat =
    PitchClass.doubleFlat


flat : Offset
flat =
    PitchClass.flat


natural : Offset
natural =
    PitchClass.natural


sharp : Offset
sharp =
    PitchClass.sharp


doubleSharp : Offset
doubleSharp =
    PitchClass.doubleSharp


tripleSharp : Offset
tripleSharp =
    PitchClass.tripleSharp


pitchClass : Pitch -> PitchClass
pitchClass (Pitch pc _) =
    pc


octave : Pitch -> Octave
octave (Pitch _ o) =
    o


fromPitchClass : Octave -> PitchClass -> Result PitchError Pitch
fromPitchClass o p =
    let
        newPitch =
            Pitch p o
    in
    if
        (semitones newPitch > semitonesUpperLimit)
            || (semitones newPitch < semitonesLowerLimit)
    then
        Err <| SemitonesOutOfRange (semitones newPitch)

    else
        Ok newPitch


sort : List Pitch -> List Pitch
sort pitchList =
    List.sortBy semitones pitchList


semitones : Pitch -> Int
semitones (Pitch pc o) =
    Octave.semitones o + PitchClass.semitones pc


areEnharmonicEquivalents : Pitch -> Pitch -> Bool
areEnharmonicEquivalents lhs rhs =
    semitones lhs == semitones rhs


allForPitchClass : PitchClass -> List Pitch
allForPitchClass thePitchClass =
    Octave.all
        |> List.map
            (\theOctave ->
                fromPitchClass theOctave thePitchClass
            )
        |> List.filterMap Result.toMaybe


firstBelow : PitchClass -> Pitch -> Result PitchError Pitch
firstBelow thePitchClass startPitch =
    let
        maybePitch =
            allForPitchClass thePitchClass
                |> List.filter
                    (\thePitch ->
                        semitones thePitch < semitones startPitch
                    )
                |> List.sortBy semitones
                |> List.reverse
                |> List.head
    in
    case maybePitch of
        Nothing ->
            Err ValidPitchNotFound

        Just thePitch ->
            Ok thePitch


firstAbove : PitchClass -> Pitch -> Result PitchError Pitch
firstAbove thePitchClass startPitch =
    let
        maybePitch =
            allForPitchClass thePitchClass
                |> List.filter
                    (\thePitch ->
                        semitones thePitch > semitones startPitch
                    )
                |> List.sortBy semitones
                |> List.head
    in
    case maybePitch of
        Nothing ->
            Err ValidPitchNotFound

        Just thePitch ->
            Ok thePitch


errorToString :
    PitchError
    -> String
errorToString error =
    case error of
        InvalidOctave err ->
            Octave.errorToString err

        SemitonesOutOfRange semis ->
            "Pitch semitone count"
                ++ String.fromInt semis
                ++ " is not between the legal range of "
                ++ String.fromInt semitonesLowerLimit
                ++ " and "
                ++ String.fromInt semitonesUpperLimit
                ++ "."

        ValidPitchNotFound ->
            "A valid pitch could not be constructed."

        InternalError ->
            "Something went wrong internally."


transposeUp :
    Interval
    -> Pitch
    -> Result PitchError Pitch
transposeUp =
    transpose PitchClass.transposeUp (+)


transposeDown :
    Interval
    -> Pitch
    -> Result PitchError Pitch
transposeDown =
    transpose PitchClass.transposeDown (-)


transpose :
    (Interval -> PitchClass -> PitchClass)
    -> (Int -> Int -> Int)
    -> Interval
    -> Pitch
    -> Result PitchError Pitch
transpose trans addIntervalSemitones interval p =
    let
        transposedPitchClass =
            pitchClass p
                |> trans interval

        targetOctaveSemitones =
            addIntervalSemitones
                (semitones p)
                (Interval.semitones interval)
                - PitchClass.semitones transposedPitchClass

        numberOfOctaves =
            targetOctaveSemitones // 12

        remainder =
            targetOctaveSemitones |> remainderBy 12
    in
    if remainder == 0 then
        Octave.octave numberOfOctaves
            |> Result.map (\o -> Pitch transposedPitchClass o)
            |> Result.mapError InvalidOctave

    else
        Err InternalError


c0 : Pitch
c0 =
    Pitch PitchClass.c Octave.zero


cSharp0 : Pitch
cSharp0 =
    Pitch PitchClass.cSharp Octave.zero


cDoubleSharp0 : Pitch
cDoubleSharp0 =
    Pitch PitchClass.cDoubleSharp Octave.zero


dDoubleSharp0 : Pitch
dDoubleSharp0 =
    Pitch PitchClass.dDoubleSharp Octave.zero


eDoubleSharp0 : Pitch
eDoubleSharp0 =
    Pitch PitchClass.eDoubleSharp Octave.zero


fDoubleSharp0 : Pitch
fDoubleSharp0 =
    Pitch PitchClass.fDoubleSharp Octave.zero


gDoubleSharp0 : Pitch
gDoubleSharp0 =
    Pitch PitchClass.gDoubleSharp Octave.zero


aDoubleSharp0 : Pitch
aDoubleSharp0 =
    Pitch PitchClass.aDoubleSharp Octave.zero


bDoubleSharp0 : Pitch
bDoubleSharp0 =
    Pitch PitchClass.bDoubleSharp Octave.zero


cDoubleSharp1 : Pitch
cDoubleSharp1 =
    Pitch PitchClass.cDoubleSharp Octave.one


dDoubleSharp1 : Pitch
dDoubleSharp1 =
    Pitch PitchClass.dDoubleSharp Octave.one


eDoubleSharp1 : Pitch
eDoubleSharp1 =
    Pitch PitchClass.eDoubleSharp Octave.one


fDoubleSharp1 : Pitch
fDoubleSharp1 =
    Pitch PitchClass.fDoubleSharp Octave.one


gDoubleSharp1 : Pitch
gDoubleSharp1 =
    Pitch PitchClass.gDoubleSharp Octave.one


aDoubleSharp1 : Pitch
aDoubleSharp1 =
    Pitch PitchClass.aDoubleSharp Octave.one


bDoubleSharp1 : Pitch
bDoubleSharp1 =
    Pitch PitchClass.bDoubleSharp Octave.one


cDoubleSharp2 : Pitch
cDoubleSharp2 =
    Pitch PitchClass.cDoubleSharp Octave.two


dDoubleSharp2 : Pitch
dDoubleSharp2 =
    Pitch PitchClass.dDoubleSharp Octave.two


eDoubleSharp2 : Pitch
eDoubleSharp2 =
    Pitch PitchClass.eDoubleSharp Octave.two


fDoubleSharp2 : Pitch
fDoubleSharp2 =
    Pitch PitchClass.fDoubleSharp Octave.two


gDoubleSharp2 : Pitch
gDoubleSharp2 =
    Pitch PitchClass.gDoubleSharp Octave.two


aDoubleSharp2 : Pitch
aDoubleSharp2 =
    Pitch PitchClass.aDoubleSharp Octave.two


bDoubleSharp2 : Pitch
bDoubleSharp2 =
    Pitch PitchClass.bDoubleSharp Octave.two


cDoubleSharp3 : Pitch
cDoubleSharp3 =
    Pitch PitchClass.cDoubleSharp Octave.three


dDoubleSharp3 : Pitch
dDoubleSharp3 =
    Pitch PitchClass.dDoubleSharp Octave.three


eDoubleSharp3 : Pitch
eDoubleSharp3 =
    Pitch PitchClass.eDoubleSharp Octave.three


fDoubleSharp3 : Pitch
fDoubleSharp3 =
    Pitch PitchClass.fDoubleSharp Octave.three


gDoubleSharp3 : Pitch
gDoubleSharp3 =
    Pitch PitchClass.gDoubleSharp Octave.three


aDoubleSharp3 : Pitch
aDoubleSharp3 =
    Pitch PitchClass.aDoubleSharp Octave.three


bDoubleSharp3 : Pitch
bDoubleSharp3 =
    Pitch PitchClass.bDoubleSharp Octave.three


cDoubleSharp4 : Pitch
cDoubleSharp4 =
    Pitch PitchClass.cDoubleSharp Octave.four


dDoubleSharp4 : Pitch
dDoubleSharp4 =
    Pitch PitchClass.dDoubleSharp Octave.four


eDoubleSharp4 : Pitch
eDoubleSharp4 =
    Pitch PitchClass.eDoubleSharp Octave.four


fDoubleSharp4 : Pitch
fDoubleSharp4 =
    Pitch PitchClass.fDoubleSharp Octave.four


gDoubleSharp4 : Pitch
gDoubleSharp4 =
    Pitch PitchClass.gDoubleSharp Octave.four


aDoubleSharp4 : Pitch
aDoubleSharp4 =
    Pitch PitchClass.aDoubleSharp Octave.four


bDoubleSharp4 : Pitch
bDoubleSharp4 =
    Pitch PitchClass.bDoubleSharp Octave.four


cDoubleSharp5 : Pitch
cDoubleSharp5 =
    Pitch PitchClass.cDoubleSharp Octave.five


dDoubleSharp5 : Pitch
dDoubleSharp5 =
    Pitch PitchClass.dDoubleSharp Octave.five


eDoubleSharp5 : Pitch
eDoubleSharp5 =
    Pitch PitchClass.eDoubleSharp Octave.five


fDoubleSharp5 : Pitch
fDoubleSharp5 =
    Pitch PitchClass.fDoubleSharp Octave.five


gDoubleSharp5 : Pitch
gDoubleSharp5 =
    Pitch PitchClass.gDoubleSharp Octave.five


aDoubleSharp5 : Pitch
aDoubleSharp5 =
    Pitch PitchClass.aDoubleSharp Octave.five


bDoubleSharp5 : Pitch
bDoubleSharp5 =
    Pitch PitchClass.bDoubleSharp Octave.five


cDoubleSharp6 : Pitch
cDoubleSharp6 =
    Pitch PitchClass.cDoubleSharp Octave.six


dDoubleSharp6 : Pitch
dDoubleSharp6 =
    Pitch PitchClass.dDoubleSharp Octave.six


eDoubleSharp6 : Pitch
eDoubleSharp6 =
    Pitch PitchClass.eDoubleSharp Octave.six


fDoubleSharp6 : Pitch
fDoubleSharp6 =
    Pitch PitchClass.fDoubleSharp Octave.six


gDoubleSharp6 : Pitch
gDoubleSharp6 =
    Pitch PitchClass.gDoubleSharp Octave.six


aDoubleSharp6 : Pitch
aDoubleSharp6 =
    Pitch PitchClass.aDoubleSharp Octave.six


bDoubleSharp6 : Pitch
bDoubleSharp6 =
    Pitch PitchClass.bDoubleSharp Octave.six


cDoubleSharp7 : Pitch
cDoubleSharp7 =
    Pitch PitchClass.cDoubleSharp Octave.seven


dDoubleSharp7 : Pitch
dDoubleSharp7 =
    Pitch PitchClass.dDoubleSharp Octave.seven


eDoubleSharp7 : Pitch
eDoubleSharp7 =
    Pitch PitchClass.eDoubleSharp Octave.seven


fDoubleSharp7 : Pitch
fDoubleSharp7 =
    Pitch PitchClass.fDoubleSharp Octave.seven


gDoubleSharp7 : Pitch
gDoubleSharp7 =
    Pitch PitchClass.gDoubleSharp Octave.seven


aDoubleSharp7 : Pitch
aDoubleSharp7 =
    Pitch PitchClass.aDoubleSharp Octave.seven


bDoubleSharp7 : Pitch
bDoubleSharp7 =
    Pitch PitchClass.bDoubleSharp Octave.seven


cDoubleSharp8 : Pitch
cDoubleSharp8 =
    Pitch PitchClass.cDoubleSharp Octave.eight


dDoubleSharp8 : Pitch
dDoubleSharp8 =
    Pitch PitchClass.dDoubleSharp Octave.eight


eDoubleSharp8 : Pitch
eDoubleSharp8 =
    Pitch PitchClass.eDoubleSharp Octave.eight


fDoubleSharp8 : Pitch
fDoubleSharp8 =
    Pitch PitchClass.fDoubleSharp Octave.eight


gDoubleSharp8 : Pitch
gDoubleSharp8 =
    Pitch PitchClass.gDoubleSharp Octave.eight


aDoubleSharp8 : Pitch
aDoubleSharp8 =
    Pitch PitchClass.aDoubleSharp Octave.eight


dDoubleFlat0 : Pitch
dDoubleFlat0 =
    Pitch PitchClass.dDoubleFlat Octave.zero


eDoubleFlat0 : Pitch
eDoubleFlat0 =
    Pitch PitchClass.eDoubleFlat Octave.zero


fDoubleFlat0 : Pitch
fDoubleFlat0 =
    Pitch PitchClass.fDoubleFlat Octave.zero


gDoubleFlat0 : Pitch
gDoubleFlat0 =
    Pitch PitchClass.gDoubleFlat Octave.zero


aDoubleFlat0 : Pitch
aDoubleFlat0 =
    Pitch PitchClass.aDoubleFlat Octave.zero


bDoubleFlat0 : Pitch
bDoubleFlat0 =
    Pitch PitchClass.bDoubleFlat Octave.zero


cDoubleFlat1 : Pitch
cDoubleFlat1 =
    Pitch PitchClass.cDoubleFlat Octave.one


dDoubleFlat1 : Pitch
dDoubleFlat1 =
    Pitch PitchClass.dDoubleFlat Octave.one


eDoubleFlat1 : Pitch
eDoubleFlat1 =
    Pitch PitchClass.eDoubleFlat Octave.one


fDoubleFlat1 : Pitch
fDoubleFlat1 =
    Pitch PitchClass.fDoubleFlat Octave.one


gDoubleFlat1 : Pitch
gDoubleFlat1 =
    Pitch PitchClass.gDoubleFlat Octave.one


aDoubleFlat1 : Pitch
aDoubleFlat1 =
    Pitch PitchClass.aDoubleFlat Octave.one


bDoubleFlat1 : Pitch
bDoubleFlat1 =
    Pitch PitchClass.bDoubleFlat Octave.one


cDoubleFlat2 : Pitch
cDoubleFlat2 =
    Pitch PitchClass.cDoubleFlat Octave.two


dDoubleFlat2 : Pitch
dDoubleFlat2 =
    Pitch PitchClass.dDoubleFlat Octave.two


eDoubleFlat2 : Pitch
eDoubleFlat2 =
    Pitch PitchClass.eDoubleFlat Octave.two


fDoubleFlat2 : Pitch
fDoubleFlat2 =
    Pitch PitchClass.fDoubleFlat Octave.two


gDoubleFlat2 : Pitch
gDoubleFlat2 =
    Pitch PitchClass.gDoubleFlat Octave.two


aDoubleFlat2 : Pitch
aDoubleFlat2 =
    Pitch PitchClass.aDoubleFlat Octave.two


bDoubleFlat2 : Pitch
bDoubleFlat2 =
    Pitch PitchClass.bDoubleFlat Octave.two


cDoubleFlat3 : Pitch
cDoubleFlat3 =
    Pitch PitchClass.cDoubleFlat Octave.three


dDoubleFlat3 : Pitch
dDoubleFlat3 =
    Pitch PitchClass.dDoubleFlat Octave.three


eDoubleFlat3 : Pitch
eDoubleFlat3 =
    Pitch PitchClass.eDoubleFlat Octave.three


fDoubleFlat3 : Pitch
fDoubleFlat3 =
    Pitch PitchClass.fDoubleFlat Octave.three


gDoubleFlat3 : Pitch
gDoubleFlat3 =
    Pitch PitchClass.gDoubleFlat Octave.three


aDoubleFlat3 : Pitch
aDoubleFlat3 =
    Pitch PitchClass.aDoubleFlat Octave.three


bDoubleFlat3 : Pitch
bDoubleFlat3 =
    Pitch PitchClass.bDoubleFlat Octave.three


cDoubleFlat4 : Pitch
cDoubleFlat4 =
    Pitch PitchClass.cDoubleFlat Octave.four


dDoubleFlat4 : Pitch
dDoubleFlat4 =
    Pitch PitchClass.dDoubleFlat Octave.four


eDoubleFlat4 : Pitch
eDoubleFlat4 =
    Pitch PitchClass.eDoubleFlat Octave.four


fDoubleFlat4 : Pitch
fDoubleFlat4 =
    Pitch PitchClass.fDoubleFlat Octave.four


gDoubleFlat4 : Pitch
gDoubleFlat4 =
    Pitch PitchClass.gDoubleFlat Octave.four


aDoubleFlat4 : Pitch
aDoubleFlat4 =
    Pitch PitchClass.aDoubleFlat Octave.four


bDoubleFlat4 : Pitch
bDoubleFlat4 =
    Pitch PitchClass.bDoubleFlat Octave.four


cDoubleFlat5 : Pitch
cDoubleFlat5 =
    Pitch PitchClass.cDoubleFlat Octave.five


dDoubleFlat5 : Pitch
dDoubleFlat5 =
    Pitch PitchClass.dDoubleFlat Octave.five


eDoubleFlat5 : Pitch
eDoubleFlat5 =
    Pitch PitchClass.eDoubleFlat Octave.five


fDoubleFlat5 : Pitch
fDoubleFlat5 =
    Pitch PitchClass.fDoubleFlat Octave.five


gDoubleFlat5 : Pitch
gDoubleFlat5 =
    Pitch PitchClass.gDoubleFlat Octave.five


aDoubleFlat5 : Pitch
aDoubleFlat5 =
    Pitch PitchClass.aDoubleFlat Octave.five


bDoubleFlat5 : Pitch
bDoubleFlat5 =
    Pitch PitchClass.bDoubleFlat Octave.five


cDoubleFlat6 : Pitch
cDoubleFlat6 =
    Pitch PitchClass.cDoubleFlat Octave.six


dDoubleFlat6 : Pitch
dDoubleFlat6 =
    Pitch PitchClass.dDoubleFlat Octave.six


eDoubleFlat6 : Pitch
eDoubleFlat6 =
    Pitch PitchClass.eDoubleFlat Octave.six


fDoubleFlat6 : Pitch
fDoubleFlat6 =
    Pitch PitchClass.fDoubleFlat Octave.six


gDoubleFlat6 : Pitch
gDoubleFlat6 =
    Pitch PitchClass.gDoubleFlat Octave.six


aDoubleFlat6 : Pitch
aDoubleFlat6 =
    Pitch PitchClass.aDoubleFlat Octave.six


bDoubleFlat6 : Pitch
bDoubleFlat6 =
    Pitch PitchClass.bDoubleFlat Octave.six


cDoubleFlat7 : Pitch
cDoubleFlat7 =
    Pitch PitchClass.cDoubleFlat Octave.seven


dDoubleFlat7 : Pitch
dDoubleFlat7 =
    Pitch PitchClass.dDoubleFlat Octave.seven


eDoubleFlat7 : Pitch
eDoubleFlat7 =
    Pitch PitchClass.eDoubleFlat Octave.seven


fDoubleFlat7 : Pitch
fDoubleFlat7 =
    Pitch PitchClass.fDoubleFlat Octave.seven


gDoubleFlat7 : Pitch
gDoubleFlat7 =
    Pitch PitchClass.gDoubleFlat Octave.seven


aDoubleFlat7 : Pitch
aDoubleFlat7 =
    Pitch PitchClass.aDoubleFlat Octave.seven


bDoubleFlat7 : Pitch
bDoubleFlat7 =
    Pitch PitchClass.bDoubleFlat Octave.seven


cDoubleFlat8 : Pitch
cDoubleFlat8 =
    Pitch PitchClass.cDoubleFlat Octave.eight


dDoubleFlat8 : Pitch
dDoubleFlat8 =
    Pitch PitchClass.dDoubleFlat Octave.eight


eDoubleFlat8 : Pitch
eDoubleFlat8 =
    Pitch PitchClass.eDoubleFlat Octave.eight


fDoubleFlat8 : Pitch
fDoubleFlat8 =
    Pitch PitchClass.fDoubleFlat Octave.eight


gDoubleFlat8 : Pitch
gDoubleFlat8 =
    Pitch PitchClass.gDoubleFlat Octave.eight


aDoubleFlat8 : Pitch
aDoubleFlat8 =
    Pitch PitchClass.aDoubleFlat Octave.eight


bDoubleFlat8 : Pitch
bDoubleFlat8 =
    Pitch PitchClass.bDoubleFlat Octave.eight


d0 : Pitch
d0 =
    Pitch PitchClass.d Octave.zero


dSharp0 : Pitch
dSharp0 =
    Pitch PitchClass.dSharp Octave.zero


dFlat0 : Pitch
dFlat0 =
    Pitch PitchClass.dFlat Octave.zero


e0 : Pitch
e0 =
    Pitch PitchClass.e Octave.zero


eSharp0 : Pitch
eSharp0 =
    Pitch PitchClass.eSharp Octave.zero


eFlat0 : Pitch
eFlat0 =
    Pitch PitchClass.eFlat Octave.zero


f0 : Pitch
f0 =
    Pitch PitchClass.f Octave.zero


fSharp0 : Pitch
fSharp0 =
    Pitch PitchClass.fSharp Octave.zero


fFlat0 : Pitch
fFlat0 =
    Pitch PitchClass.fFlat Octave.zero


g0 : Pitch
g0 =
    Pitch PitchClass.g Octave.zero


gSharp0 : Pitch
gSharp0 =
    Pitch PitchClass.gSharp Octave.zero


gFlat0 : Pitch
gFlat0 =
    Pitch PitchClass.gFlat Octave.zero


a0 : Pitch
a0 =
    Pitch PitchClass.a Octave.zero


aSharp0 : Pitch
aSharp0 =
    Pitch PitchClass.aSharp Octave.zero


aFlat0 : Pitch
aFlat0 =
    Pitch PitchClass.aFlat Octave.zero


b0 : Pitch
b0 =
    Pitch PitchClass.b Octave.zero


bSharp0 : Pitch
bSharp0 =
    Pitch PitchClass.bSharp Octave.zero


bFlat0 : Pitch
bFlat0 =
    Pitch PitchClass.bFlat Octave.zero


c1 : Pitch
c1 =
    Pitch PitchClass.c Octave.one


cSharp1 : Pitch
cSharp1 =
    Pitch PitchClass.cSharp Octave.one


cFlat1 : Pitch
cFlat1 =
    Pitch PitchClass.cFlat Octave.one


d1 : Pitch
d1 =
    Pitch PitchClass.d Octave.one


dSharp1 : Pitch
dSharp1 =
    Pitch PitchClass.dSharp Octave.one


dFlat1 : Pitch
dFlat1 =
    Pitch PitchClass.dFlat Octave.one


e1 : Pitch
e1 =
    Pitch PitchClass.e Octave.one


eSharp1 : Pitch
eSharp1 =
    Pitch PitchClass.eSharp Octave.one


eFlat1 : Pitch
eFlat1 =
    Pitch PitchClass.eFlat Octave.one


f1 : Pitch
f1 =
    Pitch PitchClass.f Octave.one


fSharp1 : Pitch
fSharp1 =
    Pitch PitchClass.fSharp Octave.one


fFlat1 : Pitch
fFlat1 =
    Pitch PitchClass.fFlat Octave.one


g1 : Pitch
g1 =
    Pitch PitchClass.g Octave.one


gSharp1 : Pitch
gSharp1 =
    Pitch PitchClass.gSharp Octave.one


gFlat1 : Pitch
gFlat1 =
    Pitch PitchClass.gFlat Octave.one


a1 : Pitch
a1 =
    Pitch PitchClass.a Octave.one


aSharp1 : Pitch
aSharp1 =
    Pitch PitchClass.aSharp Octave.one


aFlat1 : Pitch
aFlat1 =
    Pitch PitchClass.aFlat Octave.one


b1 : Pitch
b1 =
    Pitch PitchClass.b Octave.one


bSharp1 : Pitch
bSharp1 =
    Pitch PitchClass.bSharp Octave.one


bFlat1 : Pitch
bFlat1 =
    Pitch PitchClass.bFlat Octave.one


c2 : Pitch
c2 =
    Pitch PitchClass.c Octave.two


cSharp2 : Pitch
cSharp2 =
    Pitch PitchClass.cSharp Octave.two


cFlat2 : Pitch
cFlat2 =
    Pitch PitchClass.cFlat Octave.two


d2 : Pitch
d2 =
    Pitch PitchClass.d Octave.two


dSharp2 : Pitch
dSharp2 =
    Pitch PitchClass.dSharp Octave.two


dFlat2 : Pitch
dFlat2 =
    Pitch PitchClass.dFlat Octave.two


e2 : Pitch
e2 =
    Pitch PitchClass.e Octave.two


eSharp2 : Pitch
eSharp2 =
    Pitch PitchClass.eSharp Octave.two


eFlat2 : Pitch
eFlat2 =
    Pitch PitchClass.eFlat Octave.two


f2 : Pitch
f2 =
    Pitch PitchClass.f Octave.two


fSharp2 : Pitch
fSharp2 =
    Pitch PitchClass.fSharp Octave.two


fFlat2 : Pitch
fFlat2 =
    Pitch PitchClass.fFlat Octave.two


g2 : Pitch
g2 =
    Pitch PitchClass.g Octave.two


gSharp2 : Pitch
gSharp2 =
    Pitch PitchClass.gSharp Octave.two


gFlat2 : Pitch
gFlat2 =
    Pitch PitchClass.gFlat Octave.two


a2 : Pitch
a2 =
    Pitch PitchClass.a Octave.two


aSharp2 : Pitch
aSharp2 =
    Pitch PitchClass.aSharp Octave.two


aFlat2 : Pitch
aFlat2 =
    Pitch PitchClass.aFlat Octave.two


b2 : Pitch
b2 =
    Pitch PitchClass.b Octave.two


bSharp2 : Pitch
bSharp2 =
    Pitch PitchClass.bSharp Octave.two


bFlat2 : Pitch
bFlat2 =
    Pitch PitchClass.bFlat Octave.two


c3 : Pitch
c3 =
    Pitch PitchClass.c Octave.three


cSharp3 : Pitch
cSharp3 =
    Pitch PitchClass.cSharp Octave.three


cFlat3 : Pitch
cFlat3 =
    Pitch PitchClass.cFlat Octave.three


d3 : Pitch
d3 =
    Pitch PitchClass.d Octave.three


dSharp3 : Pitch
dSharp3 =
    Pitch PitchClass.dSharp Octave.three


dFlat3 : Pitch
dFlat3 =
    Pitch PitchClass.dFlat Octave.three


e3 : Pitch
e3 =
    Pitch PitchClass.e Octave.three


eSharp3 : Pitch
eSharp3 =
    Pitch PitchClass.eSharp Octave.three


eFlat3 : Pitch
eFlat3 =
    Pitch PitchClass.eFlat Octave.three


f3 : Pitch
f3 =
    Pitch PitchClass.f Octave.three


fSharp3 : Pitch
fSharp3 =
    Pitch PitchClass.fSharp Octave.three


fFlat3 : Pitch
fFlat3 =
    Pitch PitchClass.fFlat Octave.three


g3 : Pitch
g3 =
    Pitch PitchClass.g Octave.three


gSharp3 : Pitch
gSharp3 =
    Pitch PitchClass.gSharp Octave.three


gFlat3 : Pitch
gFlat3 =
    Pitch PitchClass.gFlat Octave.three


a3 : Pitch
a3 =
    Pitch PitchClass.a Octave.three


aSharp3 : Pitch
aSharp3 =
    Pitch PitchClass.aSharp Octave.three


aFlat3 : Pitch
aFlat3 =
    Pitch PitchClass.aFlat Octave.three


b3 : Pitch
b3 =
    Pitch PitchClass.b Octave.three


bSharp3 : Pitch
bSharp3 =
    Pitch PitchClass.bSharp Octave.three


bFlat3 : Pitch
bFlat3 =
    Pitch PitchClass.bFlat Octave.three


c4 : Pitch
c4 =
    Pitch PitchClass.c Octave.four


cSharp4 : Pitch
cSharp4 =
    Pitch PitchClass.cSharp Octave.four


cFlat4 : Pitch
cFlat4 =
    Pitch PitchClass.cFlat Octave.four


d4 : Pitch
d4 =
    Pitch PitchClass.d Octave.four


dSharp4 : Pitch
dSharp4 =
    Pitch PitchClass.dSharp Octave.four


dFlat4 : Pitch
dFlat4 =
    Pitch PitchClass.dFlat Octave.four


e4 : Pitch
e4 =
    Pitch PitchClass.e Octave.four


eSharp4 : Pitch
eSharp4 =
    Pitch PitchClass.eSharp Octave.four


eFlat4 : Pitch
eFlat4 =
    Pitch PitchClass.eFlat Octave.four


f4 : Pitch
f4 =
    Pitch PitchClass.f Octave.four


fSharp4 : Pitch
fSharp4 =
    Pitch PitchClass.fSharp Octave.four


fFlat4 : Pitch
fFlat4 =
    Pitch PitchClass.fFlat Octave.four


g4 : Pitch
g4 =
    Pitch PitchClass.g Octave.four


gSharp4 : Pitch
gSharp4 =
    Pitch PitchClass.gSharp Octave.four


gFlat4 : Pitch
gFlat4 =
    Pitch PitchClass.gFlat Octave.four


a4 : Pitch
a4 =
    Pitch PitchClass.a Octave.four


aSharp4 : Pitch
aSharp4 =
    Pitch PitchClass.aSharp Octave.four


aFlat4 : Pitch
aFlat4 =
    Pitch PitchClass.aFlat Octave.four


b4 : Pitch
b4 =
    Pitch PitchClass.b Octave.four


bSharp4 : Pitch
bSharp4 =
    Pitch PitchClass.bSharp Octave.four


bFlat4 : Pitch
bFlat4 =
    Pitch PitchClass.bFlat Octave.four


c5 : Pitch
c5 =
    Pitch PitchClass.c Octave.five


cSharp5 : Pitch
cSharp5 =
    Pitch PitchClass.cSharp Octave.five


cFlat5 : Pitch
cFlat5 =
    Pitch PitchClass.cFlat Octave.five


d5 : Pitch
d5 =
    Pitch PitchClass.d Octave.five


dSharp5 : Pitch
dSharp5 =
    Pitch PitchClass.dSharp Octave.five


dFlat5 : Pitch
dFlat5 =
    Pitch PitchClass.dFlat Octave.five


e5 : Pitch
e5 =
    Pitch PitchClass.e Octave.five


eSharp5 : Pitch
eSharp5 =
    Pitch PitchClass.eSharp Octave.five


eFlat5 : Pitch
eFlat5 =
    Pitch PitchClass.eFlat Octave.five


f5 : Pitch
f5 =
    Pitch PitchClass.f Octave.five


fSharp5 : Pitch
fSharp5 =
    Pitch PitchClass.fSharp Octave.five


fFlat5 : Pitch
fFlat5 =
    Pitch PitchClass.fFlat Octave.five


g5 : Pitch
g5 =
    Pitch PitchClass.g Octave.five


gSharp5 : Pitch
gSharp5 =
    Pitch PitchClass.gSharp Octave.five


gFlat5 : Pitch
gFlat5 =
    Pitch PitchClass.gFlat Octave.five


a5 : Pitch
a5 =
    Pitch PitchClass.a Octave.five


aSharp5 : Pitch
aSharp5 =
    Pitch PitchClass.aSharp Octave.five


aFlat5 : Pitch
aFlat5 =
    Pitch PitchClass.aFlat Octave.five


b5 : Pitch
b5 =
    Pitch PitchClass.b Octave.five


bSharp5 : Pitch
bSharp5 =
    Pitch PitchClass.bSharp Octave.five


bFlat5 : Pitch
bFlat5 =
    Pitch PitchClass.bFlat Octave.five


c6 : Pitch
c6 =
    Pitch PitchClass.c Octave.six


cSharp6 : Pitch
cSharp6 =
    Pitch PitchClass.cSharp Octave.six


cFlat6 : Pitch
cFlat6 =
    Pitch PitchClass.cFlat Octave.six


d6 : Pitch
d6 =
    Pitch PitchClass.d Octave.six


dSharp6 : Pitch
dSharp6 =
    Pitch PitchClass.dSharp Octave.six


dFlat6 : Pitch
dFlat6 =
    Pitch PitchClass.dFlat Octave.six


e6 : Pitch
e6 =
    Pitch PitchClass.e Octave.six


eSharp6 : Pitch
eSharp6 =
    Pitch PitchClass.eSharp Octave.six


eFlat6 : Pitch
eFlat6 =
    Pitch PitchClass.eFlat Octave.six


f6 : Pitch
f6 =
    Pitch PitchClass.f Octave.six


fSharp6 : Pitch
fSharp6 =
    Pitch PitchClass.fSharp Octave.six


fFlat6 : Pitch
fFlat6 =
    Pitch PitchClass.fFlat Octave.six


g6 : Pitch
g6 =
    Pitch PitchClass.g Octave.six


gSharp6 : Pitch
gSharp6 =
    Pitch PitchClass.gSharp Octave.six


gFlat6 : Pitch
gFlat6 =
    Pitch PitchClass.gFlat Octave.six


a6 : Pitch
a6 =
    Pitch PitchClass.a Octave.six


aSharp6 : Pitch
aSharp6 =
    Pitch PitchClass.aSharp Octave.six


aFlat6 : Pitch
aFlat6 =
    Pitch PitchClass.aFlat Octave.six


b6 : Pitch
b6 =
    Pitch PitchClass.b Octave.six


bSharp6 : Pitch
bSharp6 =
    Pitch PitchClass.bSharp Octave.six


bFlat6 : Pitch
bFlat6 =
    Pitch PitchClass.bFlat Octave.six


c7 : Pitch
c7 =
    Pitch PitchClass.c Octave.seven


cSharp7 : Pitch
cSharp7 =
    Pitch PitchClass.cSharp Octave.seven


cFlat7 : Pitch
cFlat7 =
    Pitch PitchClass.cFlat Octave.seven


d7 : Pitch
d7 =
    Pitch PitchClass.d Octave.seven


dSharp7 : Pitch
dSharp7 =
    Pitch PitchClass.dSharp Octave.seven


dFlat7 : Pitch
dFlat7 =
    Pitch PitchClass.dFlat Octave.seven


e7 : Pitch
e7 =
    Pitch PitchClass.e Octave.seven


eSharp7 : Pitch
eSharp7 =
    Pitch PitchClass.eSharp Octave.seven


eFlat7 : Pitch
eFlat7 =
    Pitch PitchClass.eFlat Octave.seven


f7 : Pitch
f7 =
    Pitch PitchClass.f Octave.seven


fSharp7 : Pitch
fSharp7 =
    Pitch PitchClass.fSharp Octave.seven


fFlat7 : Pitch
fFlat7 =
    Pitch PitchClass.fFlat Octave.seven


g7 : Pitch
g7 =
    Pitch PitchClass.g Octave.seven


gSharp7 : Pitch
gSharp7 =
    Pitch PitchClass.gSharp Octave.seven


gFlat7 : Pitch
gFlat7 =
    Pitch PitchClass.gFlat Octave.seven


a7 : Pitch
a7 =
    Pitch PitchClass.a Octave.seven


aSharp7 : Pitch
aSharp7 =
    Pitch PitchClass.aSharp Octave.seven


aFlat7 : Pitch
aFlat7 =
    Pitch PitchClass.aFlat Octave.seven


b7 : Pitch
b7 =
    Pitch PitchClass.b Octave.seven


bSharp7 : Pitch
bSharp7 =
    Pitch PitchClass.bSharp Octave.seven


bFlat7 : Pitch
bFlat7 =
    Pitch PitchClass.bFlat Octave.seven


c8 : Pitch
c8 =
    Pitch PitchClass.c Octave.eight


cSharp8 : Pitch
cSharp8 =
    Pitch PitchClass.cSharp Octave.eight


cFlat8 : Pitch
cFlat8 =
    Pitch PitchClass.cFlat Octave.eight


d8 : Pitch
d8 =
    Pitch PitchClass.d Octave.eight


dSharp8 : Pitch
dSharp8 =
    Pitch PitchClass.dSharp Octave.eight


dFlat8 : Pitch
dFlat8 =
    Pitch PitchClass.dFlat Octave.eight


e8 : Pitch
e8 =
    Pitch PitchClass.e Octave.eight


eSharp8 : Pitch
eSharp8 =
    Pitch PitchClass.eSharp Octave.eight


eFlat8 : Pitch
eFlat8 =
    Pitch PitchClass.eFlat Octave.eight


f8 : Pitch
f8 =
    Pitch PitchClass.f Octave.eight


fSharp8 : Pitch
fSharp8 =
    Pitch PitchClass.fSharp Octave.eight


fFlat8 : Pitch
fFlat8 =
    Pitch PitchClass.fFlat Octave.eight


g8 : Pitch
g8 =
    Pitch PitchClass.g Octave.eight


gSharp8 : Pitch
gSharp8 =
    Pitch PitchClass.gSharp Octave.eight


gFlat8 : Pitch
gFlat8 =
    Pitch PitchClass.gFlat Octave.eight


a8 : Pitch
a8 =
    Pitch PitchClass.a Octave.eight


aSharp8 : Pitch
aSharp8 =
    Pitch PitchClass.aSharp Octave.eight


aFlat8 : Pitch
aFlat8 =
    Pitch PitchClass.aFlat Octave.eight


b8 : Pitch
b8 =
    Pitch PitchClass.b Octave.eight


bSharp8 : Pitch
bSharp8 =
    Pitch PitchClass.bSharp Octave.eight


bFlat8 : Pitch
bFlat8 =
    Pitch PitchClass.bFlat Octave.eight
