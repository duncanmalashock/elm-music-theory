module Music.Internal.Key exposing
    ( Key
    , MajorOrMinor(..)
    , a
    , aFlat
    , aMinor
    , b
    , bFlat
    , bFlatMinor
    , bMinor
    , c
    , cMinor
    , cSharpMinor
    , d
    , dFlat
    , dMinor
    , e
    , eFlat
    , eFlatMinor
    , eMinor
    , f
    , fMinor
    , fSharp
    , fSharpMinor
    , g
    , gFlat
    , gMinor
    , gSharpMinor
    , isMajor
    , isMinor
    , major
    , majorKey
    , minor
    , minorKey
    , relative
    , scale
    , signature
    , symbol
    , tonic
    )

import Music.Internal.Interval as Interval
import Music.Internal.Letter exposing (Letter(..))
import Music.Internal.PitchClass as PitchClass exposing (PitchClass, pitchClass)
import Music.Internal.Scale as Scale exposing (Scale)
import Music.Internal.ScaleType as ScaleType


type Key
    = Key PitchClass MajorOrMinor


type MajorOrMinor
    = Major
    | Minor


majorKey : MajorOrMinor
majorKey =
    Major


minorKey : MajorOrMinor
minorKey =
    Minor


relative : Key -> Key
relative (Key pc majorOrMinor) =
    case majorOrMinor of
        Minor ->
            Key
                (PitchClass.transpose
                    Interval.minorThird
                    pc
                )
                Major

        Major ->
            Key
                (PitchClass.transpose
                    (Interval.minorThird
                        |> Interval.reverse
                    )
                    pc
                )
                Minor


signature : Key -> List PitchClass.PitchClass
signature theKey =
    scale theKey
        |> Scale.toList
        |> List.filter (\pc -> PitchClass.accidentals pc /= 0)
        |> List.sortBy orderByAppearanceInKeySignature


orderByAppearanceInKeySignature : PitchClass.PitchClass -> Int
orderByAppearanceInKeySignature pitchClass =
    if pitchClass == PitchClass.bFlat then
        0

    else if pitchClass == PitchClass.eFlat then
        1

    else if pitchClass == PitchClass.aFlat then
        2

    else if pitchClass == PitchClass.dFlat then
        3

    else if pitchClass == PitchClass.gFlat then
        4

    else if pitchClass == PitchClass.fFlat then
        5

    else if pitchClass == PitchClass.fSharp then
        6

    else if pitchClass == PitchClass.cSharp then
        7

    else if pitchClass == PitchClass.gSharp then
        8

    else if pitchClass == PitchClass.dSharp then
        9

    else if pitchClass == PitchClass.aSharp then
        10

    else if pitchClass == PitchClass.eSharp then
        11

    else if pitchClass == PitchClass.bSharp then
        12

    else
        99


symbol : Key -> String
symbol (Key pc majorOrMinor) =
    PitchClass.toString pc
        ++ (case majorOrMinor of
                Minor ->
                    "m"

                Major ->
                    ""
           )


isMajor : Key -> Bool
isMajor (Key _ majorOrMinor) =
    majorOrMinor == Major


isMinor : Key -> Bool
isMinor (Key _ majorOrMinor) =
    majorOrMinor == Minor


major : PitchClass -> Key
major root =
    Key root Major


minor : PitchClass -> Key
minor root =
    Key root Minor


scale : Key -> Scale
scale (Key root majorOrMinor) =
    case majorOrMinor of
        Major ->
            Scale.scale root ScaleType.ionian

        Minor ->
            Scale.scale root ScaleType.aeolian


tonic : Key -> PitchClass
tonic (Key root _) =
    root



---- MAJOR KEYS


gFlat : Key
gFlat =
    major <| pitchClass G PitchClass.flat


dFlat : Key
dFlat =
    major <| pitchClass D PitchClass.flat


aFlat : Key
aFlat =
    major <| pitchClass A PitchClass.flat


eFlat : Key
eFlat =
    major <| pitchClass E PitchClass.flat


bFlat : Key
bFlat =
    major <| pitchClass B PitchClass.flat


f : Key
f =
    major <| pitchClass F PitchClass.natural


c : Key
c =
    major <| pitchClass C PitchClass.natural


g : Key
g =
    major <| pitchClass G PitchClass.natural


d : Key
d =
    major <| pitchClass D PitchClass.natural


a : Key
a =
    major <| pitchClass A PitchClass.natural


e : Key
e =
    major <| pitchClass E PitchClass.natural


b : Key
b =
    major <| pitchClass B PitchClass.natural


fSharp : Key
fSharp =
    major <| pitchClass F PitchClass.sharp



---- MINOR KEYS


eFlatMinor : Key
eFlatMinor =
    minor <| pitchClass E PitchClass.flat


bFlatMinor : Key
bFlatMinor =
    minor <| pitchClass B PitchClass.flat


fMinor : Key
fMinor =
    minor <| pitchClass F PitchClass.natural


cMinor : Key
cMinor =
    minor <| pitchClass C PitchClass.natural


gMinor : Key
gMinor =
    minor <| pitchClass G PitchClass.natural


dMinor : Key
dMinor =
    minor <| pitchClass D PitchClass.natural


aMinor : Key
aMinor =
    minor <| pitchClass A PitchClass.natural


eMinor : Key
eMinor =
    minor <| pitchClass E PitchClass.natural


bMinor : Key
bMinor =
    minor <| pitchClass B PitchClass.natural


fSharpMinor : Key
fSharpMinor =
    minor <| pitchClass F PitchClass.sharp


cSharpMinor : Key
cSharpMinor =
    minor <| pitchClass C PitchClass.sharp


gSharpMinor : Key
gSharpMinor =
    minor <| pitchClass G PitchClass.sharp
