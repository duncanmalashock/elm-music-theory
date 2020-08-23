module MusicTheory.Internal.Key exposing
    ( Key
    , MajorOrMinor(..)
    , aFlatMajor
    , aMajor
    , aMinor
    , bFlatMajor
    , bFlatMinor
    , bMajor
    , bMinor
    , cMajor
    , cMinor
    , cSharpMinor
    , dFlatMajor
    , dMajor
    , dMinor
    , eFlatMajor
    , eFlatMinor
    , eMajor
    , eMinor
    , fMajor
    , fMinor
    , fSharpMajor
    , fSharpMinor
    , gFlatMajor
    , gMajor
    , gMinor
    , gSharpMinor
    , major
    , minor
    , scale
    , tonic
    )

import MusicTheory.Internal.Letter exposing (Letter(..))
import MusicTheory.Internal.PitchClass as PitchClass exposing (PitchClass, pitchClass)
import MusicTheory.Internal.Scale as Scale exposing (Scale)
import MusicTheory.Internal.ScaleType as ScaleType


type Key
    = Key PitchClass MajorOrMinor


type MajorOrMinor
    = Major
    | Minor


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


gFlatMajor : Key
gFlatMajor =
    major <| pitchClass G PitchClass.flat


dFlatMajor : Key
dFlatMajor =
    major <| pitchClass D PitchClass.flat


aFlatMajor : Key
aFlatMajor =
    major <| pitchClass A PitchClass.flat


eFlatMajor : Key
eFlatMajor =
    major <| pitchClass E PitchClass.flat


bFlatMajor : Key
bFlatMajor =
    major <| pitchClass B PitchClass.flat


fMajor : Key
fMajor =
    major <| pitchClass F PitchClass.natural


cMajor : Key
cMajor =
    major <| pitchClass C PitchClass.natural


gMajor : Key
gMajor =
    major <| pitchClass G PitchClass.natural


dMajor : Key
dMajor =
    major <| pitchClass D PitchClass.natural


aMajor : Key
aMajor =
    major <| pitchClass A PitchClass.natural


eMajor : Key
eMajor =
    major <| pitchClass E PitchClass.natural


bMajor : Key
bMajor =
    major <| pitchClass B PitchClass.natural


fSharpMajor : Key
fSharpMajor =
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
