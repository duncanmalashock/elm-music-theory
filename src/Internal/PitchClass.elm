module Internal.PitchClass exposing
    ( PitchClass
    , areEnharmonicEquivalents, letter, accidentals
    , toString
    , c, cSharp, d, dSharp, dFlat, e, eFlat, f, fSharp, g, gSharp, gFlat, a, aSharp, aFlat, b, bFlat
    , cFlat, eSharp, fFlat, bSharp
    , cDoubleSharp, cDoubleFlat, dDoubleSharp, dDoubleFlat, eDoubleSharp, eDoubleFlat, fDoubleSharp, fDoubleFlat, gDoubleSharp, gDoubleFlat, aDoubleSharp, aDoubleFlat, bDoubleSharp, bDoubleFlat
    , Offset, addOffset, offset, offsetFromInt, offsetToString
    , allInChromaticScale, allInDownwardChromaticScale, allInUpwardChromaticScale
    , natural, flat, sharp, doubleFlat, doubleSharp, tripleFlat, tripleSharp
    , pitchClass, semitones, transpose
    , Serial, toSerial
    )

{-|

@docs PitchClass

@docs fromPitch, areEnharmonicEquivalents, letter, Letter, accidentals

@docs toString

@docs c, cSharp, d, dSharp, dFlat, e, eFlat, f, fSharp, g, gSharp, gFlat, a, aSharp, aFlat, b, bFlat

@docs cFlat, eSharp, fFlat, bSharp

@docs cDoubleSharp, cDoubleFlat, dDoubleSharp, dDoubleFlat, eDoubleSharp, eDoubleFlat, fDoubleSharp, fDoubleFlat, gDoubleSharp, gDoubleFlat, aDoubleSharp, aDoubleFlat, bDoubleSharp, bDoubleFlat

@docs Offset, addOffset, offset, offsetFromInt, offsetToString
@docs allInChromaticScale, allInDownwardChromaticScale, allInUpwardChromaticScale
@docs natural, flat, sharp, doubleFlat, doubleSharp, tripleFlat, tripleSharp
@docs pitchClass, semitones, transpose

@docs Serial, toSerial

-}

import Internal.Interval as Interval exposing (Direction(..), Interval(..))
import Internal.Letter as Letter exposing (Letter(..))
import Util.Basic


type alias Serial =
    { letter : String
    , offset : Int
    }


toSerial : PitchClass -> Serial
toSerial (PitchClass l (Offset o)) =
    { letter = Letter.toString l
    , offset = o
    }


allInChromaticScale : List PitchClass
allInChromaticScale =
    [ c
    , cSharp
    , dFlat
    , d
    , dSharp
    , eFlat
    , e
    , f
    , fSharp
    , gFlat
    , g
    , gSharp
    , aFlat
    , a
    , aSharp
    , bFlat
    , b
    ]


allInUpwardChromaticScale : List PitchClass
allInUpwardChromaticScale =
    [ c
    , cSharp
    , d
    , dSharp
    , e
    , f
    , fSharp
    , g
    , gSharp
    , a
    , aSharp
    , b
    ]


allInDownwardChromaticScale : List PitchClass
allInDownwardChromaticScale =
    [ c
    , dFlat
    , d
    , eFlat
    , e
    , f
    , gFlat
    , g
    , aFlat
    , a
    , bFlat
    , b
    ]


a : PitchClass
a =
    pitchClass A natural


aSharp : PitchClass
aSharp =
    pitchClass A sharp


aDoubleSharp : PitchClass
aDoubleSharp =
    pitchClass A doubleSharp


aFlat : PitchClass
aFlat =
    pitchClass A flat


aDoubleFlat : PitchClass
aDoubleFlat =
    pitchClass A doubleFlat


b : PitchClass
b =
    pitchClass B natural


bSharp : PitchClass
bSharp =
    pitchClass B sharp


bDoubleSharp : PitchClass
bDoubleSharp =
    pitchClass B doubleSharp


bFlat : PitchClass
bFlat =
    pitchClass B flat


bDoubleFlat : PitchClass
bDoubleFlat =
    pitchClass B doubleFlat


c : PitchClass
c =
    pitchClass C natural


cSharp : PitchClass
cSharp =
    pitchClass C sharp


cDoubleSharp : PitchClass
cDoubleSharp =
    pitchClass C doubleSharp


cFlat : PitchClass
cFlat =
    pitchClass C flat


cDoubleFlat : PitchClass
cDoubleFlat =
    pitchClass C doubleFlat


d : PitchClass
d =
    pitchClass D natural


dSharp : PitchClass
dSharp =
    pitchClass D sharp


dDoubleSharp : PitchClass
dDoubleSharp =
    pitchClass D doubleSharp


dFlat : PitchClass
dFlat =
    pitchClass D flat


dDoubleFlat : PitchClass
dDoubleFlat =
    pitchClass D doubleFlat


e : PitchClass
e =
    pitchClass E natural


eSharp : PitchClass
eSharp =
    pitchClass E sharp


eDoubleSharp : PitchClass
eDoubleSharp =
    pitchClass E doubleSharp


eFlat : PitchClass
eFlat =
    pitchClass E flat


eDoubleFlat : PitchClass
eDoubleFlat =
    pitchClass E doubleFlat


f : PitchClass
f =
    pitchClass F natural


fSharp : PitchClass
fSharp =
    pitchClass F sharp


fDoubleSharp : PitchClass
fDoubleSharp =
    pitchClass F doubleSharp


fFlat : PitchClass
fFlat =
    pitchClass F flat


fDoubleFlat : PitchClass
fDoubleFlat =
    pitchClass F doubleFlat


g : PitchClass
g =
    pitchClass G natural


gSharp : PitchClass
gSharp =
    pitchClass G sharp


gDoubleSharp : PitchClass
gDoubleSharp =
    pitchClass G doubleSharp


gFlat : PitchClass
gFlat =
    pitchClass G flat


gDoubleFlat : PitchClass
gDoubleFlat =
    pitchClass G doubleFlat


type PitchClass
    = PitchClass Letter Offset


type Offset
    = Offset Int


offsetFromInt : Int -> Offset
offsetFromInt int =
    Offset int


pitchClass : Letter -> Offset -> PitchClass
pitchClass l o =
    PitchClass l o


offset : PitchClass -> Int
offset (PitchClass _ (Offset o)) =
    o


addOffset : Offset -> PitchClass -> PitchClass
addOffset (Offset newO) (PitchClass l (Offset o)) =
    PitchClass l (Offset (o + newO))


accidentals : PitchClass -> Int
accidentals thePitchClass =
    offset thePitchClass


letter : PitchClass -> Letter
letter (PitchClass l _) =
    l


semitones : PitchClass -> Int
semitones (PitchClass l (Offset o)) =
    Letter.semitones l + o


tripleFlat : Offset
tripleFlat =
    Offset -3


doubleFlat : Offset
doubleFlat =
    Offset -2


flat : Offset
flat =
    Offset -1


natural : Offset
natural =
    Offset 0


sharp : Offset
sharp =
    Offset 1


doubleSharp : Offset
doubleSharp =
    Offset 2


tripleSharp : Offset
tripleSharp =
    Offset 3


toString : PitchClass -> String
toString (PitchClass l o) =
    Letter.toString l ++ offsetToString o


offsetToString : Offset -> String
offsetToString (Offset o) =
    if o == 0 then
        ""

    else if o < 0 then
        List.repeat (abs o) "♭" |> String.join ""

    else
        List.repeat (abs o) "♯" |> String.join ""


transpose : Interval -> PitchClass -> PitchClass
transpose interval pc =
    let
        numberOfIntervalSteps =
            Interval.intervalNumberIndex
                (Interval.number interval)

        startingLetter =
            letter pc

        ( targetLetter, semitoneDistance ) =
            case Interval.direction interval of
                Up ->
                    ( startingLetter, 0 )
                        |> Util.Basic.applyNTimes
                            numberOfIntervalSteps
                            Letter.nextWithSemitoneCount

                Down ->
                    ( startingLetter, 0 )
                        |> Util.Basic.applyNTimes
                            numberOfIntervalSteps
                            Letter.prevWithSemitoneCount
    in
    PitchClass
        targetLetter
        (Offset
            (Interval.semitones interval
                - semitoneDistance
                + offset pc
            )
        )


areEnharmonicEquivalents : PitchClass -> PitchClass -> Bool
areEnharmonicEquivalents lhs rhs =
    let
        lSemis =
            semitones lhs |> modBy 12

        rSemis =
            semitones rhs |> modBy 12
    in
    lSemis == rSemis
