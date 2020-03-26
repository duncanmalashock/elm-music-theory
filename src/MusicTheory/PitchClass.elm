module MusicTheory.PitchClass exposing
    ( Offset
    , PitchClass
    , a
    , aDoubleFlat
    , aDoubleSharp
    , aFlat
    , aSharp
    , areEnharmonicEquivalents
    , b
    , bDoubleFlat
    , bDoubleSharp
    , bFlat
    , bSharp
    , c
    , cDoubleFlat
    , cDoubleSharp
    , cFlat
    , cSharp
    , d
    , dDoubleFlat
    , dDoubleSharp
    , dFlat
    , dSharp
    , doubleFlat
    , doubleSharp
    , e
    , eDoubleFlat
    , eDoubleSharp
    , eFlat
    , eSharp
    , f
    , fDoubleFlat
    , fDoubleSharp
    , fFlat
    , fSharp
    , flat
    , g
    , gDoubleFlat
    , gDoubleSharp
    , gFlat
    , gSharp
    , natural
    , pitchClass
    , semitones
    , sharp
    , toString
    , transposeDown
    , transposeUp
    , tripleFlat
    , tripleSharp
    )

import MusicTheory.Interval as Interval exposing (Interval(..))
import MusicTheory.Letter as Letter exposing (Letter(..))


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


pitchClass : Letter -> Offset -> PitchClass
pitchClass l o =
    PitchClass l o


offset : PitchClass -> Int
offset (PitchClass _ (Offset o)) =
    o


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
toString pc =
    case pc of
        PitchClass l (Offset o) ->
            if o == 0 then
                Letter.toString l

            else if o < 0 then
                Letter.toString l ++ (List.repeat (abs o) "♭" |> String.join "")

            else
                Letter.toString l ++ (List.repeat (abs o) "♯" |> String.join "")


transposeUp : Interval -> PitchClass -> PitchClass
transposeUp interval pc =
    let
        ( targetLetter, letterToLetterDistance ) =
            targetLetterWithSemitoneDistance
                (Letter.index (letter pc))
                (Interval.intervalNumberIndex
                    (Interval.number interval)
                )
                ( letter pc, 0 )
    in
    PitchClass
        targetLetter
        (Offset
            (Interval.semitones interval
                - letterToLetterDistance
                + offset pc
            )
        )


transposeDown : Interval -> PitchClass -> PitchClass
transposeDown interval pc =
    interval
        |> Interval.complement
        |> (\i -> transposeUp i pc)


areEnharmonicEquivalents : PitchClass -> PitchClass -> Bool
areEnharmonicEquivalents lhs rhs =
    semitones lhs == semitones rhs



-- INTERNALS


targetLetterWithSemitoneDistance : Int -> Int -> ( Letter, Int ) -> ( Letter, Int )
targetLetterWithSemitoneDistance currentIndex steps ( currentLetter, totalSemitones ) =
    if steps <= 0 then
        ( currentLetter, totalSemitones )

    else
        let
            ( currentTargetLetter, stepSemitones ) =
                Letter.indexToLetterAndSteps (currentIndex + 1)
        in
        targetLetterWithSemitoneDistance
            (currentIndex + 1)
            (steps - 1)
            ( currentTargetLetter
            , totalSemitones + stepSemitones
            )
