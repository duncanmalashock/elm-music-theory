module MusicTheory.Internal.PitchClass exposing
    ( Offset
    , PitchClass
    , all
    , areEnharmonicEqual
    , doubleFlat
    , doubleSharp
    , flat
    , letter
    , natural
    , offset
    , pitchClass
    , semitones
    , semitonesNotOctaveBound
    , sharp
    , toString
    , transposeDown
    , transposeUp
    , tripleFlat
    , tripleSharp
    )

import MusicTheory.Interval as Interval exposing (Interval(..))
import MusicTheory.Letter as Letter exposing (Letter(..))


type Offset
    = Offset Int


type PitchClass
    = PitchClass Letter Offset


pitchClass : Letter -> Offset -> PitchClass
pitchClass l o =
    PitchClass l o


offset : PitchClass -> Int
offset (PitchClass _ (Offset o)) =
    o


letter : PitchClass -> Letter
letter (PitchClass l _) =
    l


semitonesNotOctaveBound : PitchClass -> Int
semitonesNotOctaveBound (PitchClass l (Offset o)) =
    Letter.semitones l + o


semitones : PitchClass -> Int
semitones pc =
    semitonesNotOctaveBound pc |> modBy 12


all : List PitchClass
all =
    Letter.letters
        |> List.concatMap (\l -> [ tripleFlat, doubleFlat, flat, natural, sharp, doubleSharp, tripleSharp ] |> List.map (pitchClass l))


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
            targetLetterWithSemitoneDistance (Letter.index (letter pc)) (Interval.intervalNumberIndex (Interval.number interval)) ( letter pc, 0 )
    in
    PitchClass targetLetter (Offset (Interval.semitones interval - letterToLetterDistance + offset pc))


transposeDown : Interval -> PitchClass -> PitchClass
transposeDown interval pc =
    interval
        |> Interval.complementary
        |> (\i -> transposeUp i pc)


areEnharmonicEqual : PitchClass -> PitchClass -> Bool
areEnharmonicEqual lhs rhs =
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
        targetLetterWithSemitoneDistance (currentIndex + 1) (steps - 1) ( currentTargetLetter, totalSemitones + stepSemitones )
