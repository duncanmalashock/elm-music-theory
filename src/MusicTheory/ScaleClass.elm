module MusicTheory.ScaleClass exposing
    ( HeptatonicIntervals
    , HexatonicIntervals
    , OctatonicIntervals
    , PentatonicIntervals
    , ScaleClass(..)
    , acoustic
    , aeolian
    , aeolianHarmonic
    , diminishedHalfToneWholeTone
    , diminishedWholeToneHalfTone
    , dorian
    , dorianFlat2
    , harmonicMinor
    , ionian
    , locrian
    , locrianNatural6
    , lydian
    , lydianAugmented
    , lydianDiminished
    , major
    , majorAugmented
    , majorMinor
    , majorPentatonic
    , melodicMinor
    , minorLocrian
    , minorPentatonic
    , mixolydian
    , phrygian
    , phrygianDominant
    , superlocrian
    , toList
    , ultralocrian
    , wholeTone
    )

import MusicTheory.Interval as Interval exposing (Interval)


type ScaleClass
    = Pentatonic PentatonicIntervals
    | Hexatonic HexatonicIntervals
    | Heptatonic HeptatonicIntervals
    | Octatonic OctatonicIntervals


type alias PentatonicIntervals =
    { rootToSecond : Interval
    , rootToThird : Interval
    , rootToFourth : Interval
    , rootToFifth : Interval
    }


type alias HexatonicIntervals =
    { rootToSecond : Interval
    , rootToThird : Interval
    , rootToFourth : Interval
    , rootToFifth : Interval
    , rootToSixth : Interval
    }


type alias HeptatonicIntervals =
    { rootToSecond : Interval
    , rootToThird : Interval
    , rootToFourth : Interval
    , rootToFifth : Interval
    , rootToSixth : Interval
    , rootToSeventh : Interval
    }


type alias OctatonicIntervals =
    { rootToSecond : Interval
    , rootToThird : Interval
    , rootToFourth : Interval
    , rootToFifth : Interval
    , rootToSixth : Interval
    , rootToSeventh : Interval
    , rootToEighth : Interval
    }


toList : ScaleClass -> List Interval
toList theScaleClass =
    case theScaleClass of
        Pentatonic intervals ->
            [ Interval.perfectUnison
            , intervals.rootToSecond
            , intervals.rootToThird
            , intervals.rootToFourth
            , intervals.rootToFifth
            ]

        Hexatonic intervals ->
            [ Interval.perfectUnison
            , intervals.rootToSecond
            , intervals.rootToThird
            , intervals.rootToFourth
            , intervals.rootToFifth
            , intervals.rootToSixth
            ]

        Heptatonic intervals ->
            [ Interval.perfectUnison
            , intervals.rootToSecond
            , intervals.rootToThird
            , intervals.rootToFourth
            , intervals.rootToFifth
            , intervals.rootToSixth
            , intervals.rootToSeventh
            ]

        Octatonic intervals ->
            [ Interval.perfectUnison
            , intervals.rootToSecond
            , intervals.rootToThird
            , intervals.rootToFourth
            , intervals.rootToFifth
            , intervals.rootToSixth
            , intervals.rootToSeventh
            , intervals.rootToEighth
            ]


mode : Int -> ScaleClass -> ScaleClass
mode modeShiftIndex scale =
    if modeShiftIndex <= 1 then
        scale

    else
        mode (modeShiftIndex - 1) (modeShift scale)


modeShift : ScaleClass -> ScaleClass
modeShift scale =
    case scale of
        Pentatonic pent ->
            let
                intervalToSubtract =
                    pent.rootToSecond
                        |> Interval.reverse
            in
            { rootToSecond = Interval.add pent.rootToThird intervalToSubtract
            , rootToThird = Interval.add pent.rootToFourth intervalToSubtract
            , rootToFourth = Interval.add pent.rootToFifth intervalToSubtract
            , rootToFifth = Interval.add Interval.perfectOctave intervalToSubtract
            }
                |> Pentatonic

        Hexatonic hex ->
            let
                intervalToSubtract =
                    hex.rootToSecond
                        |> Interval.reverse
            in
            { rootToSecond = Interval.add hex.rootToThird intervalToSubtract
            , rootToThird = Interval.add hex.rootToFourth intervalToSubtract
            , rootToFourth = Interval.add hex.rootToFifth intervalToSubtract
            , rootToFifth = Interval.add hex.rootToSixth intervalToSubtract
            , rootToSixth = Interval.add Interval.perfectOctave intervalToSubtract
            }
                |> Hexatonic

        Heptatonic hep ->
            let
                intervalToSubtract =
                    hep.rootToSecond
                        |> Interval.reverse
            in
            { rootToSecond = Interval.add hep.rootToThird intervalToSubtract
            , rootToThird = Interval.add hep.rootToFourth intervalToSubtract
            , rootToFourth = Interval.add hep.rootToFifth intervalToSubtract
            , rootToFifth = Interval.add hep.rootToSixth intervalToSubtract
            , rootToSixth = Interval.add hep.rootToSeventh intervalToSubtract
            , rootToSeventh = Interval.add Interval.perfectOctave intervalToSubtract
            }
                |> Heptatonic

        Octatonic oct ->
            let
                intervalToSubtract =
                    oct.rootToSecond
                        |> Interval.reverse
            in
            { rootToSecond = Interval.add oct.rootToThird intervalToSubtract
            , rootToThird = Interval.add oct.rootToFourth intervalToSubtract
            , rootToFourth = Interval.add oct.rootToFifth intervalToSubtract
            , rootToFifth = Interval.add oct.rootToSixth intervalToSubtract
            , rootToSixth = Interval.add oct.rootToSeventh intervalToSubtract
            , rootToSeventh = Interval.add oct.rootToEighth intervalToSubtract
            , rootToEighth = Interval.add Interval.perfectOctave intervalToSubtract
            }
                |> Octatonic



-- Modes of major


ionian : ScaleClass
ionian =
    Heptatonic
        { rootToSecond = Interval.majorSecond
        , rootToThird = Interval.majorThird
        , rootToFourth = Interval.perfectFourth
        , rootToFifth = Interval.perfectFifth
        , rootToSixth = Interval.majorSixth
        , rootToSeventh = Interval.majorSeventh
        }


major : ScaleClass
major =
    ionian


dorian : ScaleClass
dorian =
    ionian
        |> mode 2


phrygian : ScaleClass
phrygian =
    ionian
        |> mode 3


lydian : ScaleClass
lydian =
    ionian
        |> mode 4


mixolydian : ScaleClass
mixolydian =
    ionian
        |> mode 5


aeolian : ScaleClass
aeolian =
    ionian
        |> mode 6


locrian : ScaleClass
locrian =
    ionian
        |> mode 7



---- Modes of melodic minor


melodicMinor : ScaleClass
melodicMinor =
    Heptatonic
        { rootToSecond = Interval.majorSecond
        , rootToThird = Interval.minorThird
        , rootToFourth = Interval.perfectFourth
        , rootToFifth = Interval.diminishedFifth
        , rootToSixth = Interval.majorSixth
        , rootToSeventh = Interval.majorSeventh
        }


dorianFlat2 : ScaleClass
dorianFlat2 =
    melodicMinor
        |> mode 2


lydianAugmented : ScaleClass
lydianAugmented =
    melodicMinor
        |> mode 3


acoustic : ScaleClass
acoustic =
    melodicMinor
        |> mode 4


majorMinor : ScaleClass
majorMinor =
    melodicMinor
        |> mode 5


minorLocrian : ScaleClass
minorLocrian =
    melodicMinor
        |> mode 6


superlocrian : ScaleClass
superlocrian =
    melodicMinor
        |> mode 7



---- Modes of harmonic minor


harmonicMinor : ScaleClass
harmonicMinor =
    Heptatonic
        { rootToSecond = Interval.majorSecond
        , rootToThird = Interval.minorThird
        , rootToFourth = Interval.perfectFourth
        , rootToFifth = Interval.perfectFifth
        , rootToSixth = Interval.minorSixth
        , rootToSeventh = Interval.majorSeventh
        }


locrianNatural6 : ScaleClass
locrianNatural6 =
    harmonicMinor
        |> mode 2


majorAugmented : ScaleClass
majorAugmented =
    harmonicMinor
        |> mode 3


lydianDiminished : ScaleClass
lydianDiminished =
    harmonicMinor
        |> mode 4


phrygianDominant : ScaleClass
phrygianDominant =
    harmonicMinor
        |> mode 5


aeolianHarmonic : ScaleClass
aeolianHarmonic =
    harmonicMinor
        |> mode 6


ultralocrian : ScaleClass
ultralocrian =
    harmonicMinor
        |> mode 7



---- Symmetrical scales


diminishedWholeToneHalfTone : ScaleClass
diminishedWholeToneHalfTone =
    Octatonic
        { rootToSecond = Interval.majorSecond
        , rootToThird = Interval.minorThird
        , rootToFourth = Interval.perfectFourth
        , rootToFifth = Interval.diminishedFifth
        , rootToSixth = Interval.minorSixth
        , rootToSeventh = Interval.majorSixth
        , rootToEighth = Interval.majorSeventh
        }


diminishedHalfToneWholeTone : ScaleClass
diminishedHalfToneWholeTone =
    Octatonic
        { rootToSecond = Interval.minorSecond
        , rootToThird = Interval.minorThird
        , rootToFourth = Interval.majorThird
        , rootToFifth = Interval.augmentedFourth
        , rootToSixth = Interval.perfectFifth
        , rootToSeventh = Interval.majorSixth
        , rootToEighth = Interval.minorSeventh
        }


wholeTone : ScaleClass
wholeTone =
    Hexatonic
        { rootToSecond = Interval.majorSecond
        , rootToThird = Interval.majorThird
        , rootToFourth = Interval.augmentedFourth
        , rootToFifth = Interval.minorSixth
        , rootToSixth = Interval.minorSeventh
        }



---- Western pentatonic scales


majorPentatonic : ScaleClass
majorPentatonic =
    Pentatonic
        { rootToSecond = Interval.majorSecond
        , rootToThird = Interval.majorThird
        , rootToFourth = Interval.perfectFifth
        , rootToFifth = Interval.majorSixth
        }


minorPentatonic : ScaleClass
minorPentatonic =
    Pentatonic
        { rootToSecond = Interval.minorThird
        , rootToThird = Interval.perfectFourth
        , rootToFourth = Interval.perfectFifth
        , rootToFifth = Interval.minorSeventh
        }
