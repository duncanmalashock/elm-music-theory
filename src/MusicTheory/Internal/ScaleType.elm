module MusicTheory.Internal.ScaleType exposing
    ( HeptatonicIntervals
    , HexatonicIntervals
    , OctatonicIntervals
    , PentatonicIntervals
    , ScaleType(..)
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

import MusicTheory.Internal.Interval as Interval exposing (Interval)


type ScaleType
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


toList : ScaleType -> List Interval
toList theScaleType =
    case theScaleType of
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


mode : Int -> ScaleType -> ScaleType
mode modeShiftIndex scale =
    if modeShiftIndex <= 1 then
        scale

    else
        mode (modeShiftIndex - 1) (modeShift scale)


modeShift : ScaleType -> ScaleType
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


ionian : ScaleType
ionian =
    Heptatonic
        { rootToSecond = Interval.majorSecond
        , rootToThird = Interval.majorThird
        , rootToFourth = Interval.perfectFourth
        , rootToFifth = Interval.perfectFifth
        , rootToSixth = Interval.majorSixth
        , rootToSeventh = Interval.majorSeventh
        }


major : ScaleType
major =
    ionian


dorian : ScaleType
dorian =
    ionian
        |> mode 2


phrygian : ScaleType
phrygian =
    ionian
        |> mode 3


lydian : ScaleType
lydian =
    ionian
        |> mode 4


mixolydian : ScaleType
mixolydian =
    ionian
        |> mode 5


aeolian : ScaleType
aeolian =
    ionian
        |> mode 6


locrian : ScaleType
locrian =
    ionian
        |> mode 7



---- Modes of melodic minor


melodicMinor : ScaleType
melodicMinor =
    Heptatonic
        { rootToSecond = Interval.majorSecond
        , rootToThird = Interval.minorThird
        , rootToFourth = Interval.perfectFourth
        , rootToFifth = Interval.perfectFifth
        , rootToSixth = Interval.majorSixth
        , rootToSeventh = Interval.majorSeventh
        }


dorianFlat2 : ScaleType
dorianFlat2 =
    melodicMinor
        |> mode 2


lydianAugmented : ScaleType
lydianAugmented =
    melodicMinor
        |> mode 3


acoustic : ScaleType
acoustic =
    melodicMinor
        |> mode 4


majorMinor : ScaleType
majorMinor =
    melodicMinor
        |> mode 5


minorLocrian : ScaleType
minorLocrian =
    melodicMinor
        |> mode 6


superlocrian : ScaleType
superlocrian =
    melodicMinor
        |> mode 7



---- Modes of harmonic minor


harmonicMinor : ScaleType
harmonicMinor =
    Heptatonic
        { rootToSecond = Interval.majorSecond
        , rootToThird = Interval.minorThird
        , rootToFourth = Interval.perfectFourth
        , rootToFifth = Interval.perfectFifth
        , rootToSixth = Interval.minorSixth
        , rootToSeventh = Interval.majorSeventh
        }


locrianNatural6 : ScaleType
locrianNatural6 =
    harmonicMinor
        |> mode 2


majorAugmented : ScaleType
majorAugmented =
    harmonicMinor
        |> mode 3


lydianDiminished : ScaleType
lydianDiminished =
    harmonicMinor
        |> mode 4


phrygianDominant : ScaleType
phrygianDominant =
    harmonicMinor
        |> mode 5


aeolianHarmonic : ScaleType
aeolianHarmonic =
    harmonicMinor
        |> mode 6


ultralocrian : ScaleType
ultralocrian =
    harmonicMinor
        |> mode 7



---- Symmetrical scales


diminishedWholeToneHalfTone : ScaleType
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


diminishedHalfToneWholeTone : ScaleType
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


wholeTone : ScaleType
wholeTone =
    Hexatonic
        { rootToSecond = Interval.majorSecond
        , rootToThird = Interval.majorThird
        , rootToFourth = Interval.augmentedFourth
        , rootToFifth = Interval.minorSixth
        , rootToSixth = Interval.minorSeventh
        }



---- Western pentatonic scales


majorPentatonic : ScaleType
majorPentatonic =
    Pentatonic
        { rootToSecond = Interval.majorSecond
        , rootToThird = Interval.majorThird
        , rootToFourth = Interval.perfectFifth
        , rootToFifth = Interval.majorSixth
        }


minorPentatonic : ScaleType
minorPentatonic =
    Pentatonic
        { rootToSecond = Interval.minorThird
        , rootToThird = Interval.perfectFourth
        , rootToFourth = Interval.perfectFifth
        , rootToFifth = Interval.minorSeventh
        }
