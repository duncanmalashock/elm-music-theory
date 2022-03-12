module Internal.ScaleType exposing
    ( ScaleType(..)
    , mode
    , toList
    , major, minor
    , ionian, dorian, phrygian, lydian, mixolydian, aeolian, locrian
    , melodicMinor, dorianFlat2, lydianAugmented, acoustic, majorMinor, minorLocrian, superlocrian
    , harmonicMinor, locrianNatural6, majorAugmented, lydianDiminished, phrygianDominant, aeolianHarmonic, ultralocrian
    , diminishedWholeToneHalfTone, diminishedHalfToneWholeTone, wholeTone
    , majorPentatonic, minorPentatonic
    , customPentatonic, customHexatonic, customHeptatonic, customOctatonic
    , HeptatonicIntervals, HexatonicIntervals, OctatonicIntervals, PentatonicIntervals
    , name, nameWithCustom
    )

{-|

@docs ScaleType

@docs mode

@docs toList

@docs major, minor

@docs ionian, dorian, phrygian, lydian, mixolydian, aeolian, locrian

@docs melodicMinor, dorianFlat2, lydianAugmented, acoustic, majorMinor, minorLocrian, superlocrian

@docs harmonicMinor, locrianNatural6, majorAugmented, lydianDiminished, phrygianDominant, aeolianHarmonic, ultralocrian

@docs diminishedWholeToneHalfTone, diminishedHalfToneWholeTone, wholeTone

@docs majorPentatonic, minorPentatonic

@docs customPentatonic, customHexatonic, customHeptatonic, customOctatonic

@docs HeptatonicIntervals, HexatonicIntervals, OctatonicIntervals, PentatonicIntervals

@docs name, nameWithCustom

-}

import AssocList
import Internal.Interval as Interval exposing (Interval)


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


customPentatonic :
    { rootToSecond : Interval
    , rootToThird : Interval
    , rootToFourth : Interval
    , rootToFifth : Interval
    }
    -> ScaleType
customPentatonic intervals =
    Pentatonic intervals


customHexatonic :
    { rootToSecond : Interval
    , rootToThird : Interval
    , rootToFourth : Interval
    , rootToFifth : Interval
    , rootToSixth : Interval
    }
    -> ScaleType
customHexatonic intervals =
    Hexatonic intervals


customHeptatonic :
    { rootToSecond : Interval
    , rootToThird : Interval
    , rootToFourth : Interval
    , rootToFifth : Interval
    , rootToSixth : Interval
    , rootToSeventh : Interval
    }
    -> ScaleType
customHeptatonic intervals =
    Heptatonic intervals


customOctatonic :
    { rootToSecond : Interval
    , rootToThird : Interval
    , rootToFourth : Interval
    , rootToFifth : Interval
    , rootToSixth : Interval
    , rootToSeventh : Interval
    , rootToEighth : Interval
    }
    -> ScaleType
customOctatonic intervals =
    Octatonic intervals


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


name : ScaleType -> Maybe String
name scaleType =
    AssocList.get scaleType (nameDict [])


nameWithCustom : List ( ScaleType, String ) -> ScaleType -> Maybe String
nameWithCustom customs scaleType =
    AssocList.get scaleType (nameDict customs)


nameDict : List ( ScaleType, String ) -> AssocList.Dict ScaleType String
nameDict customs =
    AssocList.fromList <|
        [ ( ionian, "Ionian" )
        , ( dorian, "Dorian" )
        , ( phrygian, "Phrygian" )
        , ( lydian, "Lydian" )
        , ( mixolydian, "Mixolydian" )
        , ( aeolian, "Aeolian" )
        , ( locrian, "Locrian" )
        , ( melodicMinor, "Melodic Minor" )
        , ( dorianFlat2, "Dorian Flat 2" )
        , ( lydianAugmented, "Lydian Augmented" )
        , ( acoustic, "Acoustic" )
        , ( majorMinor, "Major Minor" )
        , ( minorLocrian, "Minor Locrian" )
        , ( superlocrian, "Superlocrian" )
        , ( harmonicMinor, "Harmonic Minor" )
        , ( locrianNatural6, "Locrian Natural 6" )
        , ( majorAugmented, "Major Augmented" )
        , ( lydianDiminished, "Lydian Diminished" )
        , ( phrygianDominant, "Phrygian Dominant" )
        , ( aeolianHarmonic, "Aeolian Harmonic" )
        , ( ultralocrian, "Ultralocrian" )
        , ( diminishedWholeToneHalfTone, "Diminished Whole-Half" )
        , ( diminishedHalfToneWholeTone, "Diminished Half-Whole" )
        , ( wholeTone, "Whole Tone" )
        , ( majorPentatonic, "Major Pentatonic" )
        , ( minorPentatonic, "Minor Pentatonic" )
        , ( major, "Major" )
        , ( minor, "Minor" )
        ]
            ++ customs



-- Common aliases


major : ScaleType
major =
    ionian


minor : ScaleType
minor =
    aeolian



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
