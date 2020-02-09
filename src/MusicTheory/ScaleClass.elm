module MusicTheory.ScaleClass exposing
    ( HeptatonicIntervals
    , HexatonicIntervals
    , OctatonicIntervals
    , PentatonicIntervals
    , ScaleClass(..)
    , aeolian
    , dorian
    , ionian
    , locrian
    , lydian
    , major
    , minor
    , mixolydian
    , phrygian
    ,  toIntervals
       -- , alteredDoubleFlat7
       -- , altered
       -- , arabian
       -- , augmented
       -- , balinese
       -- , blues
       -- , byzantine
       -- , chinese
       -- , diminishedHalfToneWholeTone
       -- , diminishedWholeToneHalfTone
       -- , dorianFlat9
       -- , dorianSharp11
       -- , doubleHarmonicMinor
       -- , egyptian
       -- , eightToneSpanish
       -- , enigmatic
       -- , harmonicMinor
       -- , hirajoshi
       -- , hungarianMajor
       -- , ichikosucho
       -- , ionianSharp5
       -- , kumoi
       -- , leadingWholeTone
       -- , locrianNatural13
       -- , locrianNatural9
       -- , lydianAugmented
       -- , lydianDiminished
       -- , lydianDominant
       -- , lydianMinor
       -- , lydianSharp9
       -- , majorFlat2Pentatonic
       -- , majorFlat6Pentatonic
       -- , majorPentatonic
       -- , melodicMinor
       -- , minor6Pentatonic
       -- , minorFlat5Pentatonic
       -- , minorPentatonic
       -- , mixolydianFlat13
       -- , mixolydianFlat9Flat13
       -- , neapolitan
       -- , neapolitanMajor
       -- , neapolitanMinor
       -- , pelog
       -- , persian
       -- , prometheus
       -- , prometheusNeopolitan
       -- , purviTheta
       -- , sixToneSymmetrical
       -- , todiTheta
       -- , wholeTone

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


toIntervals : ScaleClass -> List Interval
toIntervals theScale =
    case theScale of
        Pentatonic scaleDegrees ->
            [ Interval.perfectUnison
            , scaleDegrees.rootToSecond
            , scaleDegrees.rootToThird
            , scaleDegrees.rootToFourth
            , scaleDegrees.rootToFifth
            ]

        Hexatonic scaleDegrees ->
            [ Interval.perfectUnison
            , scaleDegrees.rootToSecond
            , scaleDegrees.rootToThird
            , scaleDegrees.rootToFourth
            , scaleDegrees.rootToFifth
            , scaleDegrees.rootToSixth
            ]

        Heptatonic scaleDegrees ->
            [ Interval.perfectUnison
            , scaleDegrees.rootToSecond
            , scaleDegrees.rootToThird
            , scaleDegrees.rootToFourth
            , scaleDegrees.rootToFifth
            , scaleDegrees.rootToSixth
            , scaleDegrees.rootToSeventh
            ]

        Octatonic scaleDegrees ->
            [ Interval.perfectUnison
            , scaleDegrees.rootToSecond
            , scaleDegrees.rootToThird
            , scaleDegrees.rootToFourth
            , scaleDegrees.rootToFifth
            , scaleDegrees.rootToSixth
            , scaleDegrees.rootToSeventh
            , scaleDegrees.rootToEighth
            ]



-- Modes of major


major : ScaleClass
major =
    ionian


minor : ScaleClass
minor =
    aeolian


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


dorian : ScaleClass
dorian =
    Heptatonic
        { rootToSecond = Interval.majorSecond
        , rootToThird = Interval.minorThird
        , rootToFourth = Interval.perfectFourth
        , rootToFifth = Interval.perfectFifth
        , rootToSixth = Interval.majorSixth
        , rootToSeventh = Interval.minorSeventh
        }


phrygian : ScaleClass
phrygian =
    Heptatonic
        { rootToSecond = Interval.minorSecond
        , rootToThird = Interval.minorThird
        , rootToFourth = Interval.perfectFourth
        , rootToFifth = Interval.perfectFifth
        , rootToSixth = Interval.minorSixth
        , rootToSeventh = Interval.minorSeventh
        }


lydian : ScaleClass
lydian =
    Heptatonic
        { rootToSecond = Interval.majorSecond
        , rootToThird = Interval.majorThird
        , rootToFourth = Interval.augmentedFourth
        , rootToFifth = Interval.perfectFifth
        , rootToSixth = Interval.majorSixth
        , rootToSeventh = Interval.majorSeventh
        }


mixolydian : ScaleClass
mixolydian =
    Heptatonic
        { rootToSecond = Interval.majorSecond
        , rootToThird = Interval.majorThird
        , rootToFourth = Interval.perfectFourth
        , rootToFifth = Interval.perfectFifth
        , rootToSixth = Interval.majorSixth
        , rootToSeventh = Interval.minorSeventh
        }


aeolian : ScaleClass
aeolian =
    Heptatonic
        { rootToSecond = Interval.majorSecond
        , rootToThird = Interval.minorThird
        , rootToFourth = Interval.perfectFourth
        , rootToFifth = Interval.perfectFifth
        , rootToSixth = Interval.minorSixth
        , rootToSeventh = Interval.minorSeventh
        }


locrian : ScaleClass
locrian =
    Heptatonic
        { rootToSecond = Interval.minorSecond
        , rootToThird = Interval.minorThird
        , rootToFourth = Interval.perfectFourth
        , rootToFifth = Interval.diminishedFifth
        , rootToSixth = Interval.minorSixth
        , rootToSeventh = Interval.minorSeventh
        }



-- TODO: convert to intervals from root
-- -- Modes of melodic minor
-- melodicMinor : ScaleClass
-- melodicMinor =
--     Heptatonic
--         { rootToSecond = Interval.majorSecond
--         , rootToThird = Interval.minorSecond
--         , rootToFourth = Interval.majorSecond
--         , rootToFifth = Interval.majorSecond
--         , rootToSixth = Interval.majorSecond
--         , rootToSeventh = Interval.majorSecond
--         }
-- dorianFlat9 : ScaleClass
-- dorianFlat9 =
--     Heptatonic
--         { rootToSecond = Interval.minorSecond
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.majorSecond
--         , rootToFifth = Interval.majorSecond
--         , rootToSixth = Interval.majorSecond
--         , rootToSeventh = Interval.minorSecond
--         }
-- lydianAugmented : ScaleClass
-- lydianAugmented =
--     Heptatonic
--         { rootToSecond = Interval.majorSecond
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.majorSecond
--         , rootToFifth = Interval.majorSecond
--         , rootToSixth = Interval.minorSecond
--         , rootToSeventh = Interval.majorSecond
--         }
-- lydianDominant : ScaleClass
-- lydianDominant =
--     Heptatonic
--         { rootToSecond = Interval.majorSecond
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.majorSecond
--         , rootToFifth = Interval.minorSecond
--         , rootToSixth = Interval.majorSecond
--         , rootToSeventh = Interval.minorSecond
--         }
-- mixolydianFlat13 : ScaleClass
-- mixolydianFlat13 =
--     Heptatonic
--         { rootToSecond = Interval.majorSecond
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.minorSecond
--         , rootToFifth = Interval.majorSecond
--         , rootToSixth = Interval.minorSecond
--         , rootToSeventh = Interval.majorSecond
--         }
-- locrianNatural9 : ScaleClass
-- locrianNatural9 =
--     Heptatonic
--         { rootToSecond = Interval.majorSecond
--         , rootToThird = Interval.minorSecond
--         , rootToFourth = Interval.majorSecond
--         , rootToFifth = Interval.minorSecond
--         , rootToSixth = Interval.majorSecond
--         , rootToSeventh = Interval.majorSecond
--         }
-- altered : ScaleClass
-- altered =
--     Heptatonic
--         { rootToSecond = Interval.minorSecond
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.minorSecond
--         , rootToFifth = Interval.majorSecond
--         , rootToSixth = Interval.majorSecond
--         , rootToSeventh = Interval.majorSecond
--         }
-- -- Modes of harmonic minor
-- -- (NOTE: add Superlocrian)
-- harmonicMinor : ScaleClass
-- harmonicMinor =
--     Heptatonic
--         { rootToSecond = Interval.majorSecond
--         , rootToThird = Interval.minorSecond
--         , rootToFourth = Interval.majorSecond
--         , rootToFifth = Interval.majorSecond
--         , rootToSixth = Interval.minorSecond
--         , rootToSeventh = Interval.minorThird
--         }
-- locrianNatural13 : ScaleClass
-- locrianNatural13 =
--     Heptatonic
--         { rootToSecond = Interval.minorSecond
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.majorSecond
--         , rootToFifth = Interval.minorSecond
--         , rootToSixth = Interval.minorThird
--         , rootToSeventh = Interval.minorSecond
--         }
-- ionianSharp5 : ScaleClass
-- ionianSharp5 =
--     Heptatonic
--         { rootToSecond = Interval.majorSecond
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.minorSecond
--         , rootToFifth = Interval.augmentedSecond
--         , rootToSixth = Interval.minorSecond
--         , rootToSeventh = Interval.majorSecond
--         }
-- dorianSharp11 : ScaleClass
-- dorianSharp11 =
--     Heptatonic
--         { rootToSecond = Interval.majorSecond
--         , rootToThird = Interval.minorSecond
--         , rootToFourth = Interval.augmentedSecond
--         , rootToFifth = Interval.minorSecond
--         , rootToSixth = Interval.majorSecond
--         , rootToSeventh = Interval.minorSecond
--         }
-- mixolydianFlat9Flat13 : ScaleClass
-- mixolydianFlat9Flat13 =
--     Heptatonic
--         { rootToSecond = Interval.minorSecond
--         , rootToThird = Interval.minorThird
--         , rootToFourth = Interval.minorSecond
--         , rootToFifth = Interval.majorSecond
--         , rootToSixth = Interval.minorSecond
--         , rootToSeventh = Interval.majorSecond
--         }
-- lydianSharp9 : ScaleClass
-- lydianSharp9 =
--     Heptatonic
--         { rootToSecond = Interval.majorSecond
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.majorSecond
--         , rootToFifth = Interval.minorSecond
--         , rootToSixth = Interval.majorSecond
--         , rootToSeventh = Interval.majorSecond
--         }
-- -- Other dominant scales
-- blues : ScaleClass
-- blues =
--     Hexatonic
--         { rootToSecond = Interval.minorThird
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.minorSecond
--         , rootToFifth = Interval.minorSecond
--         , rootToSixth = Interval.minorThird
--         }
-- diminishedWholeToneHalfTone : ScaleClass
-- diminishedWholeToneHalfTone =
--     Octatonic
--         { rootToSecond = Interval.majorSecond
--         , rootToThird = Interval.minorSecond
--         , rootToFourth = Interval.majorSecond
--         , rootToFifth = Interval.minorSecond
--         , rootToSixth = Interval.majorSecond
--         , rootToSeventh = Interval.minorSecond
--         , rootToEighth = Interval.majorSecond
--         }
-- diminishedHalfToneWholeTone : ScaleClass
-- diminishedHalfToneWholeTone =
--     Octatonic
--         { rootToSecond = Interval.minorSecond
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.minorSecond
--         , rootToFifth = Interval.majorSecond
--         , rootToSixth = Interval.minorSecond
--         , rootToSeventh = Interval.majorSecond
--         , rootToEighth = Interval.minorSecond
--         }
-- wholeTone : ScaleClass
-- wholeTone =
--     Hexatonic
--         { rootToSecond = Interval.majorSecond
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.majorSecond
--         , rootToFifth = Interval.majorSecond
--         , rootToSixth = Interval.majorSecond
--         }
-- -- Western pentatonic scales
-- majorPentatonic : ScaleClass
-- majorPentatonic =
--     Pentatonic
--         { rootToSecond = Interval.majorSecond
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.minorThird
--         , rootToFifth = Interval.majorSecond
--         }
-- minorPentatonic : ScaleClass
-- minorPentatonic =
--     Pentatonic
--         { rootToSecond = Interval.minorThird
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.majorSecond
--         , rootToFifth = Interval.minorThird
--         }
-- minor6Pentatonic : ScaleClass
-- minor6Pentatonic =
--     Pentatonic
--         { rootToSecond = Interval.minorThird
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.majorSecond
--         , rootToFifth = Interval.majorSecond
--         }
-- majorFlat6Pentatonic : ScaleClass
-- majorFlat6Pentatonic =
--     Pentatonic
--         { rootToSecond = Interval.majorSecond
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.minorThird
--         , rootToFifth = Interval.minorSecond
--         }
-- minorFlat5Pentatonic : ScaleClass
-- minorFlat5Pentatonic =
--     Pentatonic
--         { rootToSecond = Interval.minorThird
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.minorSecond
--         , rootToFifth = Interval.majorThird
--         }
-- majorFlat2Pentatonic : ScaleClass
-- majorFlat2Pentatonic =
--     Pentatonic
--         { rootToSecond = Interval.minorSecond
--         , rootToThird = Interval.augmentedSecond
--         , rootToFourth = Interval.minorThird
--         , rootToFifth = Interval.majorSecond
--         }
-- -- Synthetic scales
-- augmented : ScaleClass
-- augmented =
--     Hexatonic
--         { rootToSecond = Interval.minorThird
--         , rootToThird = Interval.minorSecond
--         , rootToFourth = Interval.minorThird
--         , rootToFifth = Interval.minorSecond
--         , rootToSixth = Interval.augmentedSecond
--         }
-- leadingWholeTone : ScaleClass
-- leadingWholeTone =
--     Heptatonic
--         { rootToSecond = Interval.majorSecond
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.majorSecond
--         , rootToFifth = Interval.majorSecond
--         , rootToSixth = Interval.majorSecond
--         , rootToSeventh = Interval.minorSecond
--         }
-- enigmatic : ScaleClass
-- enigmatic =
--     Heptatonic
--         { rootToSecond = Interval.minorSecond
--         , rootToThird = Interval.minorThird
--         , rootToFourth = Interval.majorSecond
--         , rootToFifth = Interval.majorSecond
--         , rootToSixth = Interval.majorSecond
--         , rootToSeventh = Interval.minorSecond
--         }
-- sixToneSymmetrical : ScaleClass
-- sixToneSymmetrical =
--     Hexatonic
--         { rootToSecond = Interval.minorSecond
--         , rootToThird = Interval.augmentedSecond
--         , rootToFourth = Interval.minorSecond
--         , rootToFifth = Interval.augmentedSecond
--         , rootToSixth = Interval.minorSecond
--         }
-- prometheus : ScaleClass
-- prometheus =
--     Hexatonic
--         { rootToSecond = Interval.majorSecond
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.majorSecond
--         , rootToFifth = Interval.minorThird
--         , rootToSixth = Interval.minorSecond
--         }
-- prometheusNeopolitan : ScaleClass
-- prometheusNeopolitan =
--     Hexatonic
--         { rootToSecond = Interval.minorSecond
--         , rootToThird = Interval.minorThird
--         , rootToFourth = Interval.majorSecond
--         , rootToFifth = Interval.minorThird
--         , rootToSixth = Interval.minorSecond
--         }
-- -- Non-Western scales
-- arabian : ScaleClass
-- arabian =
--     Heptatonic
--         { rootToSecond = Interval.majorSecond
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.minorSecond
--         , rootToFifth = Interval.minorSecond
--         , rootToSixth = Interval.majorSecond
--         , rootToSeventh = Interval.majorSecond
--         }
-- balinese : ScaleClass
-- balinese =
--     Pentatonic
--         { rootToSecond = Interval.minorSecond
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.minorThird
--         , rootToFifth = Interval.majorSecond
--         }
-- byzantine : ScaleClass
-- byzantine =
--     Heptatonic
--         { rootToSecond = Interval.minorSecond
--         , rootToThird = Interval.minorThird
--         , rootToFourth = Interval.minorSecond
--         , rootToFifth = Interval.majorSecond
--         , rootToSixth = Interval.minorSecond
--         , rootToSeventh = Interval.augmentedSecond
--         }
-- chinese : ScaleClass
-- chinese =
--     Pentatonic
--         { rootToSecond = Interval.majorThird
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.minorSecond
--         , rootToFifth = Interval.majorThird
--         }
-- egyptian : ScaleClass
-- egyptian =
--     Pentatonic
--         { rootToSecond = Interval.majorSecond
--         , rootToThird = Interval.minorThird
--         , rootToFourth = Interval.majorSecond
--         , rootToFifth = Interval.minorThird
--         }
-- eightToneSpanish : ScaleClass
-- eightToneSpanish =
--     Octatonic
--         { rootToSecond = Interval.minorSecond
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.minorSecond
--         , rootToFifth = Interval.minorSecond
--         , rootToSixth = Interval.minorSecond
--         , rootToSeventh = Interval.majorSecond
--         , rootToEighth = Interval.majorSecond
--         }
-- hirajoshi : ScaleClass
-- hirajoshi =
--     Pentatonic
--         { rootToSecond = Interval.majorSecond
--         , rootToThird = Interval.minorSecond
--         , rootToFourth = Interval.majorThird
--         , rootToFifth = Interval.minorSecond
--         }
-- hungarianMajor : ScaleClass
-- hungarianMajor =
--     Heptatonic
--         { rootToSecond = Interval.minorThird
--         , rootToThird = Interval.minorSecond
--         , rootToFourth = Interval.majorSecond
--         , rootToFifth = Interval.minorSecond
--         , rootToSixth = Interval.majorSecond
--         , rootToSeventh = Interval.minorSecond
--         }
-- ichikosucho : ScaleClass
-- ichikosucho =
--     Octatonic
--         { rootToSecond = Interval.majorSecond
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.minorSecond
--         , rootToFifth = Interval.minorSecond
--         , rootToSixth = Interval.minorSecond
--         , rootToSeventh = Interval.majorSecond
--         , rootToEighth = Interval.majorSecond
--         }
-- kumoi : ScaleClass
-- kumoi =
--     Pentatonic
--         { rootToSecond = Interval.majorSecond
--         , rootToThird = Interval.minorSecond
--         , rootToFourth = Interval.majorThird
--         , rootToFifth = Interval.majorSecond
--         }
-- pelog : ScaleClass
-- pelog =
--     Pentatonic
--         { rootToSecond = Interval.minorSecond
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.majorThird
--         , rootToFifth = Interval.minorSecond
--         }
-- persian : ScaleClass
-- persian =
--     Heptatonic
--         { rootToSecond = Interval.minorSecond
--         , rootToThird = Interval.minorThird
--         , rootToFourth = Interval.minorSecond
--         , rootToFifth = Interval.minorSecond
--         , rootToSixth = Interval.majorSecond
--         , rootToSeventh = Interval.minorThird
--         }
-- purviTheta : ScaleClass
-- purviTheta =
--     Heptatonic
--         { rootToSecond = Interval.minorSecond
--         , rootToThird = Interval.augmentedSecond
--         , rootToFourth = Interval.majorSecond
--         , rootToFifth = Interval.minorSecond
--         , rootToSixth = Interval.minorSecond
--         , rootToSeventh = Interval.augmentedSecond
--         }
-- todiTheta : ScaleClass
-- todiTheta =
--     Heptatonic
--         { rootToSecond = Interval.minorSecond
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.minorThird
--         , rootToFifth = Interval.minorSecond
--         , rootToSixth = Interval.minorSecond
--         , rootToSeventh = Interval.augmentedSecond
--         }
-- -- Others
-- -- NOTE: categorize
-- alteredDoubleFlat7 : ScaleClass
-- alteredDoubleFlat7 =
--     Heptatonic
--         { rootToSecond = Interval.minorSecond
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.minorSecond
--         , rootToFifth = Interval.majorSecond
--         , rootToSixth = Interval.majorSecond
--         , rootToSeventh = Interval.minorSecond
--         }
-- lydianDiminished : ScaleClass
-- lydianDiminished =
--     Heptatonic
--         { rootToSecond = Interval.majorSecond
--         , rootToThird = Interval.minorSecond
--         , rootToFourth = Interval.minorThird
--         , rootToFifth = Interval.minorSecond
--         , rootToSixth = Interval.majorSecond
--         , rootToSeventh = Interval.majorSecond
--         }
-- lydianMinor : ScaleClass
-- lydianMinor =
--     Heptatonic
--         { rootToSecond = Interval.majorSecond
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.majorSecond
--         , rootToFifth = Interval.minorSecond
--         , rootToSixth = Interval.minorSecond
--         , rootToSeventh = Interval.majorSecond
--         }
-- doubleHarmonicMinor : ScaleClass
-- doubleHarmonicMinor =
--     Heptatonic
--         { rootToSecond = Interval.majorSecond
--         , rootToThird = Interval.minorSecond
--         , rootToFourth = Interval.minorThird
--         , rootToFifth = Interval.minorSecond
--         , rootToSixth = Interval.minorSecond
--         , rootToSeventh = Interval.augmentedSecond
--         }
-- neapolitan : ScaleClass
-- neapolitan =
--     Heptatonic
--         { rootToSecond = Interval.minorSecond
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.majorSecond
--         , rootToFifth = Interval.majorSecond
--         , rootToSixth = Interval.minorSecond
--         , rootToSeventh = Interval.minorThird
--         }
-- neapolitanMajor : ScaleClass
-- neapolitanMajor =
--     Heptatonic
--         { rootToSecond = Interval.minorSecond
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.majorSecond
--         , rootToFifth = Interval.majorSecond
--         , rootToSixth = Interval.majorSecond
--         , rootToSeventh = Interval.majorSecond
--         }
-- neapolitanMinor : ScaleClass
-- neapolitanMinor =
--     Heptatonic
--         { rootToSecond = Interval.minorSecond
--         , rootToThird = Interval.majorSecond
--         , rootToFourth = Interval.majorSecond
--         , rootToFifth = Interval.majorSecond
--         , rootToSixth = Interval.minorSecond
--         , rootToSeventh = Interval.majorSecond
--         }
