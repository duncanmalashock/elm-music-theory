module MusicTheory.ScaleClass exposing
    ( HeptatonicIntervals
    , HexatonicIntervals
    , OctatonicIntervals
    , PentatonicIntervals
    , ScaleClass(..)
    , aeolian
    , altered
    , alteredDoubleFlat7
    , arabian
    , augmented
    , balinese
    , blues
    , byzantine
    , chinese
    , diminishedHalfToneWholeTone
    , diminishedWholeToneHalfTone
    , dorian
    , dorianFlat9
    , dorianSharp11
    , doubleHarmonicMinor
    , egyptian
    , eightToneSpanish
    , enigmatic
    , harmonicMinor
    , hirajoshi
    , hungarianMajor
    , ichikosucho
    , ionian
    , ionianSharp5
    , kumoi
    , leadingWholeTone
    , locrian
    , locrianNatural13
    , locrianNatural9
    , lydian
    , lydianAugmented
    , lydianDiminished
    , lydianDominant
    , lydianMinor
    , lydianSharp9
    , major
    , majorFlat2Pentatonic
    , majorFlat6Pentatonic
    , majorPentatonic
    , melodicMinor
    , minor
    , minor6Pentatonic
    , minorFlat5Pentatonic
    , minorPentatonic
    , mixolydian
    , mixolydianFlat13
    , mixolydianFlat9Flat13
    , neapolitan
    , neapolitanMajor
    , neapolitanMinor
    , pelog
    , persian
    , phrygian
    , prometheus
    , prometheusNeopolitan
    , purviTheta
    , sixToneSymmetrical
    , todiTheta
    , wholeTone
    )

import MusicTheory.Interval as Interval exposing (Interval)


type ScaleClass
    = Pentatonic PentatonicIntervals
    | Hexatonic HexatonicIntervals
    | Heptatonic HeptatonicIntervals
    | Octatonic OctatonicIntervals


type alias PentatonicIntervals =
    { first : Interval
    , second : Interval
    , third : Interval
    , fourth : Interval
    }


type alias HexatonicIntervals =
    { first : Interval
    , second : Interval
    , third : Interval
    , fourth : Interval
    , fifth : Interval
    }


type alias HeptatonicIntervals =
    { first : Interval
    , second : Interval
    , third : Interval
    , fourth : Interval
    , fifth : Interval
    , sixth : Interval
    }


type alias OctatonicIntervals =
    { first : Interval
    , second : Interval
    , third : Interval
    , fourth : Interval
    , fifth : Interval
    , sixth : Interval
    , seventh : Interval
    }



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
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


dorian : ScaleClass
dorian =
    Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


phrygian : ScaleClass
phrygian =
    Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


lydian : ScaleClass
lydian =
    Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


mixolydian : ScaleClass
mixolydian =
    Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


aeolian : ScaleClass
aeolian =
    Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


locrian : ScaleClass
locrian =
    Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }



-- Modes of melodic minor


melodicMinor : ScaleClass
melodicMinor =
    Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


dorianFlat9 : ScaleClass
dorianFlat9 =
    Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


lydianAugmented : ScaleClass
lydianAugmented =
    Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


lydianDominant : ScaleClass
lydianDominant =
    Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


mixolydianFlat13 : ScaleClass
mixolydianFlat13 =
    Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


locrianNatural9 : ScaleClass
locrianNatural9 =
    Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


altered : ScaleClass
altered =
    Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }



-- Modes of harmonic minor
-- (NOTE: add Superlocrian)


harmonicMinor : ScaleClass
harmonicMinor =
    Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.minorThird
        }


locrianNatural13 : ScaleClass
locrianNatural13 =
    Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.minorThird
        , sixth = Interval.minorSecond
        }


ionianSharp5 : ScaleClass
ionianSharp5 =
    Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.augmentedSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


dorianSharp11 : ScaleClass
dorianSharp11 =
    Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.augmentedSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


mixolydianFlat9Flat13 : ScaleClass
mixolydianFlat9Flat13 =
    Heptatonic
        { first = Interval.minorSecond
        , second = Interval.minorThird
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


lydianSharp9 : ScaleClass
lydianSharp9 =
    Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }



-- Other dominant scales


blues : ScaleClass
blues =
    Hexatonic
        { first = Interval.minorThird
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.minorThird
        }


diminishedWholeToneHalfTone : ScaleClass
diminishedWholeToneHalfTone =
    Octatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        , seventh = Interval.majorSecond
        }


diminishedHalfToneWholeTone : ScaleClass
diminishedHalfToneWholeTone =
    Octatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        , seventh = Interval.minorSecond
        }


wholeTone : ScaleClass
wholeTone =
    Hexatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        }



-- Western pentatonic scales


majorPentatonic : ScaleClass
majorPentatonic =
    Pentatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorThird
        , fourth = Interval.majorSecond
        }


minorPentatonic : ScaleClass
minorPentatonic =
    Pentatonic
        { first = Interval.minorThird
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorThird
        }


minor6Pentatonic : ScaleClass
minor6Pentatonic =
    Pentatonic
        { first = Interval.minorThird
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        }


majorFlat6Pentatonic : ScaleClass
majorFlat6Pentatonic =
    Pentatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorThird
        , fourth = Interval.minorSecond
        }


minorFlat5Pentatonic : ScaleClass
minorFlat5Pentatonic =
    Pentatonic
        { first = Interval.minorThird
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorThird
        }


majorFlat2Pentatonic : ScaleClass
majorFlat2Pentatonic =
    Pentatonic
        { first = Interval.minorSecond
        , second = Interval.augmentedSecond
        , third = Interval.minorThird
        , fourth = Interval.majorSecond
        }



-- Synthetic scales


augmented : ScaleClass
augmented =
    Hexatonic
        { first = Interval.minorThird
        , second = Interval.minorSecond
        , third = Interval.minorThird
        , fourth = Interval.minorSecond
        , fifth = Interval.augmentedSecond
        }


leadingWholeTone : ScaleClass
leadingWholeTone =
    Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


enigmatic : ScaleClass
enigmatic =
    Heptatonic
        { first = Interval.minorSecond
        , second = Interval.minorThird
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


sixToneSymmetrical : ScaleClass
sixToneSymmetrical =
    Hexatonic
        { first = Interval.minorSecond
        , second = Interval.augmentedSecond
        , third = Interval.minorSecond
        , fourth = Interval.augmentedSecond
        , fifth = Interval.minorSecond
        }


prometheus : ScaleClass
prometheus =
    Hexatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorThird
        , fifth = Interval.minorSecond
        }


prometheusNeopolitan : ScaleClass
prometheusNeopolitan =
    Hexatonic
        { first = Interval.minorSecond
        , second = Interval.minorThird
        , third = Interval.majorSecond
        , fourth = Interval.minorThird
        , fifth = Interval.minorSecond
        }



-- Non-Western scales


arabian : ScaleClass
arabian =
    Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


balinese : ScaleClass
balinese =
    Pentatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.minorThird
        , fourth = Interval.majorSecond
        }


byzantine : ScaleClass
byzantine =
    Heptatonic
        { first = Interval.minorSecond
        , second = Interval.minorThird
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.augmentedSecond
        }


chinese : ScaleClass
chinese =
    Pentatonic
        { first = Interval.majorThird
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorThird
        }


egyptian : ScaleClass
egyptian =
    Pentatonic
        { first = Interval.majorSecond
        , second = Interval.minorThird
        , third = Interval.majorSecond
        , fourth = Interval.minorThird
        }


eightToneSpanish : ScaleClass
eightToneSpanish =
    Octatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        , seventh = Interval.majorSecond
        }


hirajoshi : ScaleClass
hirajoshi =
    Pentatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorThird
        , fourth = Interval.minorSecond
        }


hungarianMajor : ScaleClass
hungarianMajor =
    Heptatonic
        { first = Interval.minorThird
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


ichikosucho : ScaleClass
ichikosucho =
    Octatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        , seventh = Interval.majorSecond
        }


kumoi : ScaleClass
kumoi =
    Pentatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorThird
        , fourth = Interval.majorSecond
        }


pelog : ScaleClass
pelog =
    Pentatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorThird
        , fourth = Interval.minorSecond
        }


persian : ScaleClass
persian =
    Heptatonic
        { first = Interval.minorSecond
        , second = Interval.minorThird
        , third = Interval.minorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorThird
        }


purviTheta : ScaleClass
purviTheta =
    Heptatonic
        { first = Interval.minorSecond
        , second = Interval.augmentedSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.augmentedSecond
        }


todiTheta : ScaleClass
todiTheta =
    Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.minorThird
        , fourth = Interval.minorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.augmentedSecond
        }



-- Others
-- NOTE: categorize


alteredDoubleFlat7 : ScaleClass
alteredDoubleFlat7 =
    Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


lydianDiminished : ScaleClass
lydianDiminished =
    Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.minorThird
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


lydianMinor : ScaleClass
lydianMinor =
    Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


doubleHarmonicMinor : ScaleClass
doubleHarmonicMinor =
    Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.minorThird
        , fourth = Interval.minorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.augmentedSecond
        }


neapolitan : ScaleClass
neapolitan =
    Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.minorThird
        }


neapolitanMajor : ScaleClass
neapolitanMajor =
    Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


neapolitanMinor : ScaleClass
neapolitanMinor =
    Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }
