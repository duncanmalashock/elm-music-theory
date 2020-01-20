module MusicTheory.ScaleClass exposing
    ( ScaleClass
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

import MusicTheory.Internal.ScaleClass as ScaleClass
import MusicTheory.Interval as Interval


type alias ScaleClass =
    ScaleClass.ScaleClass



-- Modes of major


major : ScaleClass
major =
    ionian


minor : ScaleClass
minor =
    aeolian


ionian : ScaleClass
ionian =
    ScaleClass.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


dorian : ScaleClass
dorian =
    ScaleClass.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


phrygian : ScaleClass
phrygian =
    ScaleClass.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


lydian : ScaleClass
lydian =
    ScaleClass.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


mixolydian : ScaleClass
mixolydian =
    ScaleClass.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


aeolian : ScaleClass
aeolian =
    ScaleClass.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


locrian : ScaleClass
locrian =
    ScaleClass.Heptatonic
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
    ScaleClass.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


dorianFlat9 : ScaleClass
dorianFlat9 =
    ScaleClass.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


lydianAugmented : ScaleClass
lydianAugmented =
    ScaleClass.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


lydianDominant : ScaleClass
lydianDominant =
    ScaleClass.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


mixolydianFlat13 : ScaleClass
mixolydianFlat13 =
    ScaleClass.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


locrianNatural9 : ScaleClass
locrianNatural9 =
    ScaleClass.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


altered : ScaleClass
altered =
    ScaleClass.Heptatonic
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
    ScaleClass.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.minorThird
        }


locrianNatural13 : ScaleClass
locrianNatural13 =
    ScaleClass.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.minorThird
        , sixth = Interval.minorSecond
        }


ionianSharp5 : ScaleClass
ionianSharp5 =
    ScaleClass.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.augmentedSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


dorianSharp11 : ScaleClass
dorianSharp11 =
    ScaleClass.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.augmentedSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


mixolydianFlat9Flat13 : ScaleClass
mixolydianFlat9Flat13 =
    ScaleClass.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.minorThird
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


lydianSharp9 : ScaleClass
lydianSharp9 =
    ScaleClass.Heptatonic
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
    ScaleClass.Hexatonic
        { first = Interval.minorThird
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.minorThird
        }


diminishedWholeToneHalfTone : ScaleClass
diminishedWholeToneHalfTone =
    ScaleClass.Octatonic
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
    ScaleClass.Octatonic
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
    ScaleClass.Hexatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        }



-- Western pentatonic scales


majorPentatonic : ScaleClass
majorPentatonic =
    ScaleClass.Pentatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorThird
        , fourth = Interval.majorSecond
        }


minorPentatonic : ScaleClass
minorPentatonic =
    ScaleClass.Pentatonic
        { first = Interval.minorThird
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorThird
        }


minor6Pentatonic : ScaleClass
minor6Pentatonic =
    ScaleClass.Pentatonic
        { first = Interval.minorThird
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        }


majorFlat6Pentatonic : ScaleClass
majorFlat6Pentatonic =
    ScaleClass.Pentatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorThird
        , fourth = Interval.minorSecond
        }


minorFlat5Pentatonic : ScaleClass
minorFlat5Pentatonic =
    ScaleClass.Pentatonic
        { first = Interval.minorThird
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorThird
        }


majorFlat2Pentatonic : ScaleClass
majorFlat2Pentatonic =
    ScaleClass.Pentatonic
        { first = Interval.minorSecond
        , second = Interval.augmentedSecond
        , third = Interval.minorThird
        , fourth = Interval.majorSecond
        }



-- Synthetic scales


augmented : ScaleClass
augmented =
    ScaleClass.Hexatonic
        { first = Interval.minorThird
        , second = Interval.minorSecond
        , third = Interval.minorThird
        , fourth = Interval.minorSecond
        , fifth = Interval.augmentedSecond
        }


leadingWholeTone : ScaleClass
leadingWholeTone =
    ScaleClass.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


enigmatic : ScaleClass
enigmatic =
    ScaleClass.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.minorThird
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


sixToneSymmetrical : ScaleClass
sixToneSymmetrical =
    ScaleClass.Hexatonic
        { first = Interval.minorSecond
        , second = Interval.augmentedSecond
        , third = Interval.minorSecond
        , fourth = Interval.augmentedSecond
        , fifth = Interval.minorSecond
        }


prometheus : ScaleClass
prometheus =
    ScaleClass.Hexatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorThird
        , fifth = Interval.minorSecond
        }


prometheusNeopolitan : ScaleClass
prometheusNeopolitan =
    ScaleClass.Hexatonic
        { first = Interval.minorSecond
        , second = Interval.minorThird
        , third = Interval.majorSecond
        , fourth = Interval.minorThird
        , fifth = Interval.minorSecond
        }



-- Non-Western scales


arabian : ScaleClass
arabian =
    ScaleClass.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


balinese : ScaleClass
balinese =
    ScaleClass.Pentatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.minorThird
        , fourth = Interval.majorSecond
        }


byzantine : ScaleClass
byzantine =
    ScaleClass.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.minorThird
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.augmentedSecond
        }


chinese : ScaleClass
chinese =
    ScaleClass.Pentatonic
        { first = Interval.majorThird
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorThird
        }


egyptian : ScaleClass
egyptian =
    ScaleClass.Pentatonic
        { first = Interval.majorSecond
        , second = Interval.minorThird
        , third = Interval.majorSecond
        , fourth = Interval.minorThird
        }


eightToneSpanish : ScaleClass
eightToneSpanish =
    ScaleClass.Octatonic
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
    ScaleClass.Pentatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorThird
        , fourth = Interval.minorSecond
        }


hungarianMajor : ScaleClass
hungarianMajor =
    ScaleClass.Heptatonic
        { first = Interval.minorThird
        , second = Interval.minorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


ichikosucho : ScaleClass
ichikosucho =
    ScaleClass.Octatonic
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
    ScaleClass.Pentatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.majorThird
        , fourth = Interval.majorSecond
        }


pelog : ScaleClass
pelog =
    ScaleClass.Pentatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorThird
        , fourth = Interval.minorSecond
        }


persian : ScaleClass
persian =
    ScaleClass.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.minorThird
        , third = Interval.minorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorThird
        }


purviTheta : ScaleClass
purviTheta =
    ScaleClass.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.augmentedSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.augmentedSecond
        }


todiTheta : ScaleClass
todiTheta =
    ScaleClass.Heptatonic
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
    ScaleClass.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.minorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.minorSecond
        }


lydianDiminished : ScaleClass
lydianDiminished =
    ScaleClass.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.minorThird
        , fourth = Interval.minorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


lydianMinor : ScaleClass
lydianMinor =
    ScaleClass.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.minorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }


doubleHarmonicMinor : ScaleClass
doubleHarmonicMinor =
    ScaleClass.Heptatonic
        { first = Interval.majorSecond
        , second = Interval.minorSecond
        , third = Interval.minorThird
        , fourth = Interval.minorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.augmentedSecond
        }


neapolitan : ScaleClass
neapolitan =
    ScaleClass.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.minorThird
        }


neapolitanMajor : ScaleClass
neapolitanMajor =
    ScaleClass.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.majorSecond
        , sixth = Interval.majorSecond
        }


neapolitanMinor : ScaleClass
neapolitanMinor =
    ScaleClass.Heptatonic
        { first = Interval.minorSecond
        , second = Interval.majorSecond
        , third = Interval.majorSecond
        , fourth = Interval.majorSecond
        , fifth = Interval.minorSecond
        , sixth = Interval.majorSecond
        }
