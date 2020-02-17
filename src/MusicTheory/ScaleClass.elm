module MusicTheory.ScaleClass exposing
    ( HeptatonicIntervals
    , HexatonicIntervals
    , OctatonicIntervals
    , PentatonicIntervals
    , ScaleClass(..)
    , ScaleClassIntervals(..)
    , acoustic
    , aeolian
    , aeolianHarmonic
    , all
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
    , toIntervals
    , toScaleClassIntervals
    , ultralocrian
    , wholeTone
    )

import MusicTheory.Interval as Interval exposing (Interval)


type ScaleClass
    = Ionian
    | Dorian
    | Phrygian
    | Lydian
    | Mixolydian
    | Aeolian
    | Locrian
      -- Modes of melodic minor
    | MelodicMinor
    | DorianFlat2
    | LydianAugmented
    | Acoustic
    | MajorMinor
    | MinorLocrian
    | Superlocrian
      -- Modes of harmonic minor
    | HarmonicMinor
    | LocrianNatural6
    | MajorAugmented
    | LydianDiminished
    | PhrygianDominant
    | AeolianHarmonic
    | Ultralocrian
      -- Symmetrical scales
    | DiminishedWholeToneHalfTone
    | DiminishedHalfToneWholeTone
    | WholeTone
      -- Pentatonic scales
    | MajorPentatonic
    | MinorPentatonic


type ScaleClassIntervals
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
    case toScaleClassIntervals theScale of
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


all : List ScaleClass
all =
    [ acoustic
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
    , ultralocrian
    , wholeTone
    ]



-- Modes of major


ionian : ScaleClass
ionian =
    Ionian


dorian : ScaleClass
dorian =
    Dorian


phrygian : ScaleClass
phrygian =
    Phrygian


lydian : ScaleClass
lydian =
    Lydian


mixolydian : ScaleClass
mixolydian =
    Mixolydian


aeolian : ScaleClass
aeolian =
    Aeolian


locrian : ScaleClass
locrian =
    Locrian



-- Modes of melodic minor


melodicMinor : ScaleClass
melodicMinor =
    MelodicMinor


dorianFlat2 : ScaleClass
dorianFlat2 =
    DorianFlat2


lydianAugmented : ScaleClass
lydianAugmented =
    LydianAugmented


acoustic : ScaleClass
acoustic =
    Acoustic


majorMinor : ScaleClass
majorMinor =
    MajorMinor


minorLocrian : ScaleClass
minorLocrian =
    MinorLocrian


superlocrian : ScaleClass
superlocrian =
    Superlocrian



-- Modes of harmonic minor


harmonicMinor : ScaleClass
harmonicMinor =
    HarmonicMinor


locrianNatural6 : ScaleClass
locrianNatural6 =
    LocrianNatural6


majorAugmented : ScaleClass
majorAugmented =
    MajorAugmented


lydianDiminished : ScaleClass
lydianDiminished =
    LydianDiminished


phrygianDominant : ScaleClass
phrygianDominant =
    PhrygianDominant


aeolianHarmonic : ScaleClass
aeolianHarmonic =
    AeolianHarmonic


ultralocrian : ScaleClass
ultralocrian =
    Ultralocrian



-- Symmetrical scales


diminishedWholeToneHalfTone : ScaleClass
diminishedWholeToneHalfTone =
    DiminishedWholeToneHalfTone


diminishedHalfToneWholeTone : ScaleClass
diminishedHalfToneWholeTone =
    DiminishedHalfToneWholeTone


wholeTone : ScaleClass
wholeTone =
    WholeTone



-- Western pentatonic scales


majorPentatonic : ScaleClass
majorPentatonic =
    MajorPentatonic


minorPentatonic : ScaleClass
minorPentatonic =
    MinorPentatonic


toScaleClassIntervals : ScaleClass -> ScaleClassIntervals
toScaleClassIntervals scaleClass =
    case scaleClass of
        Ionian ->
            Heptatonic
                { rootToSecond = Interval.majorSecond
                , rootToThird = Interval.majorThird
                , rootToFourth = Interval.perfectFourth
                , rootToFifth = Interval.perfectFifth
                , rootToSixth = Interval.majorSixth
                , rootToSeventh = Interval.majorSeventh
                }

        Dorian ->
            Heptatonic
                { rootToSecond = Interval.majorSecond
                , rootToThird = Interval.minorThird
                , rootToFourth = Interval.perfectFourth
                , rootToFifth = Interval.perfectFifth
                , rootToSixth = Interval.majorSixth
                , rootToSeventh = Interval.minorSeventh
                }

        Phrygian ->
            Heptatonic
                { rootToSecond = Interval.minorSecond
                , rootToThird = Interval.minorThird
                , rootToFourth = Interval.perfectFourth
                , rootToFifth = Interval.perfectFifth
                , rootToSixth = Interval.minorSixth
                , rootToSeventh = Interval.minorSeventh
                }

        Lydian ->
            Heptatonic
                { rootToSecond = Interval.majorSecond
                , rootToThird = Interval.majorThird
                , rootToFourth = Interval.augmentedFourth
                , rootToFifth = Interval.perfectFifth
                , rootToSixth = Interval.majorSixth
                , rootToSeventh = Interval.majorSeventh
                }

        Mixolydian ->
            Heptatonic
                { rootToSecond = Interval.majorSecond
                , rootToThird = Interval.majorThird
                , rootToFourth = Interval.perfectFourth
                , rootToFifth = Interval.perfectFifth
                , rootToSixth = Interval.majorSixth
                , rootToSeventh = Interval.minorSeventh
                }

        Aeolian ->
            Heptatonic
                { rootToSecond = Interval.majorSecond
                , rootToThird = Interval.minorThird
                , rootToFourth = Interval.perfectFourth
                , rootToFifth = Interval.perfectFifth
                , rootToSixth = Interval.minorSixth
                , rootToSeventh = Interval.minorSeventh
                }

        Locrian ->
            Heptatonic
                { rootToSecond = Interval.minorSecond
                , rootToThird = Interval.minorThird
                , rootToFourth = Interval.perfectFourth
                , rootToFifth = Interval.diminishedFifth
                , rootToSixth = Interval.minorSixth
                , rootToSeventh = Interval.minorSeventh
                }

        MelodicMinor ->
            Heptatonic
                { rootToSecond = Interval.majorSecond
                , rootToThird = Interval.minorThird
                , rootToFourth = Interval.perfectFourth
                , rootToFifth = Interval.diminishedFifth
                , rootToSixth = Interval.majorSixth
                , rootToSeventh = Interval.majorSeventh
                }

        DorianFlat2 ->
            Heptatonic
                { rootToSecond = Interval.minorSecond
                , rootToThird = Interval.minorThird
                , rootToFourth = Interval.perfectFourth
                , rootToFifth = Interval.diminishedFifth
                , rootToSixth = Interval.majorSixth
                , rootToSeventh = Interval.minorSeventh
                }

        LydianAugmented ->
            Heptatonic
                { rootToSecond = Interval.majorSecond
                , rootToThird = Interval.majorThird
                , rootToFourth = Interval.augmentedFourth
                , rootToFifth = Interval.augmentedFifth
                , rootToSixth = Interval.majorSixth
                , rootToSeventh = Interval.majorSeventh
                }

        Acoustic ->
            Heptatonic
                { rootToSecond = Interval.majorSecond
                , rootToThird = Interval.majorThird
                , rootToFourth = Interval.augmentedFourth
                , rootToFifth = Interval.perfectFifth
                , rootToSixth = Interval.majorSixth
                , rootToSeventh = Interval.minorSeventh
                }

        MajorMinor ->
            Heptatonic
                { rootToSecond = Interval.majorSecond
                , rootToThird = Interval.majorThird
                , rootToFourth = Interval.perfectFourth
                , rootToFifth = Interval.perfectFifth
                , rootToSixth = Interval.minorSixth
                , rootToSeventh = Interval.minorSeventh
                }

        MinorLocrian ->
            Heptatonic
                { rootToSecond = Interval.majorSecond
                , rootToThird = Interval.minorThird
                , rootToFourth = Interval.perfectFourth
                , rootToFifth = Interval.diminishedFifth
                , rootToSixth = Interval.minorSixth
                , rootToSeventh = Interval.minorSeventh
                }

        Superlocrian ->
            Heptatonic
                { rootToSecond = Interval.minorSecond
                , rootToThird = Interval.minorThird
                , rootToFourth = Interval.majorThird
                , rootToFifth = Interval.diminishedFifth
                , rootToSixth = Interval.minorSixth
                , rootToSeventh = Interval.minorSeventh
                }

        HarmonicMinor ->
            Heptatonic
                { rootToSecond = Interval.majorSecond
                , rootToThird = Interval.minorThird
                , rootToFourth = Interval.perfectFourth
                , rootToFifth = Interval.perfectFifth
                , rootToSixth = Interval.minorSixth
                , rootToSeventh = Interval.majorSeventh
                }

        LocrianNatural6 ->
            Heptatonic
                { rootToSecond = Interval.minorSecond
                , rootToThird = Interval.minorThird
                , rootToFourth = Interval.perfectFourth
                , rootToFifth = Interval.diminishedFifth
                , rootToSixth = Interval.majorSixth
                , rootToSeventh = Interval.minorSeventh
                }

        MajorAugmented ->
            Heptatonic
                { rootToSecond = Interval.majorSecond
                , rootToThird = Interval.majorThird
                , rootToFourth = Interval.perfectFourth
                , rootToFifth = Interval.augmentedFifth
                , rootToSixth = Interval.majorSixth
                , rootToSeventh = Interval.majorSeventh
                }

        LydianDiminished ->
            Heptatonic
                { rootToSecond = Interval.majorSecond
                , rootToThird = Interval.minorThird
                , rootToFourth = Interval.augmentedFourth
                , rootToFifth = Interval.perfectFifth
                , rootToSixth = Interval.majorSixth
                , rootToSeventh = Interval.minorSeventh
                }

        PhrygianDominant ->
            Heptatonic
                { rootToSecond = Interval.minorSecond
                , rootToThird = Interval.majorThird
                , rootToFourth = Interval.perfectFourth
                , rootToFifth = Interval.perfectFifth
                , rootToSixth = Interval.minorSixth
                , rootToSeventh = Interval.minorSeventh
                }

        AeolianHarmonic ->
            Heptatonic
                { rootToSecond = Interval.augmentedSecond
                , rootToThird = Interval.majorThird
                , rootToFourth = Interval.augmentedFourth
                , rootToFifth = Interval.perfectFifth
                , rootToSixth = Interval.majorSixth
                , rootToSeventh = Interval.majorSeventh
                }

        Ultralocrian ->
            Heptatonic
                { rootToSecond = Interval.minorSecond
                , rootToThird = Interval.minorThird
                , rootToFourth = Interval.diminishedFourth
                , rootToFifth = Interval.diminishedFifth
                , rootToSixth = Interval.minorSixth
                , rootToSeventh = Interval.diminishedSeventh
                }

        DiminishedWholeToneHalfTone ->
            Octatonic
                { rootToSecond = Interval.majorSecond
                , rootToThird = Interval.minorThird
                , rootToFourth = Interval.perfectFourth
                , rootToFifth = Interval.diminishedFifth
                , rootToSixth = Interval.minorSixth
                , rootToSeventh = Interval.majorSixth
                , rootToEighth = Interval.majorSeventh
                }

        DiminishedHalfToneWholeTone ->
            Octatonic
                { rootToSecond = Interval.minorSecond
                , rootToThird = Interval.minorThird
                , rootToFourth = Interval.majorThird
                , rootToFifth = Interval.augmentedFourth
                , rootToSixth = Interval.perfectFifth
                , rootToSeventh = Interval.majorSixth
                , rootToEighth = Interval.minorSeventh
                }

        WholeTone ->
            Hexatonic
                { rootToSecond = Interval.majorSecond
                , rootToThird = Interval.majorThird
                , rootToFourth = Interval.augmentedFourth
                , rootToFifth = Interval.minorSixth
                , rootToSixth = Interval.minorSeventh
                }

        MajorPentatonic ->
            Pentatonic
                { rootToSecond = Interval.majorSecond
                , rootToThird = Interval.majorThird
                , rootToFourth = Interval.perfectFifth
                , rootToFifth = Interval.majorSixth
                }

        MinorPentatonic ->
            Pentatonic
                { rootToSecond = Interval.minorThird
                , rootToThird = Interval.perfectFourth
                , rootToFourth = Interval.perfectFifth
                , rootToFifth = Interval.minorSeventh
                }
