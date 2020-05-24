module MusicTheory.Voicing exposing
    ( FivePartVoicing
    , FourPartVoicing
    , PitchesFivePart
    , PitchesFourPart
    , PitchesThreePart
    , ThreePartVoicing
    , fivePart
    , fourPart
    , fourPartToList
    , threePart
    , toPitchesFivePart
    , toPitchesFourPart
    , toPitchesThreePart
    )

import MusicTheory.Interval as Interval
import MusicTheory.Pitch as Pitch
import MusicTheory.VoicingClass as VoicingClass


type ThreePartVoicing
    = ThreePartVoicing Pitch.Pitch VoicingClass.ThreePartVoicingClass


threePart : Pitch.Pitch -> VoicingClass.ThreePartVoicingClass -> ThreePartVoicing
threePart root voicingClass =
    ThreePartVoicing root voicingClass


fourPart : Pitch.Pitch -> VoicingClass.FourPartVoicingClass -> FourPartVoicing
fourPart root voicingClass =
    FourPartVoicing root voicingClass


fivePart : Pitch.Pitch -> VoicingClass.FivePartVoicingClass -> FivePartVoicing
fivePart root voicingClass =
    FivePartVoicing root voicingClass


type FourPartVoicing
    = FourPartVoicing Pitch.Pitch VoicingClass.FourPartVoicingClass


type FivePartVoicing
    = FivePartVoicing Pitch.Pitch VoicingClass.FivePartVoicingClass


type alias PitchesThreePart =
    { voiceOne : Pitch.Pitch
    , voiceTwo : Pitch.Pitch
    , voiceThree : Pitch.Pitch
    }


type alias PitchesFourPart =
    { voiceOne : Pitch.Pitch
    , voiceTwo : Pitch.Pitch
    , voiceThree : Pitch.Pitch
    , voiceFour : Pitch.Pitch
    }


type alias PitchesFivePart =
    { voiceOne : Pitch.Pitch
    , voiceTwo : Pitch.Pitch
    , voiceThree : Pitch.Pitch
    , voiceFour : Pitch.Pitch
    , voiceFive : Pitch.Pitch
    }


toPitchesThreePart : ThreePartVoicing -> PitchesThreePart
toPitchesThreePart (ThreePartVoicing root voicingClass) =
    { voiceOne = Pitch.transposeUp voicingClass.voiceOne root
    , voiceTwo = Pitch.transposeUp voicingClass.voiceTwo root
    , voiceThree = Pitch.transposeUp voicingClass.voiceThree root
    }


toPitchesFourPart : FourPartVoicing -> PitchesFourPart
toPitchesFourPart (FourPartVoicing root voicingClass) =
    { voiceOne = Pitch.transposeUp voicingClass.voiceOne root
    , voiceTwo = Pitch.transposeUp voicingClass.voiceTwo root
    , voiceThree = Pitch.transposeUp voicingClass.voiceThree root
    , voiceFour = Pitch.transposeUp voicingClass.voiceFour root
    }


fourPartToList : PitchesFourPart -> List Pitch.Pitch
fourPartToList { voiceOne, voiceTwo, voiceThree, voiceFour } =
    [ voiceOne, voiceTwo, voiceThree, voiceFour ]


toPitchesFivePart : FivePartVoicing -> PitchesFivePart
toPitchesFivePart (FivePartVoicing root voicingClass) =
    { voiceOne = Pitch.transposeUp voicingClass.voiceOne root
    , voiceTwo = Pitch.transposeUp voicingClass.voiceTwo root
    , voiceThree = Pitch.transposeUp voicingClass.voiceThree root
    , voiceFour = Pitch.transposeUp voicingClass.voiceFour root
    , voiceFive = Pitch.transposeUp voicingClass.voiceFive root
    }


type alias LowIntervalLimit =
    { intervalInSemitones : Interval.Interval
    , lowestAllowedPitch : Pitch.Pitch
    }


lowIntervalLimitForInterval : Interval.Interval -> Pitch.Pitch -> LowIntervalLimit
lowIntervalLimitForInterval theInterval thePitch =
    LowIntervalLimit theInterval thePitch


lowIntervalLimits : List LowIntervalLimit
lowIntervalLimits =
    [ lowIntervalLimitForInterval Interval.minorSecond Pitch.e3
    , lowIntervalLimitForInterval Interval.majorSecond Pitch.eFlat3
    , lowIntervalLimitForInterval Interval.minorThird Pitch.c3
    , lowIntervalLimitForInterval Interval.majorThird Pitch.bFlat2
    , lowIntervalLimitForInterval Interval.perfectFourth Pitch.bFlat2
    , lowIntervalLimitForInterval Interval.augmentedFourth Pitch.bFlat2
    , lowIntervalLimitForInterval Interval.perfectFifth Pitch.bFlat1
    , lowIntervalLimitForInterval Interval.minorSixth Pitch.g2
    , lowIntervalLimitForInterval Interval.majorSixth Pitch.f2
    , lowIntervalLimitForInterval Interval.minorSeventh Pitch.f2
    , lowIntervalLimitForInterval Interval.majorSeventh Pitch.f2
    , lowIntervalLimitForInterval Interval.minorNinth Pitch.e2
    , lowIntervalLimitForInterval Interval.majorNinth Pitch.eFlat2
    , lowIntervalLimitForInterval Interval.minorTenth Pitch.c2
    , lowIntervalLimitForInterval Interval.majorTenth Pitch.bFlat1
    ]
