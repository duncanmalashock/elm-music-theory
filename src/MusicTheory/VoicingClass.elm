module MusicTheory.VoicingClass exposing
    ( FivePartVoicingClass
    , FourPartVoicingClass
    , ThreePartVoicingClass
    , allIntervalsFivePart
    , allIntervalsFourPart
    , allIntervalsThreePart
    )

import MusicTheory.Interval as Interval


type alias ThreePartVoicingClass =
    { voiceOne : Interval.Interval
    , voiceTwo : Interval.Interval
    , voiceThree : Interval.Interval
    }


type alias FourPartVoicingClass =
    { voiceOne : Interval.Interval
    , voiceTwo : Interval.Interval
    , voiceThree : Interval.Interval
    , voiceFour : Interval.Interval
    }


type alias FivePartVoicingClass =
    { voiceOne : Interval.Interval
    , voiceTwo : Interval.Interval
    , voiceThree : Interval.Interval
    , voiceFour : Interval.Interval
    , voiceFive : Interval.Interval
    }


type alias ThreePartIntervalList =
    { voiceThreeToVoiceOne : Interval.Interval
    , voiceThreeToVoiceTwo : Interval.Interval
    , voiceTwoToVoiceOne : Interval.Interval
    }


type alias FourPartIntervalList =
    { voiceFourToVoiceOne : Interval.Interval
    , voiceFourToVoiceTwo : Interval.Interval
    , voiceThreeToVoiceOne : Interval.Interval
    , voiceFourToVoiceThree : Interval.Interval
    , voiceThreeToVoiceTwo : Interval.Interval
    , voiceTwoToVoiceOne : Interval.Interval
    }


type alias FivePartIntervalList =
    { voiceFiveToVoiceOne : Interval.Interval
    , voiceFiveToVoiceTwo : Interval.Interval
    , voiceFourToVoiceOne : Interval.Interval
    , voiceFiveToVoiceThree : Interval.Interval
    , voiceFourToVoiceTwo : Interval.Interval
    , voiceThreeToVoiceOne : Interval.Interval
    , voiceFiveToVoiceFour : Interval.Interval
    , voiceFourToVoiceThree : Interval.Interval
    , voiceThreeToVoiceTwo : Interval.Interval
    , voiceTwoToVoiceOne : Interval.Interval
    }


allIntervalsThreePart : ThreePartVoicingClass -> ThreePartIntervalList
allIntervalsThreePart vc =
    { voiceThreeToVoiceOne =
        Interval.subtract vc.voiceThree vc.voiceOne
    , voiceThreeToVoiceTwo =
        Interval.subtract vc.voiceThree vc.voiceTwo
    , voiceTwoToVoiceOne =
        Interval.subtract vc.voiceTwo vc.voiceOne
    }


allIntervalsFourPart : FourPartVoicingClass -> FourPartIntervalList
allIntervalsFourPart vc =
    { voiceFourToVoiceOne =
        Interval.subtract vc.voiceFour vc.voiceOne
    , voiceFourToVoiceTwo =
        Interval.subtract vc.voiceFour vc.voiceTwo
    , voiceThreeToVoiceOne =
        Interval.subtract vc.voiceThree vc.voiceOne
    , voiceFourToVoiceThree =
        Interval.subtract vc.voiceFour vc.voiceThree
    , voiceThreeToVoiceTwo =
        Interval.subtract vc.voiceThree vc.voiceTwo
    , voiceTwoToVoiceOne =
        Interval.subtract vc.voiceTwo vc.voiceOne
    }


allIntervalsFivePart : FivePartVoicingClass -> FivePartIntervalList
allIntervalsFivePart vc =
    { voiceFiveToVoiceOne =
        Interval.subtract vc.voiceFive vc.voiceOne
    , voiceFiveToVoiceTwo =
        Interval.subtract vc.voiceFive vc.voiceTwo
    , voiceFourToVoiceOne =
        Interval.subtract vc.voiceFour vc.voiceOne
    , voiceFiveToVoiceThree =
        Interval.subtract vc.voiceFive vc.voiceThree
    , voiceFourToVoiceTwo =
        Interval.subtract vc.voiceFour vc.voiceTwo
    , voiceThreeToVoiceOne =
        Interval.subtract vc.voiceThree vc.voiceOne
    , voiceFiveToVoiceFour =
        Interval.subtract vc.voiceFive vc.voiceFour
    , voiceFourToVoiceThree =
        Interval.subtract vc.voiceFour vc.voiceThree
    , voiceThreeToVoiceTwo =
        Interval.subtract vc.voiceThree vc.voiceTwo
    , voiceTwoToVoiceOne =
        Interval.subtract vc.voiceTwo vc.voiceOne
    }
