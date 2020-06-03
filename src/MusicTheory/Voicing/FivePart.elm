module MusicTheory.Voicing.FivePart exposing (..)

import MusicTheory.Interval as Interval
import MusicTheory.Pitch as Pitch


voicing : Pitch.Pitch -> VoicingClass -> Voicing
voicing root voicingClass =
    Voicing root voicingClass


type Voicing
    = Voicing Pitch.Pitch VoicingClass


type alias Pitches =
    { voiceOne : Pitch.Pitch
    , voiceTwo : Pitch.Pitch
    , voiceThree : Pitch.Pitch
    , voiceFour : Pitch.Pitch
    , voiceFive : Pitch.Pitch
    }


toPitches : Voicing -> Pitches
toPitches (Voicing root voicingClass) =
    { voiceOne = Pitch.transposeUp voicingClass.voiceOne root
    , voiceTwo = Pitch.transposeUp voicingClass.voiceTwo root
    , voiceThree = Pitch.transposeUp voicingClass.voiceThree root
    , voiceFour = Pitch.transposeUp voicingClass.voiceFour root
    , voiceFive = Pitch.transposeUp voicingClass.voiceFive root
    }


type alias VoicingClass =
    { voiceOne : Interval.Interval
    , voiceTwo : Interval.Interval
    , voiceThree : Interval.Interval
    , voiceFour : Interval.Interval
    , voiceFive : Interval.Interval
    }


allIntervals : VoicingClass -> IntervalList
allIntervals vc =
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


type alias IntervalList =
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
