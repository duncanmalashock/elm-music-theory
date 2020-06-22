module MusicTheory.Voicing.FourPart exposing
    ( Pitches
    , Ranges
    , Voicing
    , VoicingClass
    , allFactors
    , allIntervals
    , allRanges
    , allVoices
    , chordToneListToVoicingClass
    , getVoiceFour
    , getVoiceOne
    , getVoiceThree
    , getVoiceTwo
    , toPitches
    )

import MusicTheory.Interval as Interval
import MusicTheory.Pitch as Pitch
import MusicTheory.Voicing as Voicing
import Util.Basic


type alias Voicing =
    Voicing.Voicing VoicingClass



-- Voicing as pitches


type alias Pitches =
    { voiceOne : Pitch.Pitch
    , voiceTwo : Pitch.Pitch
    , voiceThree : Pitch.Pitch
    , voiceFour : Pitch.Pitch
    }


toPitches : Voicing -> Pitches
toPitches theVoicing =
    let
        theRoot =
            Voicing.root theVoicing

        vc =
            Voicing.voicingClass theVoicing
    in
    { voiceOne = Pitch.transposeUp vc.voiceOne theRoot
    , voiceTwo = Pitch.transposeUp vc.voiceTwo theRoot
    , voiceThree = Pitch.transposeUp vc.voiceThree theRoot
    , voiceFour = Pitch.transposeUp vc.voiceFour theRoot
    }


allVoices : List (Voicing -> Pitch.Pitch)
allVoices =
    [ getVoiceOne
    , getVoiceTwo
    , getVoiceThree
    , getVoiceFour
    ]


getVoiceOne : Voicing -> Pitch.Pitch
getVoiceOne =
    toPitches >> .voiceOne


getVoiceTwo : Voicing -> Pitch.Pitch
getVoiceTwo =
    toPitches >> .voiceTwo


getVoiceThree : Voicing -> Pitch.Pitch
getVoiceThree =
    toPitches >> .voiceThree


getVoiceFour : Voicing -> Pitch.Pitch
getVoiceFour =
    toPitches >> .voiceFour



-- Ranges


type alias Ranges =
    { voiceOne : Pitch.Range
    , voiceTwo : Pitch.Range
    , voiceThree : Pitch.Range
    , voiceFour : Pitch.Range
    }


allRanges : List (Ranges -> Pitch.Range)
allRanges =
    [ .voiceOne
    , .voiceTwo
    , .voiceThree
    , .voiceFour
    ]



-- Voicing as chord factors


type alias VoicingClass =
    { voiceOne : Interval.Interval
    , voiceTwo : Interval.Interval
    , voiceThree : Interval.Interval
    , voiceFour : Interval.Interval
    }


allFactors : List (Voicing -> Interval.Interval)
allFactors =
    [ getFactorInVoiceOne
    , getFactorInVoiceTwo
    , getFactorInVoiceThree
    , getFactorInVoiceFour
    ]


getFactorInVoiceOne : Voicing -> Interval.Interval
getFactorInVoiceOne =
    Voicing.voicingClass >> .voiceOne


getFactorInVoiceTwo : Voicing -> Interval.Interval
getFactorInVoiceTwo =
    Voicing.voicingClass >> .voiceTwo


getFactorInVoiceThree : Voicing -> Interval.Interval
getFactorInVoiceThree =
    Voicing.voicingClass >> .voiceThree


getFactorInVoiceFour : Voicing -> Interval.Interval
getFactorInVoiceFour =
    Voicing.voicingClass >> .voiceFour



-- All intervals contained in voicing class


allIntervals : VoicingClass -> AllIntervalsContained
allIntervals vc =
    { fourToOne =
        Interval.subtract vc.voiceFour vc.voiceOne
    , fourToTwo =
        Interval.subtract vc.voiceFour vc.voiceTwo
    , threeToOne =
        Interval.subtract vc.voiceThree vc.voiceOne
    , fourToThree =
        Interval.subtract vc.voiceFour vc.voiceThree
    , threeToTwo =
        Interval.subtract vc.voiceThree vc.voiceTwo
    , twoToOne =
        Interval.subtract vc.voiceTwo vc.voiceOne
    }


type alias AllIntervalsContained =
    { fourToOne : Interval.Interval
    , fourToTwo : Interval.Interval
    , threeToOne : Interval.Interval
    , fourToThree : Interval.Interval
    , threeToTwo : Interval.Interval
    , twoToOne : Interval.Interval
    }



-- Generating voicings


chordToneListToVoicingClass : List Interval.Interval -> Maybe VoicingClass
chordToneListToVoicingClass intervals =
    case intervals of
        i1 :: i2 :: i3 :: i4 :: _ ->
            { voiceOne = i4
            , voiceTwo = i3
            , voiceThree = i2
            , voiceFour = i1
            }
                |> correctIntervalOctaves
                |> Just

        _ ->
            Nothing


correctIntervalOctaves : VoicingClass -> VoicingClass
correctIntervalOctaves { voiceOne, voiceTwo, voiceThree, voiceFour } =
    let
        -- TODO: allow for larger (e.g. 10th) and smaller distances (unison) between voices
        voiceThreeCorrected =
            Util.Basic.while
                (\v3 -> Interval.semitones voiceFour >= Interval.semitones v3)
                Interval.addOctave
                voiceThree

        voiceTwoCorrected =
            Util.Basic.while
                (\v2 -> Interval.semitones voiceThreeCorrected >= Interval.semitones v2)
                Interval.addOctave
                voiceTwo

        voiceOneCorrected =
            Util.Basic.while
                (\v4 -> Interval.semitones voiceTwoCorrected >= Interval.semitones v4)
                Interval.addOctave
                voiceOne
    in
    { voiceOne = voiceOneCorrected
    , voiceTwo = voiceTwoCorrected
    , voiceThree = voiceThreeCorrected
    , voiceFour = voiceFour
    }
