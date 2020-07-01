module MusicTheory.Voicing.FourPart exposing
    ( Pitches
    , Ranges
    , VoiceIntervalLimits
    , Voicing
    , VoicingClass
    , adjustToBeAboveMinimum
    , allFactors
    , allIntervals
    , allRanges
    , allVoices
    , getVoiceFour
    , getVoiceOne
    , getVoiceThree
    , getVoiceTwo
    , placeFactors
    , toPitches
    )

import MusicTheory.Interval as Interval
import MusicTheory.Pitch as Pitch
import MusicTheory.Voicing as Voicing
import MusicTheory.VoicingClass as VoicingClass
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


placeFactors : VoiceIntervalLimits -> VoicingClass -> List VoicingClass
placeFactors intervalLimits voicingClass =
    let
        voiceFourInitial =
            Interval.toSimple voicingClass.voiceFour

        voiceThreeInitial =
            adjustToBeAboveMinimum
                { minimum = intervalLimits.fourToThree.min
                , lower = voiceFourInitial
                , upper = Interval.toSimple voicingClass.voiceThree
                }

        voiceTwoInitial =
            adjustToBeAboveMinimum
                { minimum = intervalLimits.threeToTwo.min
                , lower = voiceThreeInitial
                , upper = Interval.toSimple voicingClass.voiceTwo
                }

        voiceOneInitial =
            adjustToBeAboveMinimum
                { minimum = intervalLimits.twoToOne.min
                , lower = voiceTwoInitial
                , upper = Interval.toSimple voicingClass.voiceOne
                }

        fourToThreeOctaveShifts =
            octaveShiftsAllowedBetween
                { maximum = intervalLimits.fourToThree.max
                , lower = voiceFourInitial
                , upper = voiceThreeInitial
                }

        threeToTwoOctaveShifts =
            octaveShiftsAllowedBetween
                { maximum = intervalLimits.threeToTwo.max
                , lower = voiceThreeInitial
                , upper = voiceTwoInitial
                }

        twoToOneOctaveShifts =
            octaveShiftsAllowedBetween
                { maximum = intervalLimits.twoToOne.max
                , lower = voiceTwoInitial
                , upper = voiceOneInitial
                }
    in
    { voiceOne = voiceOneInitial
    , voiceTwo = voiceTwoInitial
    , voiceThree = voiceThreeInitial
    , voiceFour = voiceFourInitial
    }
        |> validateInitialVoicingClass intervalLimits
        |> Maybe.map
            (performAllowedOctaveShifts
                { shiftsForVoiceThree = fourToThreeOctaveShifts
                , shiftsForVoiceTwo = threeToTwoOctaveShifts
                , shiftsForVoiceOne = twoToOneOctaveShifts
                }
            )
        |> Maybe.withDefault []


validateInitialVoicingClass : VoiceIntervalLimits -> VoicingClass -> Maybe VoicingClass
validateInitialVoicingClass intervalLimits voicingClass =
    let
        differenceMeetsMinimum l u minimum =
            Interval.semitones (Interval.subtract l u) >= Interval.semitones minimum

        differenceMeetsMaximum l u maximum =
            Interval.semitones (Interval.subtract l u) <= Interval.semitones maximum
    in
    if
        List.all identity
            [ differenceMeetsMinimum voicingClass.voiceFour voicingClass.voiceThree intervalLimits.fourToThree.min
            , differenceMeetsMaximum voicingClass.voiceFour voicingClass.voiceThree intervalLimits.fourToThree.max
            , differenceMeetsMinimum voicingClass.voiceThree voicingClass.voiceTwo intervalLimits.threeToTwo.min
            , differenceMeetsMaximum voicingClass.voiceThree voicingClass.voiceTwo intervalLimits.threeToTwo.max
            , differenceMeetsMinimum voicingClass.voiceTwo voicingClass.voiceOne intervalLimits.twoToOne.min
            , differenceMeetsMaximum voicingClass.voiceTwo voicingClass.voiceOne intervalLimits.twoToOne.max
            ]
    then
        Just voicingClass

    else
        Nothing


adjustToBeAboveMinimum :
    { minimum : Interval.Interval
    , lower : Interval.Interval
    , upper : Interval.Interval
    }
    -> Interval.Interval
adjustToBeAboveMinimum { minimum, lower, upper } =
    let
        lowerIsAboveUpper l u =
            Interval.semitones (Interval.subtract l u) < 0

        differenceIsBelowMinimum l u =
            Interval.semitones (Interval.subtract l u) < Interval.semitones minimum
    in
    upper
        |> Util.Basic.while
            (\currentUpper ->
                lowerIsAboveUpper lower currentUpper
                    || differenceIsBelowMinimum lower currentUpper
            )
            Interval.addOctave


octaveShiftsAllowedBetween :
    { maximum : Interval.Interval
    , lower : Interval.Interval
    , upper : Interval.Interval
    }
    -> Int
octaveShiftsAllowedBetween { maximum, lower, upper } =
    let
        difference l u =
            Interval.semitones (Interval.subtract l u)

        differenceIsBelowMaximum l u =
            difference l u < Interval.semitones maximum

        maximumIsAtLeastCurrentDifferencePlusAnOctave l u =
            Interval.semitones maximum >= (Interval.semitones Interval.perfectOctave + difference l u)
    in
    ( 0, upper )
        |> Util.Basic.while
            (\( count, currentUpper ) ->
                differenceIsBelowMaximum lower currentUpper
                    && maximumIsAtLeastCurrentDifferencePlusAnOctave lower currentUpper
            )
            (\( count, shouldBeAbove ) ->
                ( count + 1, Interval.addOctave shouldBeAbove )
            )
        |> (\( count, shouldBeAbove ) -> count)


performAllowedOctaveShifts :
    { shiftsForVoiceThree : Int
    , shiftsForVoiceTwo : Int
    , shiftsForVoiceOne : Int
    }
    -> VoicingClass
    -> List VoicingClass
performAllowedOctaveShifts { shiftsForVoiceThree, shiftsForVoiceTwo, shiftsForVoiceOne } initialVoicingClass =
    [ initialVoicingClass ]
        |> shiftFromVoiceThree shiftsForVoiceThree
        |> shiftFromVoiceTwo shiftsForVoiceTwo
        |> shiftFromVoiceOne shiftsForVoiceOne


shiftFromVoiceThree : Int -> List VoicingClass -> List VoicingClass
shiftFromVoiceThree shiftCount voicingClasses =
    let
        doShifts : List Interval.Interval -> VoicingClass -> List VoicingClass
        doShifts intervalsToShift vc =
            List.map
                (\shift ->
                    { vc
                        | voiceOne = vc.voiceOne |> Interval.add shift
                        , voiceTwo = vc.voiceTwo |> Interval.add shift
                        , voiceThree = vc.voiceThree |> Interval.add shift
                    }
                )
                intervalsToShift
    in
    voicingClasses
        |> List.concatMap
            (doShifts (shiftCountToIntervalList shiftCount))


shiftFromVoiceTwo : Int -> List VoicingClass -> List VoicingClass
shiftFromVoiceTwo shiftCount voicingClasses =
    let
        doShifts : List Interval.Interval -> VoicingClass -> List VoicingClass
        doShifts intervalsToShift vc =
            List.map
                (\shift ->
                    { vc
                        | voiceOne = vc.voiceOne |> Interval.add shift
                        , voiceTwo = vc.voiceTwo |> Interval.add shift
                    }
                )
                intervalsToShift
    in
    voicingClasses
        |> List.concatMap
            (doShifts (shiftCountToIntervalList shiftCount))


shiftFromVoiceOne : Int -> List VoicingClass -> List VoicingClass
shiftFromVoiceOne shiftCount voicingClasses =
    let
        doShifts : List Interval.Interval -> VoicingClass -> List VoicingClass
        doShifts intervalsToShift vc =
            List.map
                (\shift ->
                    { vc
                        | voiceOne = vc.voiceOne |> Interval.add shift
                    }
                )
                intervalsToShift
    in
    voicingClasses
        |> List.concatMap
            (doShifts (shiftCountToIntervalList shiftCount))


shiftCountToIntervalList : Int -> List Interval.Interval
shiftCountToIntervalList count =
    List.map
        (\currentCount ->
            Util.Basic.applyNTimes currentCount Interval.addOctave Interval.perfectUnison
        )
        (List.range 0 count)


type alias VoiceIntervalLimits =
    { twoToOne : VoicingClass.IntervalRange
    , threeToTwo : VoicingClass.IntervalRange
    , fourToThree : VoicingClass.IntervalRange
    }
