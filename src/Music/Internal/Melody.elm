module Music.Internal.Melody exposing
    ( Fragment
    , FragmentConfig
    , FragmentData
    , Melody
    , ScaleAndChord
    , ScaleStepper
    , addFragment
    , addFragments
    , chordFromFragment
    , currentStepperPitch
    , findMatchingTonesByStepDistance
    , fragment
    , fragmentToPitchList
    , fragmentToStepper
    , getFragments
    , melody
    , moveByScaleSteps
    , moveChromaticallyAfterScaleSteps
    , moveChromaticallyFromCurrentScaleStep
    , repeatLastPitch
    , scaleAndChordFromFragment
    , scaleFromFragment
    , scaleStepper
    , startWithIntervalOffset
    , startsOnChordTone
    , startsOnScaleTone
    , toList
    )

import List.Extra
import List.Zipper exposing (Zipper)
import Music.Internal.Chord as Chord
import Music.Internal.Interval as Interval
import Music.Internal.Octave as Octave
import Music.Internal.Pitch as Pitch
import Music.Internal.Scale as Scale
import Util.Basic



-- Melody


type Melody
    = Melody Fragment (List Fragment)


melody : Fragment -> Melody
melody firstFragment =
    Melody firstFragment []


addFragment : Fragment -> Melody -> Melody
addFragment newFragment (Melody firstFragment tail) =
    Melody firstFragment (tail ++ [ newFragment ])


addFragments : List Fragment -> Melody -> Melody
addFragments newFragments (Melody firstFragment tail) =
    Melody firstFragment (tail ++ newFragments)


startsOnChordTone : Melody -> Bool
startsOnChordTone (Melody firstFragment fragments) =
    fragmentToPitchList firstFragment
        |> List.head
        |> Maybe.map
            (\firstPitch ->
                Chord.containsPitchClass (Pitch.pitchClass firstPitch) (chordFromFragment firstFragment)
            )
        |> Maybe.withDefault False


startsOnScaleTone : Melody -> Bool
startsOnScaleTone (Melody firstFragment fragments) =
    fragmentToPitchList firstFragment
        |> List.head
        |> Maybe.map
            (\firstPitch ->
                Scale.containsPitchClass (Pitch.pitchClass firstPitch) (scaleFromFragment firstFragment) { ignoreSpelling = True }
            )
        |> Maybe.withDefault False



-- Fragment


fragment : FragmentConfig -> Fragment
fragment config =
    { start =
        { scaleDegree = Tuple.first config.startingDegree
        , octave = Tuple.second config.startingDegree
        , adjustByInterval = Interval.perfectUnison
        }
    , movements = []
    , scaleAndChord = config.scaleAndChord
    }
        |> Fragment


startWithIntervalOffset : Interval.Interval -> Fragment -> Fragment
startWithIntervalOffset interval (Fragment theFragment) =
    { theFragment
        | start =
            { scaleDegree = theFragment.start.scaleDegree
            , octave = theFragment.start.octave
            , adjustByInterval = interval
            }
    }
        |> Fragment


type alias FragmentConfig =
    { startingDegree : ( Int, Octave.Octave )
    , scaleAndChord : ScaleAndChord
    }


type Fragment
    = Fragment FragmentData


type alias FragmentData =
    { start : Start
    , movements : List Movement
    , scaleAndChord : ScaleAndChord
    }


type alias Start =
    { scaleDegree : Int
    , adjustByInterval : Interval.Interval
    , octave : Octave.Octave
    }


type alias ScaleAndChord =
    { chord : Chord.Chord
    , scale : Scale.Scale
    }


chordFromFragment : Fragment -> Chord.Chord
chordFromFragment (Fragment fragmentData) =
    fragmentData.scaleAndChord.chord


scaleFromFragment : Fragment -> Scale.Scale
scaleFromFragment (Fragment fragmentData) =
    fragmentData.scaleAndChord.scale


scaleAndChordFromFragment : Fragment -> ScaleAndChord
scaleAndChordFromFragment (Fragment fragmentData) =
    fragmentData.scaleAndChord


getFragments : Melody -> List Fragment
getFragments (Melody firstFragment fragments) =
    firstFragment :: fragments



-- Movements


type Movement
    = MoveByScaleSteps Int
    | MoveByScaleStepsAndAddInterval { transposedBy : Interval.Interval, moveBySteps : Int }
    | RepeatLastPitch


moveByScaleSteps : Int -> Fragment -> Fragment
moveByScaleSteps scaleSteps theFragment =
    MoveByScaleSteps scaleSteps
        |> addMovement theFragment


repeatLastPitch : Fragment -> Fragment
repeatLastPitch theFragment =
    RepeatLastPitch
        |> addMovement theFragment


moveChromaticallyFromCurrentScaleStep : Interval.Interval -> Fragment -> Fragment
moveChromaticallyFromCurrentScaleStep interval theFragment =
    MoveByScaleStepsAndAddInterval { transposedBy = interval, moveBySteps = 0 }
        |> addMovement theFragment


moveChromaticallyAfterScaleSteps : Int -> Interval.Interval -> Fragment -> Fragment
moveChromaticallyAfterScaleSteps scaleSteps interval theFragment =
    MoveByScaleStepsAndAddInterval { transposedBy = interval, moveBySteps = scaleSteps }
        |> addMovement theFragment


addMovement : Fragment -> Movement -> Fragment
addMovement (Fragment fragmentData) movement =
    { fragmentData
        | movements = fragmentData.movements ++ [ movement ]
    }
        |> Fragment



-- Conversion to pitches


toList : Melody -> List Pitch.Pitch
toList (Melody firstFragment fragments) =
    List.concatMap fragmentToPitchList (firstFragment :: fragments)


fragmentToPitchList : Fragment -> List Pitch.Pitch
fragmentToPitchList theFragment =
    fragmentToStepper theFragment
        |> getOutputPitchesFromStepper


fragmentToStepper : Fragment -> ScaleStepper
fragmentToStepper (Fragment fragmentData) =
    scaleStepper fragmentData.scaleAndChord fragmentData.start
        |> (\stepper -> List.foldl addOutputPitchFromMovement stepper fragmentData.movements)


findMatchingTonesByStepDistance : (StepperStep -> Bool) -> Fragment -> Interval.Interval -> List Int
findMatchingTonesByStepDistance isMatchingTone ((Fragment fragmentData) as theFragment) goalInterval =
    let
        maxIntervalSemitoneDeviation =
            2

        intervalSemitoneDeviation theCurrentPitch candidatePitch =
            let
                semitoneDistanceFromCurrentToCandidate =
                    Interval.semitones (Pitch.intervalBetween theCurrentPitch candidatePitch)

                semitoneDistanceInGoalInterval =
                    Interval.semitones goalInterval
            in
            (semitoneDistanceFromCurrentToCandidate - semitoneDistanceInGoalInterval)
                |> abs
    in
    fragmentToStepper theFragment
        |> (\((ScaleStepper zipper outputPitchList) as stepper) ->
                let
                    currentPitch =
                        currentStepperPitch stepper Interval.perfectUnison

                    indicesAfter =
                        zipper
                            |> List.Zipper.after
                            |> List.Extra.findIndices
                                (\candidate ->
                                    isMatchingTone candidate
                                        && (intervalSemitoneDeviation currentPitch candidate.pitch
                                                <= maxIntervalSemitoneDeviation
                                           )
                                )
                            |> List.map ((+) 1)

                    indicesBefore =
                        zipper
                            |> List.Zipper.before
                            |> List.reverse
                            |> List.Extra.findIndices
                                (\candidate ->
                                    isMatchingTone candidate
                                        && (intervalSemitoneDeviation currentPitch candidate.pitch
                                                <= maxIntervalSemitoneDeviation
                                           )
                                )
                            |> List.map ((+) 1)
                            |> List.map ((*) -1)

                    currentIndex =
                        zipper
                            |> List.Zipper.current
                            |> (\candidate ->
                                    if
                                        isMatchingTone candidate
                                            && (intervalSemitoneDeviation currentPitch candidate.pitch
                                                    <= maxIntervalSemitoneDeviation
                                               )
                                    then
                                        [ 0 ]

                                    else
                                        []
                               )
                in
                indicesBefore
                    ++ indicesAfter
                    ++ currentIndex
           )


getOutputPitchesFromStepper : ScaleStepper -> List Pitch.Pitch
getOutputPitchesFromStepper (ScaleStepper zipper outputPitchList) =
    outputPitchList


addOutputPitchFromMovement : Movement -> ScaleStepper -> ScaleStepper
addOutputPitchFromMovement motion ((ScaleStepper zipper outputPitchList) as stepper) =
    let
        ( intervalForChromaticPitches, numberOfScaleSteps ) =
            case motion of
                MoveByScaleSteps int ->
                    ( Interval.perfectUnison, int )

                MoveByScaleStepsAndAddInterval { moveBySteps, transposedBy } ->
                    ( transposedBy, moveBySteps )

                RepeatLastPitch ->
                    ( Interval.perfectUnison, 0 )

        timesToApply =
            abs numberOfScaleSteps
                |> (\val ->
                        if val == 0 then
                            1

                        else
                            val
                   )

        maybeDirectionBool =
            if numberOfScaleSteps == 0 then
                Nothing

            else if numberOfScaleSteps > 0 then
                Just True

            else
                Just False
    in
    case motion of
        RepeatLastPitch ->
            let
                lastOutputPitch =
                    outputPitchList
                        |> List.reverse
                        |> List.take 1
            in
            ScaleStepper zipper (outputPitchList ++ lastOutputPitch)

        _ ->
            Util.Basic.applyNTimes timesToApply (moveByOneScaleStep maybeDirectionBool) stepper
                |> appendToOutputPitchList intervalForChromaticPitches


appendToOutputPitchList : Interval.Interval -> ScaleStepper -> ScaleStepper
appendToOutputPitchList interval ((ScaleStepper zipper outputPitchList) as stepper) =
    let
        newPitch =
            currentStepperPitch stepper interval
    in
    ScaleStepper zipper (outputPitchList ++ [ newPitch ])


moveByOneScaleStep :
    Maybe Bool
    -> ScaleStepper
    -> ScaleStepper
moveByOneScaleStep positiveNegativeOrZero (ScaleStepper zipper outputPitchList) =
    (case positiveNegativeOrZero of
        Just True ->
            List.Zipper.next zipper

        Just False ->
            List.Zipper.previous zipper

        Nothing ->
            Just zipper
    )
        |> Maybe.map
            (\resultZipper ->
                ScaleStepper resultZipper outputPitchList
            )
        |> Maybe.withDefault
            (ScaleStepper zipper outputPitchList)



-- ScaleStepper


type ScaleStepper
    = ScaleStepper (Zipper StepperStep) (List Pitch.Pitch)


type alias StepperStep =
    { pitch : Pitch.Pitch
    , isChordTone : Bool
    }


pitchToScaleStep : Chord.Chord -> Pitch.Pitch -> StepperStep
pitchToScaleStep chord pitch =
    { pitch = pitch
    , isChordTone = Chord.containsPitchClass (Pitch.pitchClass pitch) chord
    }


stepperStepToPitch : StepperStep -> Interval.Interval -> Pitch.Pitch
stepperStepToPitch theStep alterationForNonScaleTone =
    theStep.pitch
        |> Pitch.transposeUp alterationForNonScaleTone


scaleStepper : ScaleAndChord -> Start -> ScaleStepper
scaleStepper { scale, chord } start =
    let
        startingScalePitch =
            Scale.degree start.scaleDegree scale
                |> Pitch.fromPitchClass start.octave

        currentStep : StepperStep
        currentStep =
            { pitch = startingScalePitch
            , isChordTone = Chord.containsPitchClass (Pitch.pitchClass startingScalePitch) chord
            }

        stepperStepList =
            Scale.toListThroughAllOctaves scale
                |> List.map (pitchToScaleStep chord)

        zipper =
            stepperStepList
                |> List.Zipper.fromList
                |> Maybe.andThen (List.Zipper.findFirst (\s -> s.pitch == currentStep.pitch))
                |> Maybe.withDefault (List.Zipper.singleton currentStep)

        pitchOutputList =
            [ stepperStepToPitch currentStep start.adjustByInterval ]
    in
    ScaleStepper zipper pitchOutputList


currentStepperPitch : ScaleStepper -> Interval.Interval -> Pitch.Pitch
currentStepperPitch (ScaleStepper zipper pitchOutputList) interval =
    List.Zipper.current zipper
        |> (\stepper -> stepperStepToPitch stepper interval)
