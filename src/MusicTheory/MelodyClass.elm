module MusicTheory.MelodyClass exposing
    ( fromMelody
    , melodyClass
    , startOnChordTone
    , startOnNonChordTone
    , startOnNonScaleTone
    , toChordToneWithGoalInterval
    , toNonChordToneWithGoalInterval
    , toNonScaleToneWithGoalInterval
    )

--

import List.Extra
import MusicTheory.Chord as Chord
import MusicTheory.Interval as Interval
import MusicTheory.Melody as Melody exposing (Fragment, Melody)
import MusicTheory.Octave as Octave
import MusicTheory.Pitch as Pitch
import MusicTheory.PitchClass as PitchClass
import MusicTheory.Scale as Scale
import Util.ConstraintSolver as ConstraintSolver


type MelodyClass
    = MelodyClass Start (List Movement)


melodyClass : Start -> List Movement -> MelodyClass
melodyClass start steps =
    MelodyClass start steps


type Movement
    = ToChordTone Interval.Interval
    | ToNonChordTone Interval.Interval
    | ToNonScaleTone Interval.Interval


type Start
    = StartOnChordTone
    | StartOnNonChordTone
    | StartOnNonScaleTone


startOnChordTone : Start
startOnChordTone =
    StartOnChordTone


startOnNonChordTone : Start
startOnNonChordTone =
    StartOnNonChordTone


startOnNonScaleTone : Start
startOnNonScaleTone =
    StartOnNonScaleTone


toChordToneWithGoalInterval : Interval.Interval -> Movement
toChordToneWithGoalInterval interval =
    ToChordTone interval


toNonChordToneWithGoalInterval : Interval.Interval -> Movement
toNonChordToneWithGoalInterval interval =
    ToNonChordTone interval


toNonScaleToneWithGoalInterval : Interval.Interval -> Movement
toNonScaleToneWithGoalInterval interval =
    ToNonScaleTone interval


fromMelody : Melody -> MelodyClass
fromMelody melody =
    let
        defineStart =
            if Melody.startsOnChordTone melody then
                startOnChordTone

            else if Melody.startsOnScaleTone melody then
                startOnNonChordTone

            else
                startOnNonScaleTone
    in
    Melody.getFragments melody
        |> toHarmonicContexts
        |> toHarmonicContextWithLast
        |> List.map harmonicContextToMovement
        |> MelodyClass defineStart


type alias HarmonicContext =
    { scale : Scale.Scale
    , chord : Chord.Chord
    , pitch : Pitch.Pitch
    }


toHarmonicContexts : List Melody.Fragment -> List HarmonicContext
toHarmonicContexts fragments =
    List.concatMap
        (\fragment ->
            Melody.fragmentToPitchList fragment
                |> List.map
                    (\pitch ->
                        { scale = Melody.scaleFromFragment fragment
                        , chord = Melody.chordFromFragment fragment
                        , pitch = pitch
                        }
                    )
        )
        fragments


toHarmonicContextWithLast :
    List HarmonicContext
    -> List ( HarmonicContext, HarmonicContext )
toHarmonicContextWithLast chordAndPitches =
    case chordAndPitches of
        head :: tail ->
            List.Extra.zip chordAndPitches tail

        [] ->
            []


harmonicContextToMovement :
    ( HarmonicContext, HarmonicContext )
    -> Movement
harmonicContextToMovement ( last, current ) =
    let
        interval =
            Pitch.intervalBetween last.pitch current.pitch
    in
    if Chord.containsPitchClass (Pitch.pitchClass current.pitch) current.chord then
        ToChordTone interval

    else if Scale.containsPitchClass (Pitch.pitchClass current.pitch) current.scale then
        ToNonChordTone interval

    else
        ToNonScaleTone interval



--type alias Config =
--    { melodyClass : MelodyClass
--    , scalesAndChords : List (Melody.ScaleAndChord )
--    }
--
--
--type alias ConfigInput =
--    { melodyClass : MelodyClass
--    , scalesAndChords : List ( Int, Melody.ScaleAndChord )
--    }
--
--
--init : ConfigInput -> Config
--init input =
--    { melodyClass = input.melodyClass
--    , scalesAndChords = input.scalesAndChords
--    }
--
--
--generate : Config -> List Melody
--generate theConfig =
--    case theConfig.scalesAndChords of
--        ( int, scaleAndChord ) :: tail ->
--            let
--                temporaryScaleStepper =
--                    Melody.scaleStepper scaleAndChord Octave.four 0
--            in
--            ConstraintSolver.solve
--                { problemSetup =
--                    { melodyClass = theConfig.melodyClass
--                    , scaleAndChords = theConfig.scalesAndChords
--                    , scaleStepper = temporaryScaleStepper
--                    , fragments = []
--                    }
--                , getNextSetups = addStep
--                , constraints = [ validateSetup ]
--                , setupToSolution = finalizeMelody
--                }
--                |> .solved
--
--        [] ->
--            []
--
--
--type alias ProblemSetup =
--    { melodyClass : MelodyClass
--    , scaleAndChords : List ( Int, Melody.ScaleAndChord )
--    , scaleStepper : Melody.ScaleStepper
--    , fragments : List Melody.MelodyFragmentData
--    }
--
--
--newStartingDegreesAndOctavesForFragmentWithCurrentPitch :
--    Pitch.Pitch
--    -> MelodyClassStep
--    -> Melody.ScaleAndChord
--    -> List ( Int, Octave.Octave )
--newStartingDegreesAndOctavesForFragmentWithCurrentPitch lastPitch stepToTake scaleAndChord =
--    -- TODO: possible starting degrees and octaves for a new fragment, starting at a current pitch
--    [ ( 0, Octave.four ) ]
--
--
--newStartingDegreesAndOctavesForFragmentWithNoCurrentPitch :
--    MelodyClassStep
--    -> Melody.ScaleAndChord
--    -> List ( Int, Octave.Octave )
--newStartingDegreesAndOctavesForFragmentWithNoCurrentPitch stepToTake scaleAndChord =
--    -- TODO: possible starting degrees and octaves for a new fragment, starting from no first pitch
--    [ ( 0, Octave.four ) ]
--
--
--scaleAndChordToString : Melody.ScaleAndChord -> String
--scaleAndChordToString { scale, chord } =
--    Chord.toPitchClasses chord
--        |> List.reverse
--        |> List.map PitchClass.toString
--        |> String.join " "
--
--
--addStep : ProblemSetup -> List ProblemSetup
--addStep setup =
--    case ( setup.scaleAndChords, melodyClassSteps setup.melodyClass ) of
--        ( ( scaleAndChordCount, scaleAndChordHead ) :: scaleAndChordTail, stepHead :: stepTail ) ->
--            -- scaleAndChords and melodyClass steps are both non-empty
--            case setup.fragments of
--                fragmentsHead :: fragmentsTail ->
--                    -- there is a first output fragment that we may be able to continue
--                    if fragmentsHead.scaleAndChord == scaleAndChordHead then
--                        -- the first output fragment's scaleAndChord matches
--                        -- our current scaleAndChord and we can add a step to it
--                        let
--                            _ =
--                                Debug.log "continue current output fragment" (scaleAndChordToString fragmentsHead.scaleAndChord)
--                        in
--                        continueCurrentFragment
--                            ( fragmentsHead, fragmentsTail )
--                            setup.scaleStepper
--                            ( stepHead, stepTail )
--                            scaleAndChordCount
--                            ( scaleAndChordHead, scaleAndChordTail )
--
--                    else
--                        -- the first output fragment's scaleAndChord doesn't
--                        -- match our current scaleAndChord. we create a list of
--                        -- new fragments from possible next starting pitches.
--                        let
--                            _ =
--                                Debug.log "start output fragment with new scaleAndChord" (scaleAndChordToString fragmentsHead.scaleAndChord)
--
--                            startingDegreesAndOctaves =
--                                -- TODO: this needs parameters for a pitch range
--                                newStartingDegreesAndOctavesForFragmentWithCurrentPitch
--                                    (Melody.currentStepperPitch setup.scaleStepper)
--                                    stepHead
--                                    scaleAndChordHead
--                        in
--                        startingDegreesAndOctaves
--                            |> List.concatMap
--                                (\( startingDegree, startingOctave ) ->
--                                    beginNewFragment
--                                        startingDegree
--                                        startingOctave
--                                        setup.fragments
--                                        stepTail
--                                        scaleAndChordCount
--                                        ( scaleAndChordHead, scaleAndChordTail )
--                                )
--
--                [] ->
--                    -- there is no first output fragment. we need to start it,
--                    -- and we have no current pitch to refer to.
--                    let
--                        _ =
--                            Debug.log "start first output fragment" (scaleAndChordToString scaleAndChordHead)
--
--                        startingDegreesAndOctaves =
--                            -- TODO: this needs parameters for a pitch range
--                            newStartingDegreesAndOctavesForFragmentWithNoCurrentPitch
--                                stepHead
--                                scaleAndChordHead
--                    in
--                    startingDegreesAndOctaves
--                        |> List.concatMap
--                            (\( startingDegree, startingOctave ) ->
--                                beginFirstFragment
--                                    startingDegree
--                                    startingOctave
--                                    setup.fragments
--                                    ( stepHead, stepTail )
--                                    scaleAndChordCount
--                                    ( scaleAndChordHead, scaleAndChordTail )
--                            )
--
--        _ ->
--            -- either scaleAndChords or melodyClass steps (or both) are empty.
--            -- we need both in order to add a step, so we have to stop here.
--            []
--
--
--beginFirstFragment :
--    Int
--    -> Octave.Octave
--    -> List Melody.MelodyFragmentData
--    -> ( MelodyClassStep, List MelodyClassStep )
--    -> Int
--    -> ( Melody.ScaleAndChord, List ( Int, Melody.ScaleAndChord ) )
--    -> List ProblemSetup
--beginFirstFragment startingStep startingOctave fragments ( stepHead, stepTail ) scaleAndChordCount ( scaleAndChordHead, scaleAndChordTail ) =
--    [ { melodyClass =
--            melodyClass StartOnChordTone (stepHead :: stepTail)
--      , scaleStepper =
--            Melody.scaleStepper
--                scaleAndChordHead
--                startingOctave
--                startingStep
--      , scaleAndChords =
--            ( scaleAndChordCount
--            , scaleAndChordHead
--            )
--                :: scaleAndChordTail
--      , fragments =
--            { startingStep = startingStep
--            , startingOctave = startingOctave
--            , steps = [ 0 ]
--            , scaleAndChord = scaleAndChordHead
--            }
--                :: fragments
--      }
--    ]
--
--
--beginNewFragment :
--    Int
--    -> Octave.Octave
--    -> List Melody.MelodyFragmentData
--    -> List MelodyClassStep
--    -> Int
--    -> ( Melody.ScaleAndChord, List ( Int, Melody.ScaleAndChord ) )
--    -> List ProblemSetup
--beginNewFragment startingStep startingOctave fragments stepTail scaleAndChordCount ( scaleAndChordHead, scaleAndChordTail ) =
--    [ { melodyClass =
--            melodyClass StartOnChordTone stepTail
--      , scaleStepper =
--            Melody.scaleStepper
--                scaleAndChordHead
--                startingOctave
--                startingStep
--      , scaleAndChords =
--            decrementScaleAndChords
--                scaleAndChordCount
--                scaleAndChordHead
--                scaleAndChordTail
--      , fragments =
--            { startingStep = startingStep
--            , startingOctave = startingOctave
--            , steps = [ 0 ]
--            , scaleAndChord = scaleAndChordHead
--            }
--                :: fragments
--      }
--    ]
--
--
--continueCurrentFragment :
--    ( Melody.MelodyFragmentData, List Melody.MelodyFragmentData )
--    -> Melody.ScaleStepper
--    -> ( MelodyClassStep, List MelodyClassStep )
--    -> Int
--    -> ( Melody.ScaleAndChord, List ( Int, Melody.ScaleAndChord ) )
--    -> List ProblemSetup
--continueCurrentFragment ( fragmentsHead, fragmentsTail ) currentScaleStepper ( stepHead, stepTail ) scaleAndChordCount ( scaleAndChordHead, scaleAndChordTail ) =
--    [ { melodyClass =
--            melodyClass StartOnChordTone stepTail
--      , scaleAndChords =
--            decrementScaleAndChords
--                scaleAndChordCount
--                scaleAndChordHead
--                scaleAndChordTail
--      , scaleStepper = currentScaleStepper
--      , fragments =
--            { fragmentsHead
--                | steps =
--                    -- TODO: determine which steps will land on chord- and non-chord- tones
--                    fragmentsHead.steps
--                        ++ (case stepHead of
--                                ToChordTone interval ->
--                                    [ 1 ]
--
--                                ToNonChordTone interval ->
--                                    [ -1 ]
--
--                                ToNonScaleTone interval ->
--                                    [ -2 ]
--                           )
--            }
--                :: fragmentsTail
--      }
--    ]
--
--
--decrementScaleAndChords :
--    Int
--    -> Melody.ScaleAndChord
--    -> List ( Int, Melody.ScaleAndChord )
--    -> List ( Int, Melody.ScaleAndChord )
--decrementScaleAndChords scaleAndChordCount scaleAndChordHead scaleAndChordTail =
--    let
--        _ =
--            Debug.log "scaleAndChord"
--                ( ( scaleAndChordCount, scaleAndChordToString scaleAndChordHead )
--                , List.length scaleAndChordTail
--                )
--    in
--    if scaleAndChordCount == 1 then
--        scaleAndChordTail
--
--    else
--        ( scaleAndChordCount - 1, scaleAndChordHead ) :: scaleAndChordTail
--
--
--validateSetup : ProblemSetup -> Result String ProblemSetup
--validateSetup setup =
--    -- TODO: melody should move in correct direction for each step
--    -- TODO: melody should move correctly to chord or scale tone for each step
--    Ok setup
--
--
--finalizeMelody : ProblemSetup -> Result String Melody
--finalizeMelody setup =
--    -- TODO: melody should have correct length
--    List.map Melody.fragment setup.fragments
--        |> List.reverse
--        |> Melody.melody
--        |> Ok
--
--
--
----
