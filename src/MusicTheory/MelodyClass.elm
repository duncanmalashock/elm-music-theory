module MusicTheory.MelodyClass exposing
    ( config
    , fromMelody
    , generateMelodies
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
import MusicTheory.Melody as Melody exposing (Melody)
import MusicTheory.Octave as Octave
import MusicTheory.Pitch as Pitch
import MusicTheory.Scale as Scale
import Util.ConstraintSolver as ConstraintSolver


type MelodyClass
    = MelodyClass Start (List Movement)


melodyClass : Start -> List Movement -> MelodyClass
melodyClass start theMovements =
    MelodyClass start theMovements


getStart : MelodyClass -> Start
getStart (MelodyClass theStart _) =
    theStart


length : MelodyClass -> Int
length (MelodyClass start theMovements) =
    List.length theMovements + 1


movements : MelodyClass -> List Movement
movements (MelodyClass _ theMovements) =
    theMovements


type Start
    = StartOnChordTone
    | StartOnNonChordTone
    | StartOnNonScaleTone


type Movement
    = ToChordTone Interval.Interval
    | ToNonChordTone Interval.Interval
    | ToNonScaleTone Interval.Interval


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



-- Generating melodies


type alias Config =
    { melodyClass : MelodyClass
    , scalesAndChords : ( Melody.ScaleAndChord, List Melody.ScaleAndChord )
    }


type alias ConfigInput =
    { melodyClass : MelodyClass
    , scalesAndChords : ( Melody.ScaleAndChord, List ( Int, Melody.ScaleAndChord ) )
    }


config : ConfigInput -> Config
config input =
    { melodyClass = input.melodyClass
    , scalesAndChords =
        case input.scalesAndChords of
            ( firstScaleAndChord, rest ) ->
                ( firstScaleAndChord
                , List.concatMap
                    (\( number, scaleAndChord ) ->
                        List.repeat number scaleAndChord
                    )
                    rest
                )
    }


generateMelodies : Config -> List Melody
generateMelodies theConfig =
    List.map
        (\setup ->
            { problemSetup = setup
            , getNextSetups = getNextSetups
            , constraints = solverConstraints theConfig
            , setupToSolution = setupToSolution
            }
        )
        (configToProblemSetups theConfig)
        |> List.concatMap (ConstraintSolver.solve >> .solved)


configToProblemSetups : Config -> List ProblemSetup
configToProblemSetups theConfig =
    case theConfig.scalesAndChords of
        ( first, rest ) ->
            List.map
                (\fragments ->
                    { fragments = ( fragments, [] )
                    , remainingMovementsToProcess =
                        List.map2
                            Tuple.pair
                            (movements theConfig.melodyClass)
                            rest
                    , totalPitchesGoal = length theConfig.melodyClass
                    }
                )
                (startFragments (getStart theConfig.melodyClass) first)


type alias ProblemSetup =
    { fragments : ( Melody.Fragment, List Melody.Fragment )
    , remainingMovementsToProcess : List ( Movement, Melody.ScaleAndChord )
    , totalPitchesGoal : Int
    }


startFragments : Start -> Melody.ScaleAndChord -> List Melody.Fragment
startFragments theStart scaleAndChord =
    degreesForStart theStart scaleAndChord
        |> List.map
            (\degree ->
                Melody.fragment
                    { startingDegree = ( degree, Octave.four )
                    , scaleAndChord = scaleAndChord
                    }
            )


degreesForStart : Start -> Melody.ScaleAndChord -> List Int
degreesForStart theStart { scale, chord } =
    let
        chordDegrees =
            Scale.toList scale
                |> List.indexedMap
                    (\index degree ->
                        if Chord.containsPitchClass degree chord then
                            Just <| index + 1

                        else
                            Nothing
                    )
                |> List.filterMap identity

        scaleDegrees =
            Scale.toList scale
                |> List.indexedMap
                    (\index degree ->
                        if Chord.containsPitchClass degree chord then
                            Nothing

                        else
                            Just <| index + 1
                    )
                |> List.filterMap identity
    in
    case theStart of
        StartOnChordTone ->
            chordDegrees

        StartOnNonChordTone ->
            scaleDegrees

        StartOnNonScaleTone ->
            []


getNextSetups : ProblemSetup -> List ProblemSetup
getNextSetups theProblemSetup =
    let
        newProblemSetup remainingMovements newFragment =
            { theProblemSetup
                | fragments =
                    case theProblemSetup.fragments of
                        ( first, rest ) ->
                            ( first, rest ++ [ newFragment ] )
                , remainingMovementsToProcess = remainingMovements
            }
    in
    case theProblemSetup.remainingMovementsToProcess of
        [] ->
            []

        ( firstMovement, firstChordAndScale ) :: tail ->
            if Melody.scaleAndChordFromFragment (currentFragment theProblemSetup) == firstChordAndScale then
                -- append to the current fragment
                let
                    theCurrentFragment =
                        currentFragment theProblemSetup
                in
                case firstMovement of
                    ToChordTone interval ->
                        Melody.findMatchingTonesByStepDistance .isChordTone theCurrentFragment interval
                            |> List.map
                                (\scaleSteps ->
                                    updateCurrentFragment theProblemSetup
                                        tail
                                        (theCurrentFragment
                                            |> Melody.moveByScaleSteps scaleSteps
                                        )
                                )

                    ToNonChordTone interval ->
                        Melody.findMatchingTonesByStepDistance (.isChordTone >> not) theCurrentFragment interval
                            |> List.map
                                (\scaleSteps ->
                                    updateCurrentFragment theProblemSetup
                                        tail
                                        (theCurrentFragment
                                            |> Melody.moveByScaleSteps scaleSteps
                                        )
                                )

                    ToNonScaleTone interval ->
                        --scaleStepsToNonScaleTones
                        []
                            |> List.map
                                (\scaleSteps ->
                                    updateCurrentFragment theProblemSetup
                                        tail
                                        (theCurrentFragment
                                            |> Melody.moveByScaleSteps scaleSteps
                                        )
                                )

            else
                -- create a new fragment
                case firstMovement of
                    ToChordTone interval ->
                        startFragments StartOnChordTone firstChordAndScale
                            |> List.map (newProblemSetup tail)

                    ToNonChordTone interval ->
                        startFragments StartOnNonChordTone firstChordAndScale
                            |> List.map (newProblemSetup tail)

                    ToNonScaleTone interval ->
                        startFragments StartOnNonScaleTone firstChordAndScale
                            |> List.map (newProblemSetup tail)


currentFragment : ProblemSetup -> Melody.Fragment
currentFragment { fragments } =
    case fragments of
        ( firstFragment, [] ) ->
            firstFragment

        ( firstFragment, rest ) ->
            List.reverse rest
                |> List.head
                |> Maybe.withDefault firstFragment


updateCurrentFragment : ProblemSetup -> List ( Movement, Melody.ScaleAndChord ) -> Melody.Fragment -> ProblemSetup
updateCurrentFragment ({ fragments } as theSetup) remainingMovements newFragment =
    case fragments of
        ( firstFragment, [] ) ->
            { theSetup
                | fragments = ( newFragment, [] )
                , remainingMovementsToProcess = remainingMovements
            }

        ( firstFragment, rest ) ->
            { theSetup
                | fragments =
                    ( firstFragment
                    , List.reverse rest
                        |> (\theRest ->
                                case theRest of
                                    head :: tail ->
                                        newFragment :: tail

                                    [] ->
                                        []
                           )
                        |> List.reverse
                    )
                , remainingMovementsToProcess = remainingMovements
            }


solverConstraints : Config -> List (ProblemSetup -> Result String ProblemSetup)
solverConstraints theConfig =
    [ Ok
    ]


setupToSolution : ProblemSetup -> Result String Melody
setupToSolution setup =
    let
        melody =
            case setup.fragments of
                ( first, rest ) ->
                    Melody.melody first
                        |> Melody.addFragments rest

        numberOfPitches =
            Melody.toList melody
                |> List.length

        hasCorrectNumberOfPitches =
            numberOfPitches == setup.totalPitchesGoal
    in
    if hasCorrectNumberOfPitches then
        Ok melody

    else
        Err
            ("expected "
                ++ String.fromInt setup.totalPitchesGoal
                ++ " pitches in output, but got "
                ++ String.fromInt numberOfPitches
            )
