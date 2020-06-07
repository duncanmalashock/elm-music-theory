module MusicTheory.Melody exposing
    ( Melody
    , MelodyFragment
    , fragment
    , melody
    , melodyClass
    , toChordToneWithGoalInterval
    , toList
    , toMelodyClass
    , toNonChordToneWithGoalInterval
    )

import List.Extra
import List.Zipper exposing (Zipper)
import MusicTheory.Chord as Chord
import MusicTheory.Interval as Interval
import MusicTheory.Octave as Octave
import MusicTheory.Pitch as Pitch
import MusicTheory.Scale as Scale
import Util.Basic


type Melody
    = Melody (List MelodyFragment)


type MelodyFragment
    = MelodyFragment MelodyFragmentData


type alias MelodyFragmentData =
    { startingDegree : Int
    , startingOctave : Octave.Octave
    , steps : List Int
    , chord : Chord.Chord
    , scale : Scale.Scale
    }


chordFromFragment : MelodyFragment -> Chord.Chord
chordFromFragment (MelodyFragment fragmentData) =
    fragmentData.chord


melody : List MelodyFragment -> Melody
melody fragments =
    Melody fragments


fragment : MelodyFragmentData -> MelodyFragment
fragment fragmentData =
    MelodyFragment fragmentData


toList : Melody -> List Pitch.Pitch
toList (Melody fragments) =
    List.concatMap fragmentToPitchList fragments


fragmentToPitchList : MelodyFragment -> List Pitch.Pitch
fragmentToPitchList (MelodyFragment { startingDegree, startingOctave, steps, chord, scale }) =
    scaleStepper scale startingOctave startingDegree
        |> generatePitchesFromStepper steps



--


type MelodyClass
    = MelodyClass (List MelodyClassStep)


melodyClass : List MelodyClassStep -> MelodyClass
melodyClass steps =
    MelodyClass steps


type MelodyClassStep
    = ToChordTone Interval.Interval
    | ToNonChordTone Interval.Interval


toChordToneWithGoalInterval : Interval.Interval -> MelodyClassStep
toChordToneWithGoalInterval interval =
    ToChordTone interval


toNonChordToneWithGoalInterval : Interval.Interval -> MelodyClassStep
toNonChordToneWithGoalInterval interval =
    ToNonChordTone interval


toMelodyClass : Melody -> MelodyClass
toMelodyClass (Melody fragments) =
    fragments
        |> toChordAndPitch
        |> toChordAndPitchTuples
        |> List.map pitchToMelodyClassStep
        |> MelodyClass


toChordAndPitch : List MelodyFragment -> List ( Chord.Chord, Pitch.Pitch )
toChordAndPitch fragments =
    List.concatMap
        (\frag ->
            fragmentToPitchList frag
                |> List.map (\pitch -> ( chordFromFragment frag, pitch ))
        )
        fragments


toChordAndPitchTuples :
    List ( Chord.Chord, Pitch.Pitch )
    -> List ( ( Chord.Chord, Pitch.Pitch ), ( Chord.Chord, Pitch.Pitch ) )
toChordAndPitchTuples chordAndPitches =
    case chordAndPitches of
        head :: tail ->
            List.Extra.zip chordAndPitches tail

        [] ->
            []


pitchToMelodyClassStep :
    ( ( Chord.Chord, Pitch.Pitch ), ( Chord.Chord, Pitch.Pitch ) )
    -> MelodyClassStep
pitchToMelodyClassStep ( ( lastChord, lastPitch ), ( currentChord, currentPitch ) ) =
    let
        interval =
            Pitch.intervalBetween lastPitch currentPitch
    in
    if Chord.containsPitchClass (Pitch.pitchClass currentPitch) currentChord then
        ToChordTone interval

    else
        ToNonChordTone interval



--{ startingDegree : Int
--, startingOctave : Octave.Octave
--, steps : List Int
--, chord : Chord.Chord
--, scale : Scale.Scale
--}



--


type ScaleStepper
    = ScaleStepper (Zipper Pitch.Pitch)


scaleStepper : Scale.Scale -> Octave.Octave -> Int -> ScaleStepper
scaleStepper scale startingOctave startingDegree =
    let
        pitch =
            Scale.degree startingDegree scale
                |> Pitch.fromPitchClass startingOctave

        default : Zipper Pitch.Pitch
        default =
            List.Zipper.singleton pitch
    in
    Scale.toListThroughAllOctaves scale
        |> List.Zipper.fromList
        |> Maybe.andThen (List.Zipper.findFirst ((==) pitch))
        |> Maybe.withDefault default
        |> ScaleStepper


generatePitchesFromStepper : List Int -> ScaleStepper -> List Pitch.Pitch
generatePitchesFromStepper steps stepper =
    List.foldl step ( stepper, [] ) steps
        |> Tuple.second


step : Int -> ( ScaleStepper, List Pitch.Pitch ) -> ( ScaleStepper, List Pitch.Pitch )
step numberOfScaleSteps ( ScaleStepper zipper, list ) =
    Util.Basic.applyNTimes
        (abs numberOfScaleSteps
            |> (\val ->
                    if val == 0 then
                        1

                    else
                        val
               )
        )
        (traverseStep
            (if numberOfScaleSteps == 0 then
                Nothing

             else if numberOfScaleSteps > 0 then
                Just True

             else
                Just False
            )
        )
        ( zipper, list )
        |> (\( z, l ) ->
                ( ScaleStepper z
                , list
                    ++ (l
                            |> List.reverse
                            |> List.head
                            |> Maybe.map List.singleton
                            |> Maybe.withDefault []
                       )
                )
           )


traverseStep :
    Maybe Bool
    -> ( Zipper Pitch.Pitch, List Pitch.Pitch )
    -> ( Zipper Pitch.Pitch, List Pitch.Pitch )
traverseStep positiveNegativeOrZero ( zipper, list ) =
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
                ( resultZipper, list ++ [ List.Zipper.current resultZipper ] )
            )
        |> Maybe.withDefault
            ( zipper, list )
