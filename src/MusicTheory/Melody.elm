module MusicTheory.Melody exposing
    ( Fragment
    , Melody
    , fragment
    , melody
    , toList
    )

import List.Zipper exposing (Zipper)
import MusicTheory.Chord as Chord
import MusicTheory.Octave as Octave
import MusicTheory.Pitch as Pitch
import MusicTheory.Scale as Scale
import Util.Basic


type Melody
    = Melody (List Fragment)


type Fragment
    = Fragment FragmentData


type alias FragmentData =
    { start : ( Octave.Octave, Int )
    , steps : List Int
    , chord : Chord.Chord
    , scale : Scale.Scale
    }


melody : List Fragment -> Melody
melody fragments =
    Melody fragments


fragment : FragmentData -> Fragment
fragment fragmentData =
    Fragment fragmentData


toList : Melody -> List Pitch.Pitch
toList (Melody fragments) =
    List.concatMap
        (\(Fragment { start, steps, chord, scale }) ->
            scaleStepper scale (Tuple.first start) (Tuple.second start)
                |> generatePitchesFromStepper steps
        )
        fragments



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
