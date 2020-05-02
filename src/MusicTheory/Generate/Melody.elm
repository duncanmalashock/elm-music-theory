module MusicTheory.Generate.Melody exposing
    ( current
    , generate
    , init
    , length
    )

import List.Zipper exposing (Zipper)
import MusicTheory.Octave as Octave
import MusicTheory.Pitch as Pitch exposing (Pitch)
import MusicTheory.Scale as Scale exposing (Scale)
import Util.Basic


type ScaleStepper
    = ScaleStepper (Zipper Pitch)


init : Scale -> Pitch -> ScaleStepper
init scale pitch =
    let
        default : Zipper Pitch
        default =
            List.Zipper.singleton pitch
    in
    Octave.allValid
        |> List.concatMap
            (\octave ->
                Scale.toList scale
                    |> List.map (Pitch.fromPitchClass octave)
            )
        |> List.Zipper.fromList
        |> Maybe.andThen (List.Zipper.findFirst ((==) pitch))
        |> Maybe.withDefault default
        |> ScaleStepper


generate : List Int -> ScaleStepper -> List Pitch
generate steps stepper =
    List.foldl step ( stepper, [] ) steps
        |> Tuple.second


step : Int -> ( ScaleStepper, List Pitch ) -> ( ScaleStepper, List Pitch )
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
        ( zipper
        , list
        )
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


traverseStep : Maybe Bool -> ( Zipper Pitch, List Pitch ) -> ( Zipper Pitch, List Pitch )
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


length : ScaleStepper -> Int
length (ScaleStepper zipper) =
    List.Zipper.toList zipper
        |> List.length


current : ScaleStepper -> Pitch
current (ScaleStepper zipper) =
    List.Zipper.current zipper
