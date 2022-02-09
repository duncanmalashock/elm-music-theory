module Music.Internal.ScaleStepper exposing (ScaleStepper, current, init)

import Music.Internal.Pitch as Pitch
import Music.Internal.Scale as Scale


type ScaleStepper
    = ScaleStepper Details


type alias Details =
    { selection : Pitch.Pitch
    , beforeSelection : List Pitch.Pitch
    , afterSelection : List Pitch.Pitch
    , scale : Scale.Scale
    }


init : Pitch.Pitch -> Scale.Scale -> ScaleStepper
init initPitch scale =
    Scale.toPitches scale
        |> splitOnInitPitch initPitch
        |> fromSplit scale


splitOnInitPitch :
    Pitch.Pitch
    -> List Pitch.Pitch
    ->
        { selection : Pitch.Pitch
        , beforeSelection : List Pitch.Pitch
        , afterSelection : List Pitch.Pitch
        }
splitOnInitPitch initPitch pitchList =
    let
        selection : Pitch.Pitch
        selection =
            Pitch.sortClosest initPitch pitchList
                |> List.head
                -- this is exceedingly rare, because
                -- all scales should have at least one pitch
                |> Maybe.withDefault initPitch

        beforeSelection : List Pitch.Pitch
        beforeSelection =
            List.filter
                (Pitch.isLessThan selection)
                pitchList

        afterSelection : List Pitch.Pitch
        afterSelection =
            List.filter
                (Pitch.isGreaterThan selection)
                pitchList
    in
    { selection = selection
    , beforeSelection = beforeSelection
    , afterSelection = afterSelection
    }


fromSplit :
    Scale.Scale
    ->
        { selection : Pitch.Pitch
        , beforeSelection : List Pitch.Pitch
        , afterSelection : List Pitch.Pitch
        }
    -> ScaleStepper
fromSplit scale { selection, beforeSelection, afterSelection } =
    ScaleStepper
        { selection = selection
        , beforeSelection = beforeSelection
        , afterSelection = afterSelection
        , scale = scale
        }


current : ScaleStepper -> Pitch.Pitch
current (ScaleStepper details) =
    details.selection


step : Int -> ScaleStepper -> ScaleStepper
step numSteps (ScaleStepper details) =
    Debug.todo "implement"


switchScale : Scale.Scale -> ScaleStepper -> ScaleStepper
switchScale newScale (ScaleStepper details) =
    Debug.todo "implement"
