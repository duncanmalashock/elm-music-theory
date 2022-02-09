module Music.Internal.ScaleStepper exposing
    ( ScaleStepper
    , current
    , init
    , stepByAmount
    , stepDown
    , stepUp
    , switchScale
    )

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
                -- this is effectively impossible, because
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


type Direction
    = DirectionUp
    | DirectionDown


stepUp : ScaleStepper -> ScaleStepper
stepUp stepper =
    step DirectionUp stepper


stepDown : ScaleStepper -> ScaleStepper
stepDown stepper =
    step DirectionDown stepper


stepByAmount : Int -> ScaleStepper -> ScaleStepper
stepByAmount numSteps stepper =
    if numSteps == 0 then
        stepper

    else if numSteps > 0 then
        stepByAmount (numSteps - 1) (stepUp stepper)

    else
        stepByAmount (numSteps + 1) (stepDown stepper)


step : Direction -> ScaleStepper -> ScaleStepper
step direction (ScaleStepper details) =
    case direction of
        DirectionUp ->
            case details.afterSelection of
                [] ->
                    ScaleStepper details

                head :: tail ->
                    ScaleStepper
                        { selection = head
                        , beforeSelection =
                            details.beforeSelection ++ [ details.selection ]
                        , afterSelection = tail
                        , scale = details.scale
                        }

        DirectionDown ->
            case List.reverse details.beforeSelection of
                [] ->
                    ScaleStepper details

                head :: tail ->
                    ScaleStepper
                        { selection = head
                        , beforeSelection = List.reverse tail
                        , afterSelection =
                            details.selection :: details.afterSelection
                        , scale = details.scale
                        }


switchScale : Scale.Scale -> ScaleStepper -> ScaleStepper
switchScale newScale (ScaleStepper details) =
    init details.selection newScale
