module Music.Internal.ScaleStepper exposing
    ( ScaleStepper
    , init, initChromatic
    , step
    , setScale, setChromatic
    , currentPitch
    )

{-|

@docs ScaleStepper
@docs init, initChromatic

@docs step
@docs setScale, setChromatic

@docs currentPitch

-}

import Music.Internal.Chromatic as Chromatic
import Music.Internal.Pitch as Pitch
import Music.Internal.Scale as Scale


type ScaleStepper
    = ScaleStepper Details


type alias Details =
    { selection : Pitch.Pitch
    , aboveSelection : List Pitch.Pitch
    , belowSelection : List Pitch.Pitch
    , pitchSource : PitchSource
    }


type PitchSource
    = PitchSourceChromatic
    | PitchSourceScale Scale.Scale


init : Pitch.Pitch -> Scale.Scale -> ScaleStepper
init initPitch scale =
    Scale.toPitches scale
        |> splitOnInitPitch initPitch
        |> fromSplit (PitchSourceScale scale)


initChromatic : Pitch.Pitch -> ScaleStepper
initChromatic initPitch =
    initChromaticAscending initPitch


initChromaticAscending : Pitch.Pitch -> ScaleStepper
initChromaticAscending initPitch =
    Chromatic.ascending
        |> splitOnInitPitch initPitch
        |> fromSplit PitchSourceChromatic


initChromaticDescending : Pitch.Pitch -> ScaleStepper
initChromaticDescending initPitch =
    Chromatic.descending
        |> splitOnInitPitch initPitch
        |> fromSplit PitchSourceChromatic


splitOnInitPitch :
    Pitch.Pitch
    -> List Pitch.Pitch
    ->
        { selection : Pitch.Pitch
        , aboveSelection : List Pitch.Pitch
        , belowSelection : List Pitch.Pitch
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

        belowSelection : List Pitch.Pitch
        belowSelection =
            List.filter
                (Pitch.isLessThan selection)
                pitchList
                |> Pitch.sort
                |> List.reverse

        aboveSelection : List Pitch.Pitch
        aboveSelection =
            List.filter
                (Pitch.isGreaterThan selection)
                pitchList
                |> Pitch.sort
                |> List.reverse
    in
    { selection = selection
    , aboveSelection = aboveSelection
    , belowSelection = belowSelection
    }


fromSplit :
    PitchSource
    ->
        { selection : Pitch.Pitch
        , aboveSelection : List Pitch.Pitch
        , belowSelection : List Pitch.Pitch
        }
    -> ScaleStepper
fromSplit pitchSource { selection, aboveSelection, belowSelection } =
    ScaleStepper
        { selection = selection
        , aboveSelection = aboveSelection
        , belowSelection = belowSelection
        , pitchSource = pitchSource
        }


currentPitch : ScaleStepper -> Pitch.Pitch
currentPitch (ScaleStepper details) =
    details.selection


type Direction
    = DirectionUp
    | DirectionDown


stepUp : ScaleStepper -> ScaleStepper
stepUp stepper =
    doStep DirectionUp stepper


stepDown : ScaleStepper -> ScaleStepper
stepDown stepper =
    doStep DirectionDown stepper


step : Int -> ScaleStepper -> ScaleStepper
step numSteps stepper =
    if numSteps == 0 then
        stepper

    else if numSteps > 0 then
        step (numSteps - 1) (stepUp stepper)

    else
        step (numSteps + 1) (stepDown stepper)


doStep : Direction -> ScaleStepper -> ScaleStepper
doStep direction (ScaleStepper old) =
    let
        (ScaleStepper details) =
            case old.pitchSource of
                PitchSourceChromatic ->
                    case direction of
                        DirectionUp ->
                            initChromaticAscending old.selection

                        DirectionDown ->
                            initChromaticDescending old.selection

                PitchSourceScale _ ->
                    ScaleStepper old
    in
    case direction of
        DirectionDown ->
            case Pitch.sort details.belowSelection |> List.reverse of
                [] ->
                    ScaleStepper details

                head :: tail ->
                    ScaleStepper
                        { selection = head
                        , aboveSelection =
                            details.aboveSelection ++ [ details.selection ]
                        , belowSelection = tail
                        , pitchSource = details.pitchSource
                        }

        DirectionUp ->
            case Pitch.sort details.aboveSelection of
                [] ->
                    ScaleStepper details

                head :: tail ->
                    ScaleStepper
                        { selection = head
                        , aboveSelection = List.reverse tail
                        , belowSelection =
                            details.selection :: details.belowSelection
                        , pitchSource = details.pitchSource
                        }


setScale : Scale.Scale -> ScaleStepper -> ScaleStepper
setScale newScale (ScaleStepper details) =
    init details.selection newScale


setChromatic : ScaleStepper -> ScaleStepper
setChromatic (ScaleStepper details) =
    initChromatic details.selection
