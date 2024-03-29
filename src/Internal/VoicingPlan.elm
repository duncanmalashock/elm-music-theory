module Internal.VoicingPlan exposing
    ( VoicingPlan, init
    , toVoicings
    , toString
    , Selection, select
    )

{-|

@docs VoicingPlan, init
@docs Selection select
@docs toVoicings
@docs toString

-}

import Internal.Interval as Interval
import Internal.Octave as Octave
import Internal.Pitch as Pitch
import Internal.PitchClass as PitchClass
import Internal.Placement as Placement
import Internal.ScaleType as ScaleType
import Internal.Voicing as Voicing
import List.Extra
import Util.ConstraintSolver


toVoicings : PitchClass.PitchClass -> VoicingPlan -> List Voicing.Voicing
toVoicings pitchClass voicingPlan =
    let
        pitchReference : Pitch.Pitch
        pitchReference =
            Pitch.fromPitchClass Octave.zero pitchClass
    in
    toVoiceList voicingPlan
        |> List.map
            (\placedSelections ->
                placedSelections
                    |> List.map
                        (\{ interval, sourceInterval } ->
                            { pitch = Pitch.transposeUp interval pitchReference
                            , interval = interval
                            , sourceInterval = sourceInterval
                            }
                        )
                    |> Voicing.init
            )


toVoiceList : VoicingPlan -> List (List PlacedSelection)
toVoiceList voicingPlan =
    Util.ConstraintSolver.solve
        { setup = initSetup voicingPlan
        , nextSetups = nextSetups
        , constraints = constraints
        , toSolution = toSolution
        }
        |> .solved


type alias Setup =
    { scaleType : ScaleType.ScaleType
    , toProcess : List Selection
    , placedSelections : List PlacedSelection
    }


initSetup : VoicingPlan -> Setup
initSetup (VoicingPlan scaleType selectionList) =
    { scaleType = scaleType
    , toProcess = selectionList
    , placedSelections = []
    }


nextSetups : Setup -> List Setup
nextSetups setup =
    case setup.toProcess of
        [] ->
            []

        (Selection toProcessHead) :: toProcessTail ->
            let
                scaleAsList : List Interval.Interval
                scaleAsList =
                    ScaleType.toList setup.scaleType

                sourceDegrees : List Interval.Interval
                sourceDegrees =
                    toProcessHead.options
                        |> List.map
                            (\index ->
                                List.Extra.getAt (index - 1) scaleAsList
                            )
                        |> List.filterMap identity

                atAllOctaves : Interval.Interval -> List Interval.Interval
                atAllOctaves sourceInterval =
                    Octave.allValid
                        |> List.map Octave.toInterval
                        |> List.map (Interval.add sourceInterval)

                placedSelectionOptions : List PlacedSelection
                placedSelectionOptions =
                    sourceDegrees
                        |> List.concatMap
                            (\sourceInterval ->
                                sourceInterval
                                    |> atAllOctaves
                                    |> List.map
                                        (\transposed ->
                                            { interval = transposed
                                            , sourceInterval = sourceInterval
                                            }
                                        )
                            )

                placedToSetup : PlacedSelection -> Setup
                placedToSetup placed =
                    { scaleType = setup.scaleType
                    , toProcess = toProcessTail
                    , placedSelections = placed :: setup.placedSelections
                    }

                maybePreviousPlaced : Maybe PlacedSelection
                maybePreviousPlaced =
                    List.head setup.placedSelections

                satisfiesPlacementConstraint : PlacedSelection -> Bool
                satisfiesPlacementConstraint placed =
                    case maybePreviousPlaced of
                        Just previousPlaced ->
                            Placement.checkIntervals
                                toProcessHead.placement
                                { from = previousPlaced.interval
                                , to = placed.interval
                                }

                        Nothing ->
                            True

                satisfiesDoublingConstraint : PlacedSelection -> Bool
                satisfiesDoublingConstraint placed =
                    if toProcessHead.canBeDoubled then
                        True

                    else
                        case setup.placedSelections of
                            [] ->
                                True

                            nonEmpty ->
                                List.map .sourceInterval nonEmpty
                                    |> List.member placed.sourceInterval
                                    |> not
            in
            placedSelectionOptions
                |> List.filter satisfiesPlacementConstraint
                |> List.filter satisfiesDoublingConstraint
                |> List.map placedToSetup


constraints : List (Setup -> Result String Setup)
constraints =
    []


toSolution : Setup -> Result String (List PlacedSelection)
toSolution setup =
    case setup.toProcess of
        [] ->
            Ok (List.reverse setup.placedSelections)

        _ ->
            Err "not all selections processed"


type alias PlacedSelection =
    { interval : Interval.Interval
    , sourceInterval : Interval.Interval
    }



-- FROM SCALE


init :
    { scaleType : ScaleType.ScaleType
    , selections : List Selection
    }
    -> VoicingPlan
init { scaleType, selections } =
    VoicingPlan scaleType selections


type VoicingPlan
    = VoicingPlan ScaleType.ScaleType (List Selection)


type Selection
    = Selection
        { options : List Int
        , canBeDoubled : Bool
        , placement : Placement.Placement
        }


select :
    { options : List Int
    , canBeDoubled : Bool
    , placement : Placement.Placement
    }
    -> Selection
select { options, canBeDoubled, placement } =
    { options = options
    , canBeDoubled = canBeDoubled
    , placement = placement
    }
        |> Selection


toString : VoicingPlan -> String
toString (VoicingPlan scaleType selections) =
    selections
        |> List.map selectionToString
        |> String.join "-"


selectionToString : Selection -> String
selectionToString (Selection details) =
    let
        viewOptions =
            optionsToString details.options

        viewDoubled =
            if details.canBeDoubled then
                ""

            else
                "U"

        viewPlacement =
            Placement.toString details.placement
    in
    "(" ++ viewOptions ++ viewPlacement ++ viewDoubled ++ ")"


optionsToString : List Int -> String
optionsToString options =
    options
        |> List.map String.fromInt
        |> String.join ","
