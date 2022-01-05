module Music.Internal.VoicingPlan exposing
    ( Selection
    , VoicingPlan
    , init
    , select
    , toString
    , toVoiceList
    , voicingClassToString
    )

import List.Extra
import Music.Internal.Interval as Interval
import Music.Internal.Octave as Octave
import Music.Internal.Placement as Placement
import Music.Internal.ScaleType as ScaleType
import Util.ConstraintSolver


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
    , toProcess = List.reverse selectionList
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
                    , placedSelections = setup.placedSelections ++ [ placed ]
                    }
            in
            placedSelectionOptions
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


voicingClassToString : List PlacedSelection -> String
voicingClassToString placedSelection =
    placedSelection
        |> List.map (.interval >> Interval.shortName)
        |> String.join ","



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
    details.options
        |> List.map String.fromInt
        |> String.join ","
