module Music.Internal.VoicingPlan exposing
    ( ForScaleTypes
    , Placement
    , Voice
    , VoicingPlan
    , firstVoice
    , forScaleTypes
    , nextVoice
    , placeAbove
    , placeAboveByAtLeast
    , placeAnywhere
    , placeBelow
    , placeBelowByAtLeast
    , placeWithin
    , toString
    )

import AssocList as Dict
import Music.Internal.Interval as Interval
import Music.Internal.ScaleType as ScaleType


toString : ForScaleTypes -> String
toString (ForScaleTypes dict) =
    Dict.toList dict
        |> List.map voicingToString
        |> String.join ","
        |> (\str -> "[" ++ str ++ "]")


voicingToString : ( ScaleType.ScaleType, VoicingPlan ) -> String
voicingToString ( _, VoicingPlan voices ) =
    voices
        |> List.map voiceToString
        |> String.join ","


forScaleTypes :
    List
        { scaleType : ScaleType.ScaleType
        , voicingPlan : VoicingPlan
        }
    -> ForScaleTypes
forScaleTypes list =
    list
        |> List.map
            (\{ scaleType, voicingPlan } ->
                ( scaleType, voicingPlan )
            )
        |> Dict.fromList
        |> ForScaleTypes


type ForScaleTypes
    = ForScaleTypes (Dict.Dict ScaleType.ScaleType VoicingPlan)


type VoicingPlan
    = VoicingPlan (List Voice)


type Voice
    = Voice
        { options : List Int
        , canBeDoubled : Bool
        , placement : Placement
        }


voiceToString : Voice -> String
voiceToString (Voice { options, canBeDoubled, placement }) =
    let
        optionsAsString =
            options
                |> List.map String.fromInt
                |> String.join ","

        canBeDoubledAsString =
            if canBeDoubled then
                "Y"

            else
                "N"

        placementAsString =
            case placement of
                PlacementAnywhere ->
                    "any"

                PlacementAbove ->
                    "above"

                PlacementAboveByAtLeast interval ->
                    "aboveMin" ++ Interval.shortName interval

                PlacementBelow ->
                    "below"

                PlacementBelowByAtLeast interval ->
                    "belowMin" ++ Interval.shortName interval

                PlacementWithin interval ->
                    "within" ++ Interval.shortName interval
    in
    "opt="
        ++ optionsAsString
        ++ "/dbl="
        ++ canBeDoubledAsString
        ++ "/plc="
        ++ placementAsString


type Placement
    = PlacementAnywhere
    | PlacementAbove
    | PlacementAboveByAtLeast Interval.Interval
    | PlacementBelow
    | PlacementBelowByAtLeast Interval.Interval
    | PlacementWithin Interval.Interval


placeAnywhere : Placement
placeAnywhere =
    PlacementAbove


placeAbove : Placement
placeAbove =
    PlacementAbove


placeAboveByAtLeast : Interval.Interval -> Placement
placeAboveByAtLeast interval =
    PlacementAboveByAtLeast interval


placeBelow : Placement
placeBelow =
    PlacementBelow


placeBelowByAtLeast : Interval.Interval -> Placement
placeBelowByAtLeast interval =
    PlacementBelowByAtLeast interval


placeWithin : Interval.Interval -> Placement
placeWithin range =
    PlacementWithin range


firstVoice :
    { options : List Int
    , canBeDoubled : Bool
    }
    -> VoicingPlan
firstVoice { options, canBeDoubled } =
    [ { options = options
      , canBeDoubled = canBeDoubled
      , placement = PlacementAnywhere
      }
        |> Voice
    ]
        |> VoicingPlan


nextVoice :
    { options : List Int
    , canBeDoubled : Bool
    , placement : Placement
    }
    -> VoicingPlan
    -> VoicingPlan
nextVoice { options, canBeDoubled, placement } (VoicingPlan list) =
    list
        ++ [ { options = options
             , canBeDoubled = canBeDoubled
             , placement = placement
             }
                |> Voice
           ]
        |> VoicingPlan
