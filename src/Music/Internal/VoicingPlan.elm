module Music.Internal.VoicingPlan exposing (..)

import AssocList as Dict
import Music.Internal.ScaleType as ScaleType


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


type Placement
    = PlacementAnywhere


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
