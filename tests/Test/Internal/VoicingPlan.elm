module Test.Internal.VoicingPlan exposing (all)

import Expect
import Music.Internal.Placement as Placement
import Music.Internal.VoicingPlan as VoicingPlan
import Music.ScaleType as ScaleType
import Test exposing (Test, describe, test)


all : Test
all =
    describe "VoicingPlan Tests"
        [ describe "init"
            [ test "builds a single voicing plan for a single scale type" <|
                \_ ->
                    let
                        expected : String
                        expected =
                            "1-3,7"

                        result : String
                        result =
                            VoicingPlan.init
                                { scaleType = ScaleType.ionian
                                , selections =
                                    [ VoicingPlan.select
                                        { options = [ 1 ]
                                        , canBeDoubled = False
                                        , placement = Placement.placeAbove
                                        }
                                    , VoicingPlan.select
                                        { options = [ 3, 7 ]
                                        , canBeDoubled = False
                                        , placement = Placement.placeAbove
                                        }
                                    ]
                                }
                                |> VoicingPlan.toString
                    in
                    Expect.equal expected result
            ]
        , describe "toVoiceList"
            [ test "creates all voice lists for a plan" <|
                \_ ->
                    let
                        plan : VoicingPlan.VoicingPlan
                        plan =
                            VoicingPlan.init
                                { scaleType = ScaleType.ionian
                                , selections =
                                    [ VoicingPlan.select
                                        { options = [ 1 ]
                                        , canBeDoubled = False
                                        , placement = Placement.placeAbove
                                        }
                                    , VoicingPlan.select
                                        { options = [ 3, 7 ]
                                        , canBeDoubled = False
                                        , placement = Placement.placeAbove
                                        }
                                    ]
                                }

                        expected : List String
                        expected =
                            []

                        result : List String
                        result =
                            plan
                                |> VoicingPlan.toVoiceList
                                |> List.map VoicingPlan.voicingClassToString
                    in
                    Expect.equal expected result
            ]
        ]
