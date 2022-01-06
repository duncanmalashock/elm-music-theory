module Test.Internal.VoicingPlan exposing (all)

import Expect
import Music.Internal.Interval as Interval
import Music.Internal.PitchClass as PitchClass
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
                            "(1>)-(3,7>U)"

                        result : String
                        result =
                            VoicingPlan.init
                                { scaleType = ScaleType.ionian
                                , selections =
                                    [ VoicingPlan.select
                                        { options = [ 1 ]
                                        , canBeDoubled = True
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
        , describe "toVoicings"
            [ test "creates all voicings for a plan" <|
                \_ ->
                    let
                        plan : VoicingPlan.VoicingPlan
                        plan =
                            VoicingPlan.init
                                { scaleType = ScaleType.aeolian
                                , selections =
                                    [ VoicingPlan.select
                                        { options = [ 1 ]
                                        , canBeDoubled = False
                                        , placement = Placement.placeAnywhere
                                        }
                                    , VoicingPlan.select
                                        { options = [ 5 ]
                                        , canBeDoubled = False
                                        , placement = Placement.placeBelowByAtMost Interval.perfectOctave
                                        }
                                    , VoicingPlan.select
                                        { options = [ 3 ]
                                        , canBeDoubled = False
                                        , placement = Placement.placeBelowByAtMost Interval.perfectOctave
                                        }
                                    , VoicingPlan.select
                                        { options = [ 6 ]
                                        , canBeDoubled = False
                                        , placement = Placement.placeBelowByAtMost Interval.perfectOctave
                                        }
                                    ]
                                }

                        expected : List String
                        expected =
                            []

                        result : List String
                        result =
                            plan
                                |> VoicingPlan.toVoicings PitchClass.c
                                |> List.map VoicingPlan.voicingToString
                    in
                    Expect.equal expected result
            ]
        ]
