module Test.Internal.VoicingPlan exposing (all)

import Expect
import Internal.Interval as Interval
import Internal.PitchClass as PitchClass
import Internal.Placement as Placement
import Internal.Voicing as Voicing
import Internal.VoicingPlan as VoicingPlan
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
                                        , placement =
                                            Placement.placeAboveByAtMost
                                                Interval.perfectOctave
                                        }
                                    , VoicingPlan.select
                                        { options = [ 3 ]
                                        , canBeDoubled = False
                                        , placement =
                                            Placement.placeAboveByAtMost
                                                Interval.perfectOctave
                                        }
                                    ]
                                }

                        expected : List String
                        expected =
                            [ "C0,G0,E♭1"
                            , "C1,G1,E♭2"
                            , "C2,G2,E♭3"
                            , "C3,G3,E♭4"
                            , "C4,G4,E♭5"
                            , "C5,G5,E♭6"
                            , "C6,G6,E♭7"
                            , "C7,G7,E♭8"
                            ]

                        result : List String
                        result =
                            plan
                                |> VoicingPlan.toVoicings PitchClass.c
                                |> List.map Voicing.toString
                    in
                    Expect.equal expected result
            ]
        ]
