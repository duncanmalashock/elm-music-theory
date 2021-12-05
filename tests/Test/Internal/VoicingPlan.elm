module Test.Internal.VoicingPlan exposing (..)

import Expect
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
                        expected =
                            "[opt=1/dbl=N/plc=any,opt=3,7/dbl=N/plc=above]"

                        result =
                            VoicingPlan.forScaleTypes
                                [ { scaleType = ScaleType.ionian
                                  , voicingPlan =
                                        VoicingPlan.firstVoice
                                            { options = [ 1 ]
                                            , canBeDoubled = False
                                            }
                                            |> VoicingPlan.nextVoice
                                                { options = [ 3, 7 ]
                                                , canBeDoubled = False
                                                , placement = VoicingPlan.placeAbove
                                                }
                                  }
                                ]
                                |> VoicingPlan.toString
                    in
                    Expect.equal expected result
            ]
        ]
