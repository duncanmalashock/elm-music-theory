module Test.Internal.VoicingPlan exposing (..)

import Expect
import Music.Internal.VoicingPlan as VoicingPlan
import Music.ScaleType as ScaleType
import Test exposing (Test, describe, test)


all : Test
all =
    describe "VoicingPlan Tests"
        [ describe "init"
            [--test "builds a single voicing plan for a single scale type" <|
             --   \_ ->
             --       let
             --           expected =
             --               Nothing
             --
             --           result =
             --               VoicingPlan.forScaleTypes
             --                   [ { scaleType = ScaleType.ionian
             --                     , voicingPlan =
             --                           VoicingPlan.firstVoice
             --                               { options = [ 1 ]
             --                               , canBeDoubled = False
             --                               }
             --                     }
             --                   ]
             --       in
             --       Expect.equal expected result
            ]
        ]
