module Test.Internal.Key exposing (all)

import Expect
import Music.Internal.Key as Key
import Music.Internal.Letter exposing (Letter(..))
import Music.Internal.PitchClass exposing (natural, pitchClass)
import Music.Internal.Scale as Scale
import Music.Internal.ScaleType as ScaleType
import Test exposing (..)


all : Test
all =
    describe "Key Tests"
        [ test "C major key should have correct scale" <|
            \_ ->
                let
                    cMajorScale =
                        Scale.scale (pitchClass C natural) ScaleType.ionian
                in
                Key.major (pitchClass C natural)
                    |> Key.scale
                    |> Expect.equal cMajorScale
        ]
