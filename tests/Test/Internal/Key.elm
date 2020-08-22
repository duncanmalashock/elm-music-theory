module Test.Internal.Key exposing (all)

import Expect
import Internal.Key as Key
import Internal.Letter exposing (Letter(..))
import Internal.PitchClass exposing (natural, pitchClass)
import Internal.Scale as Scale
import Internal.ScaleClass as ScaleClass
import Test exposing (..)


all : Test
all =
    describe "Key Tests"
        [ test "C major key should have correct scale" <|
            \_ ->
                let
                    cMajorScale =
                        Scale.scale (pitchClass C natural) ScaleClass.ionian
                in
                Key.major (pitchClass C natural)
                    |> Key.scale
                    |> Expect.equal cMajorScale
        ]
