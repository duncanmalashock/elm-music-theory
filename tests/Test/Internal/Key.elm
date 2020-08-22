module Test.Internal.Key exposing (all)

import Expect
import MusicTheory.Internal.Key as Key
import MusicTheory.Internal.Letter exposing (Letter(..))
import MusicTheory.Internal.PitchClass exposing (natural, pitchClass)
import MusicTheory.Internal.Scale as Scale
import MusicTheory.Internal.ScaleClass as ScaleClass
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
