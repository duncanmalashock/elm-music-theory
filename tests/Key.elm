module Key exposing (all)

import Expect
import MusicTheory.Key as Key
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.PitchClass exposing (natural, pitchClass)
import MusicTheory.Scale as Scale
import MusicTheory.ScaleClass as ScaleClass
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
