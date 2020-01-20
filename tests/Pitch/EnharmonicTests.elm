module Pitch.EnharmonicTests exposing (all)

import Expect
import MusicTheory.Internal.Pitch as Internal
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.Octave as Octave exposing (OctaveError(..))
import MusicTheory.Pitch as Pitch
import MusicTheory.Pitch.Enharmonic as PitchEnharmonic exposing (EnharmonicTransformationError(..))
import MusicTheory.PitchClass.Enharmonic as PitchClassEnharmonic
import Test exposing (..)
import Util.PitchFuzzer


all : Test
all =
    describe "Enharmonic Tests"
        [ fuzz Util.PitchFuzzer.pitch "simple enharmonic equivalent should have same number of semitones" <|
            \pitch ->
                let
                    expectedSemitones =
                        pitch |> Internal.semitones

                    expectedResult =
                        if expectedSemitones < 0 then
                            Err <| Invalid (pitch |> Internal.pitchClass |> PitchClassEnharmonic.simple) (BelowValidRange -1)

                        else if expectedSemitones >= 108 then
                            Err <| Invalid (pitch |> Internal.pitchClass |> PitchClassEnharmonic.simple) (AboveValidRange 9)

                        else
                            Ok expectedSemitones
                in
                pitch
                    |> PitchEnharmonic.simple
                    |> Result.map Internal.semitones
                    |> Expect.equal expectedResult
        , test "enharmonic equivalent of Ebbb4 as natural or sharp should be C#4" <|
            \_ ->
                Pitch.pitch E Pitch.tripleFlat Octave.four
                    |> PitchEnharmonic.asNaturalOrElseSharp
                    |> Expect.equal (Ok <| Pitch.pitch C Pitch.sharp Octave.four)
        , test "enharmonic equivalent of Cbb4 as natural or sharp should be A#3" <|
            \_ ->
                Pitch.pitch C Pitch.doubleFlat Octave.four
                    |> PitchEnharmonic.asNaturalOrElseSharp
                    |> Expect.equal (Ok <| Pitch.pitch A Pitch.sharp Octave.three)
        , test "enharmonic equivalent of F###2 as natural or flat should be Ab2" <|
            \_ ->
                Pitch.pitch F Pitch.tripleSharp Octave.two
                    |> PitchEnharmonic.asNaturalOrElseFlat
                    |> Expect.equal (Ok <| Pitch.pitch A Pitch.flat Octave.two)
        , test "enharmonic equivalent of B#7 as natural or flat should be C8" <|
            \_ ->
                Pitch.pitch B Pitch.sharp Octave.seven
                    |> PitchEnharmonic.asNaturalOrElseFlat
                    |> Expect.equal (Ok <| Pitch.pitch C Pitch.natural Octave.eight)
        ]
