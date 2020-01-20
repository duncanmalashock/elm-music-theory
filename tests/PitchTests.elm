module PitchTests exposing (all)

import Expect
import Fuzz exposing (Fuzzer)
import MusicTheory.Internal.Pitch as Internal exposing (TransposeError(..))
import MusicTheory.Internal.PitchClass as PitchClass
import MusicTheory.Interval as Interval
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.Octave as Octave exposing (OctaveError(..))
import MusicTheory.Pitch as Pitch
import Test exposing (..)
import Util.IntervalFuzzer
import Util.PitchFuzzer


all : Test
all =
    describe "Pitch Tests"
        [ test "semitones of B##4 should be 61 (4*12 (octave) + 11 (letter B) + 2 (double sharp))" <|
            \_ ->
                Internal.fromPitchClass Octave.four (PitchClass.pitchClass B PitchClass.doubleSharp)
                    |> Internal.semitones
                    |> Expect.equal 61
        , test "semitones of C#5 should be 61 (5*12 (octave) + 0 (letter C) + 1 (sharp))" <|
            \_ ->
                Internal.fromPitchClass Octave.five (PitchClass.pitchClass C PitchClass.sharp)
                    |> Internal.semitones
                    |> Expect.equal 61
        , test "toString" <|
            \_ ->
                let
                    testCases =
                        [ ( Internal.pitch C Internal.natural Octave.four, "C4" )
                        , ( Internal.pitch A Internal.flat Octave.zero, "A♭0" )
                        , ( Internal.pitch G Internal.tripleSharp Octave.six, "A♯6" )
                        , ( Internal.pitch B Internal.sharp Octave.eight, "C9" )
                        , ( Internal.pitch C Internal.flat Octave.zero, "B-1" )
                        ]

                    input =
                        testCases |> List.map (Tuple.first >> Pitch.toString)

                    expected =
                        testCases |> List.map Tuple.second
                in
                Expect.equal input expected
        , test "transpose up perfect 5 from C4 should be G4" <|
            \_ ->
                Pitch.pitch C Pitch.natural Octave.four
                    |> Pitch.transposeUp Interval.perfectFifth
                    |> Expect.equal (Ok <| Pitch.pitch G Pitch.natural Octave.four)
        , test "transpose up perfect 5 from G4 should be D5" <|
            \_ ->
                Pitch.pitch G Pitch.natural Octave.four
                    |> Pitch.transposeUp Interval.perfectFifth
                    |> Expect.equal (Ok <| Pitch.pitch D Pitch.natural Octave.five)
        , test "transpose up major 3 from A8 should fail" <|
            \_ ->
                Pitch.pitch A Pitch.natural Octave.eight
                    |> Pitch.transposeUp Interval.majorThird
                    |> Expect.equal (Err <| InvalidOctave <| AboveValidRange 9)
        , test "transpose down perfect 5 from G4 should be C4" <|
            \_ ->
                Pitch.pitch G Pitch.natural Octave.four
                    |> Pitch.transposeDown Interval.perfectFifth
                    |> Expect.equal (Ok <| Pitch.pitch C Pitch.natural Octave.four)
        , test "transpose down perfect 5 from D5 should be G4" <|
            \_ ->
                Pitch.pitch D Pitch.natural Octave.five
                    |> Pitch.transposeDown Interval.perfectFifth
                    |> Expect.equal (Ok <| Pitch.pitch G Pitch.natural Octave.four)
        , test "transpose down major 3 from D0 should fail" <|
            \_ ->
                Pitch.pitch D Pitch.natural Octave.zero
                    |> Pitch.transposeDown Interval.majorThird
                    |> Expect.equal (Err <| InvalidOctave <| BelowValidRange -1)
        , fuzz2 (Util.PitchFuzzer.pitchWithOctave Octave.four) Util.IntervalFuzzer.interval "transpose pitch by interval, result should have correct number of semitones" <|
            \pitch interval ->
                pitch
                    |> Pitch.transposeUp interval
                    |> Result.map Pitch.semitones
                    |> Expect.equal (Ok <| Interval.semitones interval + Pitch.semitones pitch)
        , fuzz2 (Util.PitchFuzzer.pitchWithOctave Octave.four) Util.IntervalFuzzer.interval "transpose a pitch up and down by the same interval should result in the original pitch" <|
            \pitch interval ->
                pitch
                    |> Pitch.transposeUp interval
                    |> Result.andThen (Pitch.transposeDown interval)
                    |> Expect.equal (Ok pitch)
        , fuzz Util.PitchFuzzer.pitch "transpose pitch up a perfect unison should result in the original pitch" <|
            \pitch ->
                pitch
                    |> Pitch.transposeUp Interval.perfectUnison
                    |> Expect.equal (Ok pitch)
        , fuzz Util.PitchFuzzer.pitch "transpose pitch down a perfect unison result in the original pitch " <|
            \pitch ->
                pitch
                    |> Pitch.transposeDown Interval.perfectUnison
                    |> Expect.equal (Ok pitch)
        , fuzz3 (Util.PitchFuzzer.pitchWithOctave Octave.four) Util.IntervalFuzzer.interval Util.IntervalFuzzer.interval "transpose up and down 2 intervals, expect original pitch" <|
            \pitch i1 i2 ->
                pitch
                    |> Pitch.transposeUp i1
                    |> Result.andThen (Pitch.transposeUp i2)
                    |> Result.andThen (Pitch.transposeDown i1)
                    |> Result.andThen (Pitch.transposeDown i2)
                    |> Expect.equal (Ok pitch)
        ]
