module Pitch exposing (all)

import Expect
import MusicTheory.Interval as Interval
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.Octave as Octave exposing (OctaveError(..))
import MusicTheory.Pitch as Pitch exposing (PitchError(..))
import MusicTheory.PitchClass as PitchClass
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Pitch Tests"
        [ describe "semitones"
            [ test "semitones of B##4 should be 61 (4*12 (octave) + 11 (letter B) + 2 (double sharp))" <|
                \_ ->
                    Pitch.fromPitchClass
                        Octave.four
                        (PitchClass.pitchClass B PitchClass.doubleSharp)
                        |> Result.map Pitch.semitones
                        |> Expect.equal (Ok 61)
            , test "semitones of C#5 should be 61 (5*12 (octave) + 0 (letter C) + 1 (sharp))" <|
                \_ ->
                    Pitch.fromPitchClass
                        Octave.five
                        (PitchClass.pitchClass C PitchClass.sharp)
                        |> Result.map Pitch.semitones
                        |> Expect.equal (Ok 61)
            ]
        , describe "transposeUp"
            [ test "transpose up perfect 5 from G4 should be D5" <|
                \_ ->
                    Pitch.pitch G Pitch.natural Octave.four
                        |> Result.andThen (Pitch.transposeUp Interval.perfectFifth)
                        |> Expect.equal
                            (Pitch.pitch D Pitch.natural Octave.five)
            , test "transpose up major 3 from A8 should fail" <|
                \_ ->
                    Pitch.pitch A Pitch.natural Octave.eight
                        |> Result.andThen (Pitch.transposeUp Interval.majorThird)
                        |> Expect.equal
                            (Err <| InvalidOctave <| AboveValidRange 9)
            ]
        , describe "transposeDown"
            [ test "transpose down perfect 5 from D5 should be G4" <|
                \_ ->
                    Pitch.pitch D Pitch.natural Octave.five
                        |> Result.andThen (Pitch.transposeDown Interval.perfectFifth)
                        |> Expect.equal
                            (Pitch.pitch G Pitch.natural Octave.four)
            , test "transpose down major 3 from D0 should fail" <|
                \_ ->
                    Pitch.pitch D Pitch.natural Octave.zero
                        |> Result.andThen (Pitch.transposeDown Interval.majorThird)
                        |> Expect.equal
                            (Err <| InvalidOctave <| BelowValidRange -1)
            ]
        , describe "allForPitchClass"
            [ test "should return Pitches in all Octaves" <|
                \_ ->
                    Pitch.allForPitchClass (PitchClass.pitchClass F Pitch.natural)
                        |> List.length
                        |> Expect.equal 9
            ]
        , describe "firstBelow"
            [ test "should return D4 (first occurence of D below F4)" <|
                \_ ->
                    Pitch.pitch F Pitch.natural Octave.four
                        |> Result.andThen
                            (Pitch.firstBelow (PitchClass.pitchClass D Pitch.natural))
                        |> Expect.equal
                            (Pitch.pitch D Pitch.natural Octave.four)
            , test "should return nothing if no valid pitch exists below" <|
                \_ ->
                    Pitch.pitch C Pitch.natural Octave.zero
                        |> Result.andThen
                            (Pitch.firstBelow (PitchClass.pitchClass A Pitch.natural))
                        |> Expect.equal
                            (Err OutOfRange)
            ]
        , describe "firstAbove"
            [ test "should return D5 (first occurence of D below F4)" <|
                \_ ->
                    Pitch.pitch F Pitch.natural Octave.four
                        |> Result.andThen
                            (Pitch.firstAbove (PitchClass.pitchClass D Pitch.natural))
                        |> Expect.equal
                            (Pitch.pitch D Pitch.natural Octave.five)
            , test "should return nothing if no valid pitch exists above" <|
                \_ ->
                    Pitch.pitch B Pitch.natural Octave.eight
                        |> Result.andThen
                            (Pitch.firstAbove (PitchClass.pitchClass A Pitch.natural))
                        |> Expect.equal
                            (Err OutOfRange)
            ]
        ]
