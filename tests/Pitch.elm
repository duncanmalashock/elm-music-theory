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
                        |> Pitch.semitones
                        |> Expect.equal 61
            , test "semitones of C#5 should be 61 (5*12 (octave) + 0 (letter C) + 1 (sharp))" <|
                \_ ->
                    Pitch.fromPitchClass
                        Octave.five
                        (PitchClass.pitchClass C PitchClass.sharp)
                        |> Pitch.semitones
                        |> Expect.equal 61
            ]
        , describe "transposeUp"
            [ test "transpose up perfect 5 from G4 should be D5" <|
                \_ ->
                    Pitch.pitch G Pitch.natural Octave.four
                        |> Pitch.transposeUp Interval.perfectFifth
                        |> Expect.equal
                            (Ok <| Pitch.pitch D Pitch.natural Octave.five)
            , test "transpose up major 3 from A8 should fail" <|
                \_ ->
                    Pitch.pitch A Pitch.natural Octave.eight
                        |> Pitch.transposeUp Interval.majorThird
                        |> Expect.equal
                            (Err <| InvalidOctave <| AboveValidRange 9)
            ]
        , describe "transposeDown"
            [ test "transpose down perfect 5 from D5 should be G4" <|
                \_ ->
                    Pitch.pitch D Pitch.natural Octave.five
                        |> Pitch.transposeDown Interval.perfectFifth
                        |> Expect.equal
                            (Ok <| Pitch.pitch G Pitch.natural Octave.four)
            , test "transpose down major 3 from D0 should fail" <|
                \_ ->
                    Pitch.pitch D Pitch.natural Octave.zero
                        |> Pitch.transposeDown Interval.majorThird
                        |> Expect.equal
                            (Err <| InvalidOctave <| BelowValidRange -1)
            ]
        , describe "allForPitchClass"
            [ test "should return Pitches in all Octaves" <|
                \_ ->
                    Pitch.allForPitchClass (PitchClass.pitchClass F Pitch.natural)
                        |> Expect.equal
                            [ Pitch.pitch F Pitch.natural Octave.zero
                            , Pitch.pitch F Pitch.natural Octave.one
                            , Pitch.pitch F Pitch.natural Octave.two
                            , Pitch.pitch F Pitch.natural Octave.three
                            , Pitch.pitch F Pitch.natural Octave.four
                            , Pitch.pitch F Pitch.natural Octave.five
                            , Pitch.pitch F Pitch.natural Octave.six
                            , Pitch.pitch F Pitch.natural Octave.seven
                            , Pitch.pitch F Pitch.natural Octave.eight
                            ]
            ]
        , describe "firstBelow"
            [ test "should return D4 (first occurence of D below F4)" <|
                \_ ->
                    Pitch.firstBelow (Pitch.pitch F Pitch.natural Octave.four) (PitchClass.pitchClass D Pitch.natural)
                        |> Expect.equal
                            (Just (Pitch.pitch D Pitch.natural Octave.four))
            , test "should return nothing if no valid pitch exists below" <|
                \_ ->
                    Pitch.firstBelow (Pitch.pitch C Pitch.natural Octave.zero) (PitchClass.pitchClass A Pitch.natural)
                        |> Expect.equal
                            Nothing
            ]
        , describe "firstAbove"
            [ test "should return D5 (first occurence of D below F4)" <|
                \_ ->
                    Pitch.firstAbove (Pitch.pitch F Pitch.natural Octave.four) (PitchClass.pitchClass D Pitch.natural)
                        |> Expect.equal
                            (Just (Pitch.pitch D Pitch.natural Octave.five))
            , test "should return nothing if no valid pitch exists above" <|
                \_ ->
                    Pitch.firstAbove (Pitch.pitch B Pitch.natural Octave.eight) (PitchClass.pitchClass C Pitch.natural)
                        |> Expect.equal
                            Nothing
            ]
        ]
