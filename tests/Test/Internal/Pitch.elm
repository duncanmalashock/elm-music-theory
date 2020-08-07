module Test.Internal.Pitch exposing (all)

import Expect
import MusicTheory.Internal.Interval as Interval
import MusicTheory.Internal.Letter exposing (Letter(..))
import MusicTheory.Internal.Octave as Octave
import MusicTheory.Internal.Pitch as Pitch exposing (PitchError(..))
import MusicTheory.Internal.PitchClass as PitchClass
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
                    Pitch.g4
                        |> Pitch.transposeUp Interval.perfectFifth
                        |> Expect.equal
                            Pitch.d5
            ]
        , describe
            "transposeDown"
            [ test "transpose down perfect 5 from D5 should be G4" <|
                \_ ->
                    Pitch.d5
                        |> Pitch.transposeDown
                            Interval.perfectFifth
                        |> Expect.equal
                            Pitch.g4
            , test "transpose down augmented 11th from D5 should be Ab3" <|
                \_ ->
                    Pitch.d5
                        |> Pitch.transposeDown
                            Interval.augmentedEleventh
                        |> Expect.equal
                            Pitch.aFlat3
            ]
        , describe "allForPitchClass"
            [ test "should return Pitches in all Octaves" <|
                \_ ->
                    Pitch.allForPitchClass (PitchClass.pitchClass F Pitch.natural)
                        |> List.length
                        |> Expect.equal 9
            ]
        , describe "firstBelow"
            [ test "should return D4 (first occurrence of D below F4)" <|
                \_ ->
                    Pitch.f4
                        |> Pitch.firstBelow (PitchClass.pitchClass D Pitch.natural)
                        |> Expect.equal
                            (Ok Pitch.d4)
            , test "should return nothing if no valid pitch exists below" <|
                \_ ->
                    Pitch.c0
                        |> Pitch.firstBelow (PitchClass.pitchClass A Pitch.natural)
                        |> Expect.equal
                            (Err ValidPitchNotFound)
            ]
        , describe "firstAbove"
            [ test "should return D5 (first occurrence of D below F4)" <|
                \_ ->
                    Pitch.f4
                        |> Pitch.firstAbove (PitchClass.pitchClass D Pitch.natural)
                        |> Expect.equal
                            (Ok Pitch.d5)
            , test "should return nothing if no valid pitch exists above" <|
                \_ ->
                    Pitch.b8
                        |> Pitch.firstAbove (PitchClass.pitchClass A Pitch.natural)
                        |> Expect.equal
                            (Err ValidPitchNotFound)
            ]
        , describe "intervalBetween"
            [ test "interval between C4 and E4 should be a major third" <|
                \_ ->
                    let
                        expected =
                            Interval.majorThird

                        result =
                            Pitch.intervalBetween Pitch.c4 Pitch.e4
                    in
                    Expect.equal expected result
            , test "interval between C4 and F4 should be a perfect fourth" <|
                \_ ->
                    let
                        expected =
                            Interval.perfectFourth

                        result =
                            Pitch.intervalBetween Pitch.c4 Pitch.f4
                    in
                    Expect.equal expected result
            , test "interval between G4 and C5 should be a perfect fourth" <|
                \_ ->
                    let
                        expected =
                            Interval.perfectFourth

                        result =
                            Pitch.intervalBetween Pitch.g4 Pitch.c5
                    in
                    Expect.equal expected result
            , test "interval between C4 and Db4 should be a minor second" <|
                \_ ->
                    let
                        expected =
                            Interval.minorSecond

                        result =
                            Pitch.intervalBetween Pitch.c4 Pitch.dFlat4
                    in
                    Expect.equal expected result
            , test "interval between G4 and G5 should be a perfect octave" <|
                \_ ->
                    let
                        expected =
                            Interval.perfectOctave

                        result =
                            Pitch.intervalBetween Pitch.g4 Pitch.g5
                    in
                    Expect.equal expected result
            , test "interval between G4 and B5 should be a major tenth" <|
                \_ ->
                    let
                        expected =
                            Interval.majorTenth

                        result =
                            Pitch.intervalBetween Pitch.g4 Pitch.b5
                    in
                    Expect.equal expected result
            , test "interval between G4 and G#5 should be an augmented octave" <|
                \_ ->
                    let
                        expected =
                            Interval.augmentedOctave

                        result =
                            Pitch.intervalBetween Pitch.g4 Pitch.gSharp5
                    in
                    Expect.equal expected result
            , test "interval between C4 and Bb3 should be a major second (down)" <|
                \_ ->
                    let
                        expected =
                            Interval.majorSecond
                                |> Interval.reverse

                        result =
                            Pitch.intervalBetween Pitch.c4 Pitch.bFlat3
                    in
                    Expect.equal expected result
            , test "interval between G4 and E3 should be a minor tenth (down)" <|
                \_ ->
                    let
                        expected =
                            Interval.minorTenth
                                |> Interval.reverse

                        result =
                            Pitch.intervalBetween Pitch.g4 Pitch.e3
                    in
                    Expect.equal expected result
            ]
        , describe "isWithin"
            [ test "C2 is within C1–C5" <|
                \_ ->
                    Pitch.c2
                        |> Pitch.isWithin (Pitch.range Pitch.c1 Pitch.c5)
                        |> Expect.equal True
            , test "C2 is not within C3–C5" <|
                \_ ->
                    Pitch.c2
                        |> Pitch.isWithin (Pitch.range Pitch.c3 Pitch.c5)
                        |> Expect.equal False
            ]
        ]
