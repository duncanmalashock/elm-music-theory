module Test.Internal.Pitch exposing (all)

import Expect
import Internal.Interval as Interval
import Internal.Letter exposing (Letter(..))
import Internal.Octave as Octave
import Internal.Pitch as Pitch
import Internal.PitchClass as PitchClass
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
                        |> Expect.equal 10
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
        , describe "simplify"
            [ test "B##4 should become C#5" <|
                \_ ->
                    let
                        expected =
                            Pitch.cSharp5

                        result =
                            Pitch.simplify (Pitch.transposeUp Interval.augmentedUnison Pitch.bSharp4)
                    in
                    Expect.equal expected result
            , test "Fb3 should become E3" <|
                \_ ->
                    let
                        expected =
                            Pitch.e3

                        result =
                            Pitch.simplify Pitch.fFlat3
                    in
                    Expect.equal expected result
            , test "Cb3 should become B2" <|
                \_ ->
                    let
                        expected =
                            Pitch.b2

                        result =
                            Pitch.simplify Pitch.cFlat3
                    in
                    Expect.equal expected result
            ]
        , describe "normalize"
            [ test "B###4 should become D5" <|
                \_ ->
                    let
                        expected =
                            Pitch.d5

                        result =
                            Pitch.normalize 2
                                (Pitch.bSharp4
                                    |> Pitch.transposeUp Interval.augmentedUnison
                                    |> Pitch.transposeUp Interval.augmentedUnison
                                )
                    in
                    Expect.equal expected result
            , test "Fb3 should stay Fb3" <|
                \_ ->
                    let
                        expected =
                            Pitch.fFlat3

                        result =
                            Pitch.normalize 2 Pitch.fFlat3
                    in
                    Expect.equal expected result
            , test "Cb3 should stay Cb3" <|
                \_ ->
                    let
                        expected =
                            Pitch.cFlat3

                        result =
                            Pitch.normalize 2 Pitch.cFlat3
                    in
                    Expect.equal expected result
            ]
        ]
