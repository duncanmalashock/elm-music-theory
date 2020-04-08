module Test.Interval exposing (all)

import Expect
import MusicTheory.Interval as Interval
import Test exposing (Test, describe, only, test)


all : Test
all =
    describe "Interval Tests"
        [ describe "semitones"
            [ test "major tenth should contain 16 semitones" <|
                \_ ->
                    Interval.majorTenth
                        |> Interval.semitones
                        |> Expect.equal 16
            , test "Major third down should contain -4 semitones" <|
                \_ ->
                    Interval.majorThird
                        |> Interval.reverse
                        |> Interval.semitones
                        |> Expect.equal -4
            , test "minor third down should contain -3 semitones" <|
                \_ ->
                    Interval.minorThird
                        |> Interval.reverse
                        |> Interval.semitones
                        |> Expect.equal -3
            ]
        , describe "firstBelow"
            [ test "Should return unchanged if it is the first below the reference" <|
                \_ ->
                    let
                        result =
                            Interval.firstBelow
                                Interval.majorThird
                                Interval.perfectFifth

                        expected =
                            Interval.majorThird
                    in
                    Expect.equal expected result
            , test "First P5 below a M3 should be P5 - PO" <|
                \_ ->
                    let
                        result =
                            Interval.firstBelow
                                Interval.perfectFifth
                                Interval.majorThird

                        expected =
                            Interval.perfectFifth
                                |> Interval.add
                                    (Interval.perfectOctave
                                        |> Interval.reverse
                                    )
                    in
                    Expect.equal expected result
            , test "First M6 below PU should be m3" <|
                \_ ->
                    let
                        result =
                            Interval.firstBelow
                                Interval.majorSixth
                                Interval.perfectUnison

                        expected =
                            Interval.minorThird
                                |> Interval.reverse
                    in
                    Expect.equal expected result
            ]
        , describe "add"
            [ test "PU plus PU should equal PU" <|
                \_ ->
                    let
                        result =
                            Interval.add
                                Interval.perfectUnison
                                Interval.perfectUnison

                        expected =
                            Interval.perfectUnison
                    in
                    Expect.equal expected result
            , test "P5 plus PU should equal P5" <|
                \_ ->
                    let
                        result =
                            Interval.add
                                Interval.perfectFifth
                                Interval.perfectUnison

                        expected =
                            Interval.perfectFifth
                    in
                    Expect.equal expected result
            , test "P5 plus M2 should equal M6" <|
                \_ ->
                    let
                        result =
                            Interval.add
                                Interval.perfectFifth
                                Interval.majorSecond

                        expected =
                            Interval.majorSixth
                    in
                    Expect.equal expected result
            , test "P4 plus M10 should equal M13" <|
                \_ ->
                    let
                        result =
                            Interval.add
                                Interval.perfectFourth
                                Interval.majorTenth

                        expected =
                            Interval.majorThirteenth
                    in
                    Expect.equal expected result
            , test "P4 plus m10 should equal m13" <|
                \_ ->
                    let
                        result =
                            Interval.add
                                Interval.perfectFourth
                                Interval.minorTenth

                        expected =
                            Interval.minorThirteenth
                    in
                    Expect.equal expected result
            , test "P4 plus A2 should equal A5" <|
                \_ ->
                    let
                        result =
                            Interval.add
                                Interval.perfectFourth
                                Interval.augmentedSecond

                        expected =
                            Interval.augmentedFifth
                    in
                    Expect.equal expected result
            , test "D2 plus A2 should equal m3" <|
                \_ ->
                    let
                        result =
                            Interval.add
                                Interval.diminishedSecond
                                Interval.augmentedSecond

                        expected =
                            Interval.minorThird
                    in
                    Expect.equal expected result
            , test "P4 plus P4 should equal m7" <|
                \_ ->
                    let
                        result =
                            Interval.add
                                Interval.perfectFourth
                                Interval.perfectFourth

                        expected =
                            Interval.minorSeventh
                    in
                    Expect.equal expected result
            , test "P5 plus P5 should equal M9" <|
                \_ ->
                    let
                        result =
                            Interval.add
                                Interval.perfectFifth
                                Interval.perfectFifth

                        expected =
                            Interval.majorNinth
                    in
                    Expect.equal expected result
            , test "P5 minus M2 should equal P4" <|
                \_ ->
                    let
                        result =
                            Interval.add
                                Interval.perfectFifth
                                (Interval.majorSecond
                                    |> Interval.reverse
                                )

                        expected =
                            Interval.perfectFourth
                    in
                    Expect.equal expected result
            , test "PU minus M2 should equal -M2" <|
                \_ ->
                    let
                        result =
                            Interval.add
                                Interval.perfectUnison
                                (Interval.perfectFifth
                                    |> Interval.reverse
                                )

                        expected =
                            Interval.perfectFifth
                                |> Interval.reverse
                    in
                    Expect.equal expected result
            , test "M6 minus PO should equal -m3" <|
                \_ ->
                    let
                        result =
                            Interval.add
                                Interval.majorSixth
                                (Interval.perfectOctave
                                    |> Interval.reverse
                                )

                        expected =
                            Interval.minorThird
                                |> Interval.reverse
                    in
                    Expect.equal expected result
            ]
        ]
