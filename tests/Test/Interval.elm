module Test.Interval exposing (all)

import Expect
import MusicTheory.Interval as Interval
import Test exposing (Test, describe, test)


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
                    Expect.equal result expected
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
                    Expect.equal result expected
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
                    Expect.equal result expected
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
                    Expect.equal result expected
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
                    Expect.equal result expected
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
                    Expect.equal result expected
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
                    Expect.equal result expected
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
                    Expect.equal result expected
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
                    Expect.equal result expected
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
                    Expect.equal result expected
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
                    Expect.equal result expected
            ]
        ]
