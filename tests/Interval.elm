module Interval exposing (all)

import Expect
import Fuzzers.IntervalFuzzer as Fuzzer
import MusicTheory.Interval as Interval
import Test exposing (Test, describe, fuzz, test)


all : Test
all =
    describe "Interval Tests"
        [ test "sum of semitones of an interval and it's complementary interval should be 12" <|
            \_ ->
                Interval.all
                    |> List.map
                        (\interval ->
                            ( Interval.semitones interval
                            , Interval.complementary interval |> Interval.semitones
                            )
                        )
                    |> List.all (\( a, b ) -> a + b == 12)
                    |> Expect.true "sum should be 12"
        , test "complementary interval of a perfect unison should be a perfect octave" <|
            \_ ->
                Interval.perfectUnison
                    |> Interval.complementary
                    |> Expect.equal Interval.perfectOctave
        , test "complementary interval of a diminished third  should be an augmented sixth" <|
            \_ ->
                Interval.diminishedThird
                    |> Interval.complementary
                    |> Expect.equal Interval.augmentedSixth
        , fuzz Fuzzer.interval "complementary intervals semitones sum should be 12" <|
            \interval ->
                interval
                    |> Interval.semitones
                    |> Expect.equal
                        (12
                            - (interval
                                |> Interval.complementary
                                |> Interval.semitones
                              )
                        )
        ]
