module Test.Interval exposing (all)

import Expect
import MusicTheory.Interval as Interval
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Interval Tests"
        [ test "sum of semitones of an interval and its complementary interval should be 12" <|
            \_ ->
                Interval.allSimple
                    |> List.map
                        (\interval ->
                            Interval.semitones interval
                                + (Interval.complement interval
                                    |> Interval.semitones
                                  )
                        )
                    |> Expect.equal (List.repeat (List.length Interval.allSimple) 12)
        , test "complementary interval of a perfect unison should be a perfect octave" <|
            \_ ->
                Interval.perfectUnison
                    |> Interval.complement
                    |> Expect.equal Interval.perfectOctave
        , test "complementary interval of a diminished third should be an augmented sixth" <|
            \_ ->
                Interval.diminishedThird
                    |> Interval.complement
                    |> Expect.equal Interval.augmentedSixth
        , test "major third plus octave should contain 16 semitones" <|
            \_ ->
                Interval.majorThird
                    |> Interval.addOctave
                    |> Interval.semitones
                    |> Expect.equal 16
        ]
