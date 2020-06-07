module Test.Melody exposing (all)

import Expect
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.Interval as Interval
import MusicTheory.Melody as Melody
import MusicTheory.Octave as Octave
import MusicTheory.Pitch as Pitch
import MusicTheory.PitchClass as PitchClass
import MusicTheory.Scale as Scale
import MusicTheory.ScaleClass as ScaleClass
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Melody tests"
        [ describe "toList" <|
            [ test "should convert a melody correctly" <|
                \_ ->
                    let
                        result =
                            [ Pitch.c4
                            , Pitch.e4
                            , Pitch.d4
                            , Pitch.b3
                            , Pitch.a3
                            , Pitch.b3
                            , Pitch.c4
                            ]

                        expected =
                            [ Melody.fragment
                                { startingDegree = 1
                                , startingOctave = Octave.four
                                , steps = [ 0, 2, -1, -2 ]
                                , chord = Chord.chord PitchClass.c ChordClass.major
                                , scale = Scale.scale PitchClass.c ScaleClass.major
                                }
                            , Melody.fragment
                                { startingDegree = 2
                                , startingOctave = Octave.three
                                , steps = [ 0, 1 ]
                                , chord = Chord.chord PitchClass.g ChordClass.major
                                , scale = Scale.scale PitchClass.g ScaleClass.mixolydian
                                }
                            , Melody.fragment
                                { startingDegree = 1
                                , startingOctave = Octave.four
                                , steps = [ 0 ]
                                , chord = Chord.chord PitchClass.c ChordClass.major
                                , scale = Scale.scale PitchClass.c ScaleClass.major
                                }
                            ]
                                |> Melody.melody
                                |> Melody.toList
                    in
                    Expect.equal expected result
            ]
        , describe "toMelodyClass" <|
            [ test "should convert a Melody to a MelodyClass" <|
                \_ ->
                    let
                        result =
                            Melody.melodyClass
                                [ Melody.toChordToneWithGoalInterval
                                    Interval.majorThird
                                , Melody.toNonChordToneWithGoalInterval
                                    (Interval.majorSecond |> Interval.reverse)
                                , Melody.toNonChordToneWithGoalInterval
                                    (Interval.minorThird |> Interval.reverse)
                                , Melody.toNonChordToneWithGoalInterval
                                    (Interval.majorSecond |> Interval.reverse)
                                , Melody.toChordToneWithGoalInterval
                                    Interval.majorSecond
                                , Melody.toChordToneWithGoalInterval
                                    Interval.minorSecond
                                ]

                        expected =
                            [ Melody.fragment
                                { startingDegree = 1
                                , startingOctave = Octave.four
                                , steps = [ 0, 2, -1, -2 ]
                                , chord = Chord.chord PitchClass.c ChordClass.major
                                , scale = Scale.scale PitchClass.c ScaleClass.major
                                }
                            , Melody.fragment
                                { startingDegree = 2
                                , startingOctave = Octave.three
                                , steps = [ 0, 1 ]
                                , chord = Chord.chord PitchClass.g ChordClass.major
                                , scale = Scale.scale PitchClass.g ScaleClass.mixolydian
                                }
                            , Melody.fragment
                                { startingDegree = 1
                                , startingOctave = Octave.four
                                , steps = [ 0 ]
                                , chord = Chord.chord PitchClass.c ChordClass.major
                                , scale = Scale.scale PitchClass.c ScaleClass.major
                                }
                            ]
                                |> Melody.melody
                                |> Melody.toMelodyClass
                    in
                    Expect.equal expected result
            ]
        ]
