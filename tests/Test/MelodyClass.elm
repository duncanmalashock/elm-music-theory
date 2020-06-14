module Test.MelodyClass exposing (all)

import Expect
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.Interval as Interval
import MusicTheory.Melody as Melody
import MusicTheory.MelodyClass as MelodyClass
import MusicTheory.Octave as Octave
import MusicTheory.Pitch as Pitch
import MusicTheory.PitchClass as PitchClass
import MusicTheory.Scale as Scale
import MusicTheory.ScaleClass as ScaleClass
import Test exposing (Test, describe, skip, test)


all : Test
all =
    describe "Melody tests"
        [ describe "fromMelody" <|
            [ test "when starting on a chord tone, should correctly convert a Melody with one movement" <|
                \_ ->
                    let
                        expected =
                            MelodyClass.melodyClass
                                MelodyClass.startOnChordTone
                                [ MelodyClass.toChordToneWithGoalInterval
                                    Interval.majorThird
                                ]

                        result =
                            [ Melody.fragment
                                { startingDegree = ( 1, Octave.four )
                                , scaleAndChord =
                                    { chord = Chord.chord PitchClass.c ChordClass.major
                                    , scale = Scale.scale PitchClass.c ScaleClass.major
                                    }
                                }
                                |> Melody.moveByScaleSteps 2
                            ]
                                |> Melody.melody
                                |> MelodyClass.fromMelody
                    in
                    Expect.equal expected result
            , test "when starting on a non-chord tone, should correctly convert a Melody with one movement" <|
                \_ ->
                    let
                        expected =
                            MelodyClass.melodyClass
                                MelodyClass.startOnNonChordTone
                                [ MelodyClass.toChordToneWithGoalInterval
                                    Interval.majorSecond
                                ]

                        result =
                            [ Melody.fragment
                                { startingDegree = ( 2, Octave.four )
                                , scaleAndChord =
                                    { chord = Chord.chord PitchClass.c ChordClass.major
                                    , scale = Scale.scale PitchClass.c ScaleClass.major
                                    }
                                }
                                |> Melody.moveByScaleSteps 1
                            ]
                                |> Melody.melody
                                |> MelodyClass.fromMelody
                    in
                    Expect.equal expected result
            , test "when starting on a non-scale tone, should correctly convert a Melody with one movement" <|
                \_ ->
                    let
                        expected =
                            MelodyClass.melodyClass
                                MelodyClass.startOnNonScaleTone
                                [ MelodyClass.toChordToneWithGoalInterval
                                    (Interval.augmentedUnison |> Interval.reverse)
                                ]

                        result =
                            [ Melody.fragment
                                { startingDegree = ( 1, Octave.four )
                                , scaleAndChord =
                                    { chord = Chord.chord PitchClass.c ChordClass.major
                                    , scale = Scale.scale PitchClass.c ScaleClass.major
                                    }
                                }
                                |> Melody.startWithIntervalOffset Interval.augmentedUnison
                                |> Melody.moveByScaleSteps 0
                            ]
                                |> Melody.melody
                                |> MelodyClass.fromMelody
                    in
                    Expect.equal expected result
            , test "With multiple fragments" <|
                \_ ->
                    let
                        expected =
                            MelodyClass.melodyClass
                                MelodyClass.startOnNonScaleTone
                                [ MelodyClass.toChordToneWithGoalInterval
                                    (Interval.augmentedUnison |> Interval.reverse)
                                , MelodyClass.toNonScaleToneWithGoalInterval
                                    Interval.augmentedFourth
                                , MelodyClass.toChordToneWithGoalInterval
                                    (Interval.augmentedUnison |> Interval.reverse)
                                ]

                        result =
                            [ Melody.fragment
                                { startingDegree = ( 1, Octave.four )
                                , scaleAndChord =
                                    { chord = Chord.chord PitchClass.c ChordClass.major
                                    , scale = Scale.scale PitchClass.c ScaleClass.major
                                    }
                                }
                                |> Melody.startWithIntervalOffset Interval.augmentedUnison
                                |> Melody.moveByScaleSteps 0
                            , Melody.fragment
                                { startingDegree = ( 1, Octave.four )
                                , scaleAndChord =
                                    { chord = Chord.chord PitchClass.f ChordClass.major
                                    , scale = Scale.scale PitchClass.f ScaleClass.major
                                    }
                                }
                                |> Melody.startWithIntervalOffset Interval.augmentedUnison
                                |> Melody.moveByScaleSteps 0
                            ]
                                |> Melody.melody
                                |> MelodyClass.fromMelody
                    in
                    Expect.equal expected result
            ]
        ]
