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
    describe "MelodyClass tests"
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
                            (Melody.fragment
                                { startingDegree = ( 1, Octave.four )
                                , scaleAndChord =
                                    { chord = Chord.chord PitchClass.c ChordClass.major
                                    , scale = Scale.scale PitchClass.c ScaleClass.major
                                    }
                                }
                                |> Melody.moveByScaleSteps 2
                            )
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
                            (Melody.fragment
                                { startingDegree = ( 2, Octave.four )
                                , scaleAndChord =
                                    { chord = Chord.chord PitchClass.c ChordClass.major
                                    , scale = Scale.scale PitchClass.c ScaleClass.major
                                    }
                                }
                                |> Melody.moveByScaleSteps 1
                            )
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
                            (Melody.fragment
                                { startingDegree = ( 1, Octave.four )
                                , scaleAndChord =
                                    { chord = Chord.chord PitchClass.c ChordClass.major
                                    , scale = Scale.scale PitchClass.c ScaleClass.major
                                    }
                                }
                                |> Melody.startWithIntervalOffset Interval.augmentedUnison
                                |> Melody.moveByScaleSteps 0
                            )
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
                            (Melody.fragment
                                { startingDegree = ( 1, Octave.four )
                                , scaleAndChord =
                                    { chord = Chord.chord PitchClass.c ChordClass.major
                                    , scale = Scale.scale PitchClass.c ScaleClass.major
                                    }
                                }
                                |> Melody.startWithIntervalOffset Interval.augmentedUnison
                                |> Melody.moveByScaleSteps 0
                            )
                                |> Melody.melody
                                |> Melody.addFragment
                                    (Melody.fragment
                                        { startingDegree = ( 1, Octave.four )
                                        , scaleAndChord =
                                            { chord = Chord.chord PitchClass.f ChordClass.major
                                            , scale = Scale.scale PitchClass.f ScaleClass.major
                                            }
                                        }
                                        |> Melody.startWithIntervalOffset Interval.augmentedUnison
                                        |> Melody.moveByScaleSteps 0
                                    )
                                |> MelodyClass.fromMelody
                    in
                    Expect.equal expected result
            ]
        , describe "generateMelodies" <|
            [ describe "Melodies with only a start" <|
                [ test "beginning on a chord tone" <|
                    \_ ->
                        let
                            expected =
                                [ 1, 3, 5 ]
                                    |> List.map
                                        (\degree ->
                                            Melody.melody
                                                (Melody.fragment
                                                    { startingDegree = ( degree, Octave.four )
                                                    , scaleAndChord =
                                                        { chord = Chord.chord PitchClass.c ChordClass.major
                                                        , scale = Scale.scale PitchClass.c ScaleClass.major
                                                        }
                                                    }
                                                )
                                        )
                                    |> melodiesToStrings

                            melodyClass =
                                Melody.fragment
                                    { startingDegree = ( 1, Octave.four )
                                    , scaleAndChord =
                                        { chord = Chord.chord PitchClass.c ChordClass.major
                                        , scale = Scale.scale PitchClass.c ScaleClass.major
                                        }
                                    }
                                    |> Melody.melody
                                    |> MelodyClass.fromMelody

                            result =
                                MelodyClass.config
                                    { melodyClass = melodyClass
                                    , scalesAndChords =
                                        ( { chord = Chord.chord PitchClass.c ChordClass.major
                                          , scale = Scale.scale PitchClass.c ScaleClass.major
                                          }
                                        , []
                                        )
                                    }
                                    |> MelodyClass.generateMelodies
                                    |> melodiesToStrings
                        in
                        Expect.equal expected result
                , test "beginning on a non-chord tone" <|
                    \_ ->
                        let
                            expected =
                                [ 2, 4, 6, 7 ]
                                    |> List.map
                                        (\degree ->
                                            Melody.melody
                                                (Melody.fragment
                                                    { startingDegree = ( degree, Octave.four )
                                                    , scaleAndChord =
                                                        { chord = Chord.chord PitchClass.c ChordClass.major
                                                        , scale = Scale.scale PitchClass.c ScaleClass.major
                                                        }
                                                    }
                                                )
                                        )
                                    |> melodiesToStrings

                            melodyClass =
                                Melody.fragment
                                    { startingDegree = ( 2, Octave.four )
                                    , scaleAndChord =
                                        { chord = Chord.chord PitchClass.c ChordClass.major
                                        , scale = Scale.scale PitchClass.c ScaleClass.major
                                        }
                                    }
                                    |> Melody.melody
                                    |> MelodyClass.fromMelody

                            result =
                                MelodyClass.config
                                    { melodyClass = melodyClass
                                    , scalesAndChords =
                                        ( { chord = Chord.chord PitchClass.c ChordClass.major
                                          , scale = Scale.scale PitchClass.c ScaleClass.major
                                          }
                                        , []
                                        )
                                    }
                                    |> MelodyClass.generateMelodies
                                    |> melodiesToStrings
                        in
                        Expect.equal expected result
                ]
            , skip <|
                describe "Melodies with a movement" <|
                    [ test "beginning on a chord tone" <|
                        \_ ->
                            let
                                expected =
                                    []

                                melodyClass =
                                    Melody.fragment
                                        { startingDegree = ( 1, Octave.four )
                                        , scaleAndChord =
                                            { chord = Chord.chord PitchClass.c ChordClass.major
                                            , scale = Scale.scale PitchClass.c ScaleClass.major
                                            }
                                        }
                                        |> Melody.moveByScaleSteps 2
                                        |> Melody.melody
                                        |> MelodyClass.fromMelody

                                result =
                                    MelodyClass.config
                                        { melodyClass = melodyClass
                                        , scalesAndChords =
                                            ( { chord = Chord.chord PitchClass.c ChordClass.major
                                              , scale = Scale.scale PitchClass.c ScaleClass.major
                                              }
                                            , [ ( 1
                                                , { chord = Chord.chord PitchClass.c ChordClass.major
                                                  , scale = Scale.scale PitchClass.c ScaleClass.major
                                                  }
                                                )
                                              ]
                                            )
                                        }
                                        |> MelodyClass.generateMelodies
                                        |> melodiesToStrings
                            in
                            Expect.equal expected result
                    , test "beginning on a non-chord tone" <|
                        \_ ->
                            let
                                expected =
                                    []

                                melodyClass =
                                    Melody.fragment
                                        { startingDegree = ( 2, Octave.four )
                                        , scaleAndChord =
                                            { chord = Chord.chord PitchClass.c ChordClass.major
                                            , scale = Scale.scale PitchClass.c ScaleClass.major
                                            }
                                        }
                                        |> Melody.moveByScaleSteps 3
                                        |> Melody.melody
                                        |> MelodyClass.fromMelody

                                result =
                                    MelodyClass.config
                                        { melodyClass = melodyClass
                                        , scalesAndChords =
                                            ( { chord = Chord.chord PitchClass.c ChordClass.major
                                              , scale = Scale.scale PitchClass.c ScaleClass.major
                                              }
                                            , [ ( 1
                                                , { chord = Chord.chord PitchClass.c ChordClass.major
                                                  , scale = Scale.scale PitchClass.c ScaleClass.major
                                                  }
                                                )
                                              ]
                                            )
                                        }
                                        |> MelodyClass.generateMelodies
                                        |> melodiesToStrings
                            in
                            Expect.equal expected result
                    ]
            ]
        , skip <|
            describe "Melodies with multiple fragments" <|
                [ test "beginning on a chord tone" <|
                    \_ ->
                        let
                            expected =
                                []

                            melodyClass =
                                Melody.fragment
                                    { startingDegree = ( 1, Octave.four )
                                    , scaleAndChord =
                                        { chord = Chord.chord PitchClass.c ChordClass.major
                                        , scale = Scale.scale PitchClass.c ScaleClass.major
                                        }
                                    }
                                    |> Melody.moveByScaleSteps 2
                                    |> Melody.melody
                                    |> Melody.addFragment
                                        (Melody.fragment
                                            { startingDegree = ( 1, Octave.four )
                                            , scaleAndChord =
                                                { chord = Chord.chord PitchClass.f ChordClass.major
                                                , scale = Scale.scale PitchClass.f ScaleClass.major
                                                }
                                            }
                                            |> Melody.moveByScaleSteps 2
                                        )
                                    |> MelodyClass.fromMelody

                            result =
                                MelodyClass.config
                                    { melodyClass = melodyClass
                                    , scalesAndChords =
                                        ( { chord = Chord.chord PitchClass.c ChordClass.major
                                          , scale = Scale.scale PitchClass.c ScaleClass.major
                                          }
                                        , [ ( 1
                                            , { chord = Chord.chord PitchClass.c ChordClass.major
                                              , scale = Scale.scale PitchClass.c ScaleClass.major
                                              }
                                            )
                                          , ( 2
                                            , { chord = Chord.chord PitchClass.f ChordClass.major
                                              , scale = Scale.scale PitchClass.f ScaleClass.major
                                              }
                                            )
                                          ]
                                        )
                                    }
                                    |> MelodyClass.generateMelodies
                                    |> melodiesToStrings
                        in
                        Expect.equal expected result
                ]
        ]


melodiesToStrings : List Melody.Melody -> List String
melodiesToStrings =
    List.map
        (Melody.toList
            >> List.map Pitch.toString
            >> String.join " "
        )
