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
import Test exposing (Test, describe, skip, test)


all : Test
all =
    describe "Melody tests"
        [ describe "toList" <|
            [ describe "Melody with one fragment" <|
                [ test "with no movements, should include only starting pitch" <|
                    \_ ->
                        let
                            expected =
                                [ Pitch.c4
                                ]

                            result =
                                Melody.fragment
                                    { startingDegree = ( 1, Octave.four )
                                    , scaleAndChord =
                                        { chord = Chord.chord PitchClass.c ChordClass.major
                                        , scale = Scale.scale PitchClass.c ScaleClass.major
                                        }
                                    }
                                    |> Melody.melody
                                    |> Melody.toList
                        in
                        Expect.equal expected result
                , describe "startWithIntervalOffset" <|
                    [ test "with no movements, should include starting pitch with interval offset" <|
                        \_ ->
                            let
                                expected =
                                    [ Pitch.cSharp4
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
                                    )
                                        |> Melody.melody
                                        |> Melody.toList
                            in
                            Expect.equal expected result
                    , test "with a movement by 0 scalesteps, should return to scale degree" <|
                        \_ ->
                            let
                                expected =
                                    [ Pitch.cSharp4
                                    , Pitch.c4
                                    , Pitch.dSharp4
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
                                        |> Melody.moveChromaticallyAfterScaleSteps 1 Interval.augmentedUnison
                                    )
                                        |> Melody.melody
                                        |> Melody.toList
                            in
                            Expect.equal expected result
                    ]
                , describe "moveByScaleSteps" <|
                    [ test "should add correct pitches the correct number of scale steps away" <|
                        \_ ->
                            let
                                expected =
                                    [ Pitch.c4
                                    , Pitch.e4
                                    , Pitch.g4
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
                                        |> Melody.moveByScaleSteps 2
                                    )
                                        |> Melody.melody
                                        |> Melody.toList
                            in
                            Expect.equal expected result
                    ]
                , describe "repeatLastPitch" <|
                    [ test "should add correct pitches with and without chromatic alteration" <|
                        \_ ->
                            let
                                expected =
                                    [ Pitch.cSharp4
                                    , Pitch.cSharp4
                                    , Pitch.e4
                                    , Pitch.e4
                                    , Pitch.eSharp4
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
                                        |> Melody.repeatLastPitch
                                        |> Melody.moveByScaleSteps 2
                                        |> Melody.repeatLastPitch
                                        |> Melody.moveChromaticallyFromCurrentScaleStep Interval.augmentedUnison
                                    )
                                        |> Melody.melody
                                        |> Melody.toList
                            in
                            Expect.equal expected result
                    ]
                , skip <|
                    describe "findMatchingTonesByStepDistance" <|
                        [ test "should only find matching tones in the same direction as the interval given" <|
                            \_ ->
                                let
                                    expected =
                                        []

                                    fragment =
                                        Melody.fragment
                                            { startingDegree = ( 1, Octave.four )
                                            , scaleAndChord =
                                                { scale = Scale.scale PitchClass.c ScaleClass.major
                                                , chord = Chord.chord PitchClass.c ChordClass.major
                                                }
                                            }

                                    result =
                                        Melody.findMatchingTonesByStepDistance (always True) fragment Interval.minorSecond
                                in
                                Expect.equal expected result
                        ]
                ]
            ]
        ]
