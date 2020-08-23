module Test.Internal.Melody exposing (all)

import Expect
import MusicTheory.Internal.Chord as Chord
import MusicTheory.Internal.ChordType as ChordType
import MusicTheory.Internal.Interval as Interval
import MusicTheory.Internal.Melody as Melody
import MusicTheory.Internal.Octave as Octave
import MusicTheory.Internal.Pitch as Pitch
import MusicTheory.Internal.PitchClass as PitchClass
import MusicTheory.Internal.Scale as Scale
import MusicTheory.Internal.ScaleType as ScaleType
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
                                        { chord = Chord.chord PitchClass.c ChordType.major
                                        , scale = Scale.scale PitchClass.c ScaleType.major
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
                                            { chord = Chord.chord PitchClass.c ChordType.major
                                            , scale = Scale.scale PitchClass.c ScaleType.major
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
                                            { chord = Chord.chord PitchClass.c ChordType.major
                                            , scale = Scale.scale PitchClass.c ScaleType.major
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
                                            { chord = Chord.chord PitchClass.c ChordType.major
                                            , scale = Scale.scale PitchClass.c ScaleType.major
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
                                            { chord = Chord.chord PitchClass.c ChordType.major
                                            , scale = Scale.scale PitchClass.c ScaleType.major
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
                ]
            ]
        ]
