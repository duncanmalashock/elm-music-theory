module Test.Internal.Harmonize exposing (..)

import Expect
import MusicTheory.Internal.Chord as Chord
import MusicTheory.Internal.ChordClass as ChordClass
import MusicTheory.Internal.HarmonicContext as HarmonicContext
import MusicTheory.Internal.Harmonize as Harmonize
import MusicTheory.Internal.Pitch as Pitch
import MusicTheory.Internal.PitchClass as PitchClass
import MusicTheory.Internal.Scale as Scale
import MusicTheory.Internal.ScaleClass as ScaleClass
import Test exposing (..)


all : Test
all =
    describe "Harmonize tests"
        [ describe "execute"
            [ test "with no tactics, should harmonize only chord tones with the chord in harmonic context" <|
                \_ ->
                    let
                        cMajor6 =
                            Chord.chord PitchClass.c ChordClass.majorSix

                        expected : List (Maybe Chord.Chord)
                        expected =
                            [ Just cMajor6
                            , Nothing
                            , Nothing
                            , Just cMajor6
                            ]

                        result : List (Maybe Chord.Chord)
                        result =
                            Harmonize.execute
                                { ifNonChordTone =
                                    \context maybeNextContext ->
                                        Nothing
                                , ifNonScaleTone =
                                    \context maybeNextContext ->
                                        Nothing
                                }
                                [ HarmonicContext.init
                                    { pitch = Pitch.c4
                                    , chord = Chord.chord PitchClass.c ChordClass.majorSix
                                    , scale = Scale.scale PitchClass.c ScaleClass.ionian
                                    }
                                , HarmonicContext.init
                                    { pitch = Pitch.d4
                                    , chord = Chord.chord PitchClass.c ChordClass.majorSix
                                    , scale = Scale.scale PitchClass.c ScaleClass.ionian
                                    }
                                , HarmonicContext.init
                                    { pitch = Pitch.dSharp4
                                    , chord = Chord.chord PitchClass.c ChordClass.majorSix
                                    , scale = Scale.scale PitchClass.c ScaleClass.ionian
                                    }
                                , HarmonicContext.init
                                    { pitch = Pitch.e4
                                    , chord = Chord.chord PitchClass.c ChordClass.majorSix
                                    , scale = Scale.scale PitchClass.c ScaleClass.ionian
                                    }
                                ]
                                |> List.map Harmonize.chordFromHarmonizedContext
                    in
                    Expect.equal expected result
            , test "using parallel approaches, should harmonize with parallel harmony" <|
                \_ ->
                    let
                        cMajor6 =
                            Chord.chord PitchClass.c ChordClass.majorSix

                        bFlatMajor6 =
                            Chord.chord PitchClass.bFlat ChordClass.majorSix

                        bMajor6 =
                            Chord.chord PitchClass.b ChordClass.majorSix

                        expected : List (Maybe Chord.Chord)
                        expected =
                            [ Just cMajor6
                            , Just bFlatMajor6
                            , Just bMajor6
                            , Just cMajor6
                            ]

                        result : List (Maybe Chord.Chord)
                        result =
                            [ HarmonicContext.init
                                { pitch = Pitch.c4
                                , chord = Chord.chord PitchClass.c ChordClass.majorSix
                                , scale = Scale.scale PitchClass.c ScaleClass.ionian
                                }
                            , HarmonicContext.init
                                { pitch = Pitch.d4
                                , chord = Chord.chord PitchClass.c ChordClass.majorSix
                                , scale = Scale.scale PitchClass.c ScaleClass.ionian
                                }
                            , HarmonicContext.init
                                { pitch = Pitch.dSharp4
                                , chord = Chord.chord PitchClass.c ChordClass.majorSix
                                , scale = Scale.scale PitchClass.c ScaleClass.ionian
                                }
                            , HarmonicContext.init
                                { pitch = Pitch.e4
                                , chord = Chord.chord PitchClass.c ChordClass.majorSix
                                , scale = Scale.scale PitchClass.c ScaleClass.ionian
                                }
                            ]
                                |> Harmonize.execute
                                    { ifNonChordTone =
                                        Harmonize.parallelApproach
                                    , ifNonScaleTone =
                                        Harmonize.parallelApproach
                                    }
                                |> List.map Harmonize.chordFromHarmonizedContext
                    in
                    Expect.equal expected result
            , describe "toneFromHarmonicContext"
                [ test "should return nonScaleTone if the pitch is not in the scale" <|
                    \_ ->
                        let
                            expected =
                                Harmonize.nonScaleTone

                            result =
                                Harmonize.toneFromHarmonicContext
                                    (HarmonicContext.init
                                        { pitch = Pitch.dFlat1
                                        , chord = Chord.chord PitchClass.c ChordClass.major
                                        , scale = Scale.scale PitchClass.c ScaleClass.ionian
                                        }
                                    )
                        in
                        Expect.equal expected result
                , test "should return nonChordTone if the pitch is in the scale but not the chord" <|
                    \_ ->
                        let
                            expected =
                                Harmonize.nonChordTone

                            result =
                                Harmonize.toneFromHarmonicContext
                                    (HarmonicContext.init
                                        { pitch = Pitch.d1
                                        , chord = Chord.chord PitchClass.c ChordClass.major
                                        , scale = Scale.scale PitchClass.c ScaleClass.ionian
                                        }
                                    )
                        in
                        Expect.equal expected result
                , test "should return chordTone if the pitch is in the chord" <|
                    \_ ->
                        let
                            expected =
                                Harmonize.chordTone

                            result =
                                Harmonize.toneFromHarmonicContext
                                    (HarmonicContext.init
                                        { pitch = Pitch.e1
                                        , chord = Chord.chord PitchClass.c ChordClass.major
                                        , scale = Scale.scale PitchClass.c ScaleClass.ionian
                                        }
                                    )
                        in
                        Expect.equal expected result
                ]
            ]
        ]
