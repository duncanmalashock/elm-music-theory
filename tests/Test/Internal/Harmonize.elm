module Test.Internal.Harmonize exposing (..)

import Expect
import Music.Internal.Chord as Chord
import Music.Internal.ChordType as ChordType
import Music.Internal.HarmonicContext as HarmonicContext
import Music.Internal.Harmonize as Harmonize
import Music.Internal.Pitch as Pitch
import Music.Internal.PitchClass as PitchClass
import Music.Internal.Scale as Scale
import Music.Internal.ScaleType as ScaleType
import Test exposing (..)


all : Test
all =
    describe "Harmonize tests"
        [ describe "execute"
            [ test "with no tactics, should harmonize only chord tones with the chord in harmonic context" <|
                \_ ->
                    let
                        cMajor6 =
                            Chord.chord PitchClass.c ChordType.majorSix

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
                                    , chord = Chord.chord PitchClass.c ChordType.majorSix
                                    , scale = Scale.scale PitchClass.c ScaleType.ionian
                                    }
                                , HarmonicContext.init
                                    { pitch = Pitch.d4
                                    , chord = Chord.chord PitchClass.c ChordType.majorSix
                                    , scale = Scale.scale PitchClass.c ScaleType.ionian
                                    }
                                , HarmonicContext.init
                                    { pitch = Pitch.dSharp4
                                    , chord = Chord.chord PitchClass.c ChordType.majorSix
                                    , scale = Scale.scale PitchClass.c ScaleType.ionian
                                    }
                                , HarmonicContext.init
                                    { pitch = Pitch.e4
                                    , chord = Chord.chord PitchClass.c ChordType.majorSix
                                    , scale = Scale.scale PitchClass.c ScaleType.ionian
                                    }
                                ]
                                |> List.map Harmonize.chordFromHarmonizedContext
                    in
                    Expect.equal expected result
            , test "using parallel approaches, should harmonize with parallel harmony" <|
                \_ ->
                    let
                        cMajor6 =
                            Chord.chord PitchClass.c ChordType.majorSix

                        bFlatMajor6 =
                            Chord.chord PitchClass.bFlat ChordType.majorSix

                        bMajor6 =
                            Chord.chord PitchClass.b ChordType.majorSix

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
                                , chord = Chord.chord PitchClass.c ChordType.majorSix
                                , scale = Scale.scale PitchClass.c ScaleType.ionian
                                }
                            , HarmonicContext.init
                                { pitch = Pitch.d4
                                , chord = Chord.chord PitchClass.c ChordType.majorSix
                                , scale = Scale.scale PitchClass.c ScaleType.ionian
                                }
                            , HarmonicContext.init
                                { pitch = Pitch.dSharp4
                                , chord = Chord.chord PitchClass.c ChordType.majorSix
                                , scale = Scale.scale PitchClass.c ScaleType.ionian
                                }
                            , HarmonicContext.init
                                { pitch = Pitch.e4
                                , chord = Chord.chord PitchClass.c ChordType.majorSix
                                , scale = Scale.scale PitchClass.c ScaleType.ionian
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
            , test "using diatonic approaches, should harmonize with diatonic harmony" <|
                \_ ->
                    let
                        cMajor6 =
                            Chord.chord PitchClass.c ChordType.majorSix

                        dMinor7 =
                            Chord.chord PitchClass.d ChordType.minorSeventh

                        expected : List (Maybe Chord.Chord)
                        expected =
                            [ Just cMajor6
                            , Just dMinor7
                            , Just cMajor6
                            ]

                        result : List (Maybe Chord.Chord)
                        result =
                            [ HarmonicContext.init
                                { pitch = Pitch.e4
                                , chord = Chord.chord PitchClass.c ChordType.majorSix
                                , scale = Scale.scale PitchClass.c ScaleType.ionian
                                }
                            , HarmonicContext.init
                                { pitch = Pitch.d4
                                , chord = Chord.chord PitchClass.c ChordType.majorSix
                                , scale = Scale.scale PitchClass.c ScaleType.ionian
                                }
                            , HarmonicContext.init
                                { pitch = Pitch.c4
                                , chord = Chord.chord PitchClass.c ChordType.majorSix
                                , scale = Scale.scale PitchClass.c ScaleType.ionian
                                }
                            ]
                                |> Harmonize.execute
                                    { ifNonChordTone =
                                        Harmonize.diatonicApproach
                                            [ ChordType.majorSix
                                            , ChordType.majorSeventh
                                            , ChordType.minorSeventh
                                            , ChordType.halfDiminished
                                            , ChordType.dominantSeventh
                                            ]
                                    , ifNonScaleTone =
                                        Harmonize.parallelApproach
                                    }
                                |> List.map Harmonize.chordFromHarmonizedContext
                    in
                    Expect.equal expected result
            , test "using diminished approaches, should harmonize with diminished harmony" <|
                \_ ->
                    let
                        cMajor6 =
                            Chord.chord PitchClass.c ChordType.majorSix

                        dDiminishedSeventh =
                            Chord.chord PitchClass.d ChordType.diminishedSeventh

                        expected : List (Maybe Chord.Chord)
                        expected =
                            [ Just cMajor6
                            , Just dDiminishedSeventh
                            , Just cMajor6
                            ]

                        result : List (Maybe Chord.Chord)
                        result =
                            [ HarmonicContext.init
                                { pitch = Pitch.e4
                                , chord = Chord.chord PitchClass.c ChordType.majorSix
                                , scale = Scale.scale PitchClass.c ScaleType.ionian
                                }
                            , HarmonicContext.init
                                { pitch = Pitch.d4
                                , chord = Chord.chord PitchClass.c ChordType.majorSix
                                , scale = Scale.scale PitchClass.c ScaleType.ionian
                                }
                            , HarmonicContext.init
                                { pitch = Pitch.c4
                                , chord = Chord.chord PitchClass.c ChordType.majorSix
                                , scale = Scale.scale PitchClass.c ScaleType.ionian
                                }
                            ]
                                |> Harmonize.execute
                                    { ifNonChordTone =
                                        Harmonize.diminishedApproach
                                    , ifNonScaleTone =
                                        Harmonize.diminishedApproach
                                    }
                                |> List.map Harmonize.chordFromHarmonizedContext
                    in
                    Expect.equal expected result
            , test "using dominant approaches, should harmonize with dominant harmony" <|
                \_ ->
                    let
                        cMajor6 =
                            Chord.chord PitchClass.c ChordType.majorSix

                        gDominantSeventh =
                            Chord.chord PitchClass.g ChordType.dominantSeventh

                        expected : List (Maybe Chord.Chord)
                        expected =
                            [ Just cMajor6
                            , Just gDominantSeventh
                            , Just cMajor6
                            ]

                        result : List (Maybe Chord.Chord)
                        result =
                            [ HarmonicContext.init
                                { pitch = Pitch.e4
                                , chord = Chord.chord PitchClass.c ChordType.majorSix
                                , scale = Scale.scale PitchClass.c ScaleType.ionian
                                }
                            , HarmonicContext.init
                                { pitch = Pitch.d4
                                , chord = Chord.chord PitchClass.c ChordType.majorSix
                                , scale = Scale.scale PitchClass.c ScaleType.ionian
                                }
                            , HarmonicContext.init
                                { pitch = Pitch.c4
                                , chord = Chord.chord PitchClass.c ChordType.majorSix
                                , scale = Scale.scale PitchClass.c ScaleType.ionian
                                }
                            ]
                                |> Harmonize.execute
                                    { ifNonChordTone =
                                        Harmonize.dominantApproach
                                            [ ChordType.dominantSeventh
                                            ]
                                    , ifNonScaleTone =
                                        Harmonize.dominantApproach
                                            [ ChordType.dominantSeventh
                                            ]
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
                                        , chord = Chord.chord PitchClass.c ChordType.major
                                        , scale = Scale.scale PitchClass.c ScaleType.ionian
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
                                        , chord = Chord.chord PitchClass.c ChordType.major
                                        , scale = Scale.scale PitchClass.c ScaleType.ionian
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
                                        , chord = Chord.chord PitchClass.c ChordType.major
                                        , scale = Scale.scale PitchClass.c ScaleType.ionian
                                        }
                                    )
                        in
                        Expect.equal expected result
                ]
            ]
        ]
