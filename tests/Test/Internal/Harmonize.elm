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
        [ describe "toneFromHarmonicContext"
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
