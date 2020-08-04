module Test.ChordScale exposing (..)

import Expect
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.ChordScale as ChordScale
import MusicTheory.PitchClass as PitchClass
import MusicTheory.Scale as Scale
import MusicTheory.ScaleClass as ScaleClass
import Test exposing (..)


all : Test
all =
    describe "ChordScale Tests"
        [ describe "diatonicChordsAt"
            [ test "B in C ionian scale should be harmonized as B half diminished" <|
                \_ ->
                    let
                        cMajorScale =
                            Scale.scale PitchClass.c ScaleClass.ionian

                        expected =
                            [ Chord.chord PitchClass.b ChordClass.diminished
                            , Chord.chord PitchClass.b ChordClass.halfDiminished
                            ]

                        actual =
                            ChordScale.diatonicChordsAt
                                { scale = cMajorScale
                                , root = PitchClass.b
                                }
                    in
                    Expect.equal expected actual
            , test "D in C ionian scale should be harmonized as D minor seventh" <|
                \_ ->
                    let
                        cMajorScale =
                            Scale.scale PitchClass.c ScaleClass.ionian

                        expected =
                            [ Chord.chord PitchClass.d ChordClass.dominantSeventhSus4
                            , Chord.chord PitchClass.d ChordClass.minor
                            , Chord.chord PitchClass.d ChordClass.minorAddNine
                            , Chord.chord PitchClass.d ChordClass.minorNinth
                            , Chord.chord PitchClass.d ChordClass.minorSeventh
                            , Chord.chord PitchClass.d ChordClass.minorSix
                            , Chord.chord PitchClass.d ChordClass.minorSixNine
                            , Chord.chord PitchClass.d ChordClass.sus2
                            , Chord.chord PitchClass.d ChordClass.sus4
                            ]

                        actual =
                            ChordScale.diatonicChordsAt
                                { scale = cMajorScale
                                , root = PitchClass.d
                                }
                    in
                    Expect.equal expected actual
            , test "Bb in C ionian scale should not have any valid harmonies" <|
                \_ ->
                    let
                        cMajorScale =
                            Scale.scale PitchClass.c ScaleClass.ionian

                        expected =
                            []

                        actual =
                            ChordScale.diatonicChordsAt
                                { scale = cMajorScale
                                , root = PitchClass.bFlat
                                }
                    in
                    Expect.equal expected actual
            ]
        ]
