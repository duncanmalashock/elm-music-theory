module Test.Internal.ChordScale exposing (..)

import Expect
import MusicTheory.Internal.Chord as Chord
import MusicTheory.Internal.ChordClass as ChordClass
import MusicTheory.Internal.ChordScale as ChordScale
import MusicTheory.Internal.PitchClass as PitchClass
import MusicTheory.Internal.Scale as Scale
import MusicTheory.Internal.ScaleClass as ScaleClass
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
                            [ Chord.chord PitchClass.b ChordClass.halfDiminished
                            ]

                        actual =
                            ChordScale.diatonicChordsAt
                                { scale = cMajorScale
                                , root = PitchClass.b
                                , chordClassesAllowed = [ ChordClass.halfDiminished ]
                                }
                    in
                    Expect.equal expected actual
            , test "D in C ionian scale should be harmonized as D minor seventh" <|
                \_ ->
                    let
                        cMajorScale =
                            Scale.scale PitchClass.c ScaleClass.ionian

                        expected =
                            [ Chord.chord PitchClass.d ChordClass.minorSeventh
                            ]

                        actual =
                            ChordScale.diatonicChordsAt
                                { scale = cMajorScale
                                , root = PitchClass.d
                                , chordClassesAllowed = [ ChordClass.minorSeventh ]
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
                                , chordClassesAllowed = ChordClass.all
                                }
                    in
                    Expect.equal expected actual
            ]
        ]
