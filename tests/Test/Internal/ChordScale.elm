module Test.Internal.ChordScale exposing (..)

import Expect
import Music.Internal.Chord as Chord
import Music.Internal.ChordScale as ChordScale
import Music.Internal.ChordType as ChordType
import Music.Internal.PitchClass as PitchClass
import Music.Internal.Scale as Scale
import Music.Internal.ScaleType as ScaleType
import Test exposing (..)


all : Test
all =
    describe "ChordScale Tests"
        [ describe "diatonicChordsAt"
            [ test "B in C ionian scale should be harmonized as B half diminished" <|
                \_ ->
                    let
                        cMajorScale =
                            Scale.scale PitchClass.c ScaleType.ionian

                        expected =
                            [ Chord.chord PitchClass.b ChordType.halfDiminished
                            ]

                        actual =
                            ChordScale.diatonicChordsAt
                                { scale = cMajorScale
                                , root = PitchClass.b
                                , chordTypesAllowed = [ ChordType.halfDiminished ]
                                }
                    in
                    Expect.equal expected actual
            , test "D in C ionian scale should be harmonized as D minor seventh" <|
                \_ ->
                    let
                        cMajorScale =
                            Scale.scale PitchClass.c ScaleType.ionian

                        expected =
                            [ Chord.chord PitchClass.d ChordType.minorSeventh
                            ]

                        actual =
                            ChordScale.diatonicChordsAt
                                { scale = cMajorScale
                                , root = PitchClass.d
                                , chordTypesAllowed = [ ChordType.minorSeventh ]
                                }
                    in
                    Expect.equal expected actual
            , test "Bb in C ionian scale should not have any valid harmonies" <|
                \_ ->
                    let
                        cMajorScale =
                            Scale.scale PitchClass.c ScaleType.ionian

                        expected =
                            []

                        actual =
                            ChordScale.diatonicChordsAt
                                { scale = cMajorScale
                                , root = PitchClass.bFlat
                                , chordTypesAllowed = ChordType.all
                                }
                    in
                    Expect.equal expected actual
            ]
        ]
