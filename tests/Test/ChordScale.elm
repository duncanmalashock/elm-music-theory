module Test.ChordScale exposing (..)

import Expect
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.ChordScale as ChordScale
import MusicTheory.Interval as Interval
import MusicTheory.PitchClass as PitchClass
import MusicTheory.Scale as Scale
import MusicTheory.ScaleClass as ScaleClass
import Test exposing (..)


all : Test
all =
    describe "ChordScale Tests"
        [ describe "diatonicChordsAtInterval"
            [ test "Minor second below C major seventh in C ionian scale should be harmonized as B half diminished" <|
                \_ ->
                    let
                        cMajorScale =
                            Scale.scale PitchClass.c ScaleClass.ionian

                        cMajorSeventh =
                            Chord.chord PitchClass.c ChordClass.majorSeventh

                        expected =
                            [ Chord.chord PitchClass.b ChordClass.diminished
                            , Chord.chord PitchClass.b ChordClass.halfDiminished
                            ]

                        actual =
                            ChordScale.diatonicChordsAtInterval
                                { targetChord = cMajorSeventh
                                , scale = cMajorScale
                                , approachByInterval = Interval.reverse Interval.minorSecond
                                }
                    in
                    Expect.equal expected actual
            , test "Major second above C major seventh in C ionian scale should be harmonized as D minor seventh" <|
                \_ ->
                    let
                        cMajorScale =
                            Scale.scale PitchClass.c ScaleClass.ionian

                        cMajorSeventh =
                            Chord.chord PitchClass.c ChordClass.majorSeventh

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
                            ChordScale.diatonicChordsAtInterval
                                { targetChord = cMajorSeventh
                                , scale = cMajorScale
                                , approachByInterval = Interval.majorSecond
                                }
                    in
                    Expect.equal expected actual
            ]
        ]
