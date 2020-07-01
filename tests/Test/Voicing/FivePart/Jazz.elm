module Test.Voicing.FivePart.Jazz exposing (..)

import Expect
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.InstrumentRanges as InstrumentRanges
import MusicTheory.PitchClass as PitchClass
import MusicTheory.Voicing.FivePart as FivePart
import MusicTheory.Voicing.FivePart.Jazz as JazzFivePart
import Test exposing (..)


testRanges : FivePart.Ranges
testRanges =
    { voiceOne = InstrumentRanges.sopranoVoice
    , voiceTwo = InstrumentRanges.altoVoice
    , voiceThree = InstrumentRanges.tenorVoice
    , voiceFour = InstrumentRanges.tenorVoice
    , voiceFive = InstrumentRanges.bassVoice
    }


all : Test
all =
    describe "all"
        [ describe "close"
            [ test "should generate voicings of the chord" <|
                \_ ->
                    let
                        result =
                            JazzFivePart.close
                                { ranges =
                                    testRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.minorSeventh
                                }
                                |> List.length
                    in
                    Expect.equal 36 result
            ]
        , describe "drop2"
            [ test "should generate voicings of the chord" <|
                \_ ->
                    let
                        result =
                            JazzFivePart.drop2
                                { ranges =
                                    testRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.majorSeventh
                                }
                                |> List.length
                    in
                    Expect.equal 88 result
            ]
        , describe "drop3"
            [ test "should generate voicings of the chord" <|
                \_ ->
                    let
                        result =
                            JazzFivePart.drop3
                                { ranges =
                                    testRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.halfDiminished
                                }
                                |> List.length
                    in
                    Expect.equal 52 result
            ]
        , describe "drop2and4"
            [ test "should generate voicings of the chord" <|
                \_ ->
                    let
                        result =
                            JazzFivePart.drop2and4
                                { ranges =
                                    testRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.minorSeventh
                                }
                                |> List.length
                    in
                    Expect.equal 30 result
            ]
        , describe "spread"
            [ test "should generate voicings of the chord" <|
                \_ ->
                    let
                        result =
                            JazzFivePart.spread
                                { ranges =
                                    testRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.minorSeventh
                                }
                                |> List.length
                    in
                    Expect.equal 19 result
            ]
        ]
