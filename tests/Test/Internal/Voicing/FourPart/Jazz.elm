module Test.Internal.Voicing.FourPart.Jazz exposing (..)

import Expect
import MusicTheory.Internal.Chord as Chord
import MusicTheory.Internal.ChordClass as ChordClass
import MusicTheory.Internal.InstrumentRanges as InstrumentRanges
import MusicTheory.Internal.PitchClass as PitchClass
import MusicTheory.Internal.Voicing.FourPart as FourPart
import MusicTheory.Internal.Voicing.FourPart.Jazz as JazzFourPart
import Test exposing (..)


satbRanges : FourPart.Ranges
satbRanges =
    { voiceOne = InstrumentRanges.sopranoVoice
    , voiceTwo = InstrumentRanges.altoVoice
    , voiceThree = InstrumentRanges.tenorVoice
    , voiceFour = InstrumentRanges.bassVoice
    }


all : Test
all =
    describe "all"
        [ describe "close"
            [ test "should generate voicings of the chord" <|
                \_ ->
                    let
                        result =
                            JazzFourPart.close
                                { ranges =
                                    satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.minorSeventh
                                }
                                |> List.length
                    in
                    Expect.equal 20 result
            ]
        , describe "drop2"
            [ test "should generate voicings of the chord" <|
                \_ ->
                    let
                        result =
                            JazzFourPart.drop2
                                { ranges =
                                    satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.majorSeventh
                                }
                                |> List.length
                    in
                    Expect.equal 56 result
            ]
        , describe "drop3"
            [ test "should generate voicings of the chord" <|
                \_ ->
                    let
                        result =
                            JazzFourPart.drop3
                                { ranges =
                                    satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.halfDiminished
                                }
                                |> List.length
                    in
                    Expect.equal 38 result
            ]
        , describe "drop2and4"
            [ test "should generate voicings of the chord" <|
                \_ ->
                    let
                        result =
                            JazzFourPart.drop2and4
                                { ranges =
                                    satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.minorSeventh
                                }
                                |> List.length
                    in
                    Expect.equal 29 result
            ]
        , describe "spread"
            [ test "should generate voicings of the chord" <|
                \_ ->
                    let
                        result =
                            JazzFourPart.spread
                                { ranges =
                                    satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.c
                                        ChordClass.minorSeventh
                                }
                                |> List.length
                    in
                    Expect.equal 17 result
            ]
        ]
