module Test.Internal.Voicing.FourPart.Jazz exposing (..)

import Expect
import Music.Chord as Chord
import Music.Internal.Chord as Chord
import Music.Internal.ChordType as ChordType
import Music.Internal.InstrumentRanges as InstrumentRanges
import Music.Internal.PitchClass as PitchClass
import Music.Internal.Voicing.FourPart as FourPart
import Music.Internal.Voicing.FourPart.Jazz as JazzFourPart
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
                            Chord.voiceFourParts
                                satbRanges
                                [ JazzFourPart.close ]
                                (Chord.chord
                                    PitchClass.c
                                    ChordType.minorSeventh
                                )
                                |> List.length
                    in
                    Expect.equal 18 result
            ]
        , describe "drop2"
            [ test "should generate voicings of the chord" <|
                \_ ->
                    let
                        result =
                            Chord.voiceFourParts
                                satbRanges
                                [ JazzFourPart.drop2 ]
                                (Chord.chord
                                    PitchClass.c
                                    ChordType.majorSeventh
                                )
                                |> List.length
                    in
                    Expect.equal 14 result
            ]
        , describe "drop3"
            [ test "should generate voicings of the chord" <|
                \_ ->
                    let
                        result =
                            Chord.voiceFourParts
                                satbRanges
                                [ JazzFourPart.drop3 ]
                                (Chord.chord
                                    PitchClass.c
                                    ChordType.halfDiminished
                                )
                                |> List.length
                    in
                    Expect.equal 34 result
            ]
        , describe "drop2and4"
            [ test "should generate voicings of the chord" <|
                \_ ->
                    let
                        result =
                            Chord.voiceFourParts
                                satbRanges
                                [ JazzFourPart.drop2and4 ]
                                (Chord.chord
                                    PitchClass.c
                                    ChordType.minorSeventh
                                )
                                |> List.length
                    in
                    Expect.equal 29 result
            ]
        , describe "spread"
            [ test "should generate voicings of the chord" <|
                \_ ->
                    let
                        result =
                            Chord.voiceFourParts
                                satbRanges
                                [ JazzFourPart.spread ]
                                (Chord.chord
                                    PitchClass.c
                                    ChordType.minorSeventh
                                )
                                |> List.length
                    in
                    Expect.equal 17 result
            ]
        ]
