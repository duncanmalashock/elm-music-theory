module Test.Internal.Voicing exposing (..)

import Expect
import Music.Internal.Chord as Chord
import Music.Internal.ChordType as ChordType
import Music.Internal.InstrumentRanges as InstrumentRanges
import Music.Internal.Interval as Interval
import Music.Internal.Pitch as Pitch
import Music.Internal.PitchClass as PitchClass
import Music.Internal.Voicing as Voicing
import Music.Internal.Voicing.FourPart as FourPart
import Music.Internal.Voicing.FourPart.Classical
import Music.Internal.Voicing.FourPart.Jazz
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Voicing tests"
        [ describe "violatesLowIntervalLimits"
            [ test "interval below limits should return True" <|
                \_ ->
                    let
                        result =
                            Voicing.violatesLowIntervalLimits Interval.minorSeventh Pitch.c2

                        expected =
                            True
                    in
                    Expect.equal expected result
            , test "voicing that violates low interval limits" <|
                \_ ->
                    let
                        voicing =
                            Voicing.config
                                { ranges =
                                    { voiceOne = InstrumentRanges.cello
                                    , voiceTwo = InstrumentRanges.cello
                                    , voiceThree = InstrumentRanges.cello
                                    , voiceFour = InstrumentRanges.cello
                                    }
                                , techniques =
                                    [ Music.Internal.Voicing.FourPart.Jazz.spread
                                    ]
                                }
                                |> Voicing.withMinimumRange
                                    (Interval.perfectOctave
                                        |> Interval.addOctave
                                    )
                                    { getTopVoice = FourPart.getVoiceOne
                                    , getBottomVoice = FourPart.getVoiceFour
                                    }
                                |> Voicing.execute FourPart.allVoices
                                    (Chord.chord
                                        PitchClass.c
                                        ChordType.dominantSeventh
                                    )
                                |> List.head

                        expected =
                            Just True

                        result =
                            Maybe.map
                                FourPart.violatesLowIntervalLimits
                                voicing
                    in
                    Expect.equal expected result
            , test "voicing that does not violate low interval limits" <|
                \_ ->
                    let
                        voicing =
                            Voicing.config
                                { ranges =
                                    { voiceOne = InstrumentRanges.violin
                                    , voiceTwo = InstrumentRanges.violin
                                    , voiceThree = InstrumentRanges.viola
                                    , voiceFour = InstrumentRanges.cello
                                    }
                                , techniques =
                                    [ Music.Internal.Voicing.FourPart.Jazz.close
                                    ]
                                }
                                |> Voicing.execute FourPart.allVoices
                                    (Chord.chord
                                        PitchClass.c
                                        ChordType.dominantSeventh
                                    )
                                |> List.head

                        expected =
                            Just False

                        result =
                            Maybe.map
                                FourPart.violatesLowIntervalLimits
                                voicing
                    in
                    Expect.equal expected result
            ]
        , describe "minimumRange"
            [ test "should return no voicings with an interval of less than 2 octaves from lowest to highest voice" <|
                \_ ->
                    let
                        returnsOnlyVoicingsSpanningAtLeastTwoOctaves : Bool
                        returnsOnlyVoicingsSpanningAtLeastTwoOctaves =
                            Voicing.config
                                { ranges =
                                    { voiceOne = InstrumentRanges.violin
                                    , voiceTwo = InstrumentRanges.violin
                                    , voiceThree = InstrumentRanges.viola
                                    , voiceFour = InstrumentRanges.cello
                                    }
                                , techniques =
                                    [ Music.Internal.Voicing.FourPart.Classical.rootPosition
                                    ]
                                }
                                |> Voicing.withMinimumRange
                                    (Interval.perfectOctave
                                        |> Interval.addOctave
                                    )
                                    { getTopVoice = FourPart.getVoiceOne
                                    , getBottomVoice = FourPart.getVoiceFour
                                    }
                                |> Voicing.execute FourPart.allVoices
                                    (Chord.chord
                                        PitchClass.c
                                        ChordType.dominantSeventh
                                    )
                                |> List.all
                                    (\voicing ->
                                        Interval.isGreaterThanOrEqualTo
                                            (Interval.perfectOctave
                                                |> Interval.addOctave
                                            )
                                            (Voicing.range
                                                { getTopVoice = FourPart.getVoiceOne
                                                , getBottomVoice = FourPart.getVoiceFour
                                                }
                                                voicing
                                            )
                                    )
                    in
                    Expect.true
                        "One of the voicings returned has too small a range"
                        returnsOnlyVoicingsSpanningAtLeastTwoOctaves
            ]
        , describe "maximumRange"
            [ test "should return no voicings with an interval of greater than 2 octaves from lowest to highest voice" <|
                \_ ->
                    let
                        returnsOnlyVoicingsSpanningAtLeastTwoOctaves : Bool
                        returnsOnlyVoicingsSpanningAtLeastTwoOctaves =
                            Voicing.config
                                { ranges =
                                    { voiceOne = InstrumentRanges.violin
                                    , voiceTwo = InstrumentRanges.violin
                                    , voiceThree = InstrumentRanges.viola
                                    , voiceFour = InstrumentRanges.cello
                                    }
                                , techniques =
                                    [ Music.Internal.Voicing.FourPart.Classical.rootPosition
                                    ]
                                }
                                |> Voicing.withMaximumRange
                                    (Interval.perfectOctave
                                        |> Interval.addOctave
                                    )
                                    { getTopVoice = FourPart.getVoiceOne
                                    , getBottomVoice = FourPart.getVoiceFour
                                    }
                                |> Voicing.execute FourPart.allVoices
                                    (Chord.chord
                                        PitchClass.c
                                        ChordType.dominantSeventh
                                    )
                                |> List.all
                                    (\voicing ->
                                        Interval.isLessThanOrEqualTo
                                            (Interval.perfectOctave
                                                |> Interval.addOctave
                                            )
                                            (Voicing.range
                                                { getTopVoice = FourPart.getVoiceOne
                                                , getBottomVoice = FourPart.getVoiceFour
                                                }
                                                voicing
                                            )
                                    )
                    in
                    Expect.true
                        "One of the voicings returned has too large a range"
                        returnsOnlyVoicingsSpanningAtLeastTwoOctaves
            ]
        ]
