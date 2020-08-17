module Test.Internal.Voicing exposing (..)

import Expect
import MusicTheory.Internal.Chord as Chord
import MusicTheory.Internal.ChordClass as ChordClass
import MusicTheory.Internal.InstrumentRanges as InstrumentRanges
import MusicTheory.Internal.Interval as Interval
import MusicTheory.Internal.Pitch as Pitch
import MusicTheory.Internal.PitchClass as PitchClass
import MusicTheory.Internal.Voicing as Voicing
import MusicTheory.Internal.Voicing.FourPart as FourPart
import MusicTheory.Internal.Voicing.FourPart.Classical
import MusicTheory.Internal.Voicing.FourPart.Jazz
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
            , test "voicings that do not violate low interval limits" <|
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
                                    [ MusicTheory.Internal.Voicing.FourPart.Jazz.spread
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
                                        ChordClass.dominantSeventh
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
                                    [ MusicTheory.Internal.Voicing.FourPart.Classical.rootPosition
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
                                        ChordClass.dominantSeventh
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
                                    [ MusicTheory.Internal.Voicing.FourPart.Classical.rootPosition
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
                                        ChordClass.dominantSeventh
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