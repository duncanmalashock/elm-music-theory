module Test.Voicing exposing (..)

import Expect
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.InstrumentRanges as InstrumentRanges
import MusicTheory.Interval as Interval
import MusicTheory.PitchClass as PitchClass
import MusicTheory.Voicing as Voicing
import MusicTheory.Voicing.FourPart as FourPart
import MusicTheory.Voicing.FourPart.Classical
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Voicing tests"
        [ describe "minimumRange"
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
                                    [ MusicTheory.Voicing.FourPart.Classical.rootPosition
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
                                    [ MusicTheory.Voicing.FourPart.Classical.rootPosition
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
