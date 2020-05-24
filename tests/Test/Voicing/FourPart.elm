module Test.Voicing.FourPart exposing (..)

import Expect
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.Interval as Interval
import MusicTheory.Pitch as Pitch
import MusicTheory.PitchClass as PitchClass
import MusicTheory.Voicing as Voicing
import MusicTheory.Voicing.FourPart as FourPart
import MusicTheory.Voicing.FourPart.Classical as Classical
import Test exposing (..)


all : Test
all =
    describe "all"
        [ describe "commonTones"
            [ test "should return three common tones between a Cmaj6 and a Cmaj7" <|
                \_ ->
                    let
                        voicingA =
                            Voicing.fourPart
                                Pitch.c4
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.fourPart
                                Pitch.c4
                                { voiceOne = Interval.majorSixth
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            FourPart.commonTones voicingA voicingB

                        expected =
                            3
                    in
                    Expect.equal expected result
            , test "should return no common tones between seventh chords a step apart" <|
                \_ ->
                    let
                        voicingA =
                            Voicing.fourPart
                                Pitch.c4
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.fourPart
                                Pitch.d4
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            FourPart.commonTones voicingA voicingB

                        expected =
                            0
                    in
                    Expect.equal expected result
            , test "should return no common tones between the same chord an octave apart" <|
                \_ ->
                    let
                        voicingA =
                            Voicing.fourPart
                                Pitch.c4
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.fourPart
                                Pitch.c5
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            FourPart.commonTones voicingA voicingB

                        expected =
                            0
                    in
                    Expect.equal expected result
            ]
        , describe "compareByCommonTones"
            [ test "should prioritize voicings with more common tones" <|
                \_ ->
                    let
                        voicingFrom =
                            Voicing.fourPart
                                Pitch.c3
                                { voiceOne = Interval.majorThird |> Interval.addOctave |> Interval.addOctave
                                , voiceTwo = Interval.perfectFifth |> Interval.addOctave
                                , voiceThree = Interval.perfectOctave
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            Classical.rootPosition
                                { ranges =
                                    Classical.satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.g
                                        ChordClass.major
                                }
                                |> List.sortWith (FourPart.compareByCommonTones voicingFrom)
                                |> List.head

                        expected =
                            Voicing.fourPart
                                Pitch.g3
                                { voiceOne = Interval.perfectFifth |> Interval.addOctave
                                , voiceTwo = Interval.perfectOctave
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }
                                |> Just
                    in
                    Expect.equal expected result
            ]
        , describe "containsParallelFifths"
            [ test "should return true for same voicing of chords a step apart" <|
                \_ ->
                    let
                        voicingA =
                            Voicing.fourPart
                                Pitch.c4
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.fourPart
                                Pitch.d4
                                { voiceOne = Interval.minorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.minorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            FourPart.containsParallelFifths voicingA voicingB

                        expected =
                            True
                    in
                    Expect.equal expected result
            , test "should return false for voicings without parallel fifths" <|
                \_ ->
                    let
                        voicingA =
                            Voicing.fourPart
                                Pitch.c4
                                { voiceOne = Interval.majorThird |> Interval.addOctave |> Interval.addOctave
                                , voiceTwo = Interval.perfectFifth |> Interval.addOctave
                                , voiceThree = Interval.perfectOctave
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.fourPart
                                Pitch.g3
                                { voiceOne = Interval.perfectFifth |> Interval.addOctave |> Interval.addOctave
                                , voiceTwo = Interval.perfectOctave |> Interval.addOctave
                                , voiceThree = Interval.majorThird |> Interval.addOctave
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            FourPart.containsParallelFifths voicingA voicingB

                        expected =
                            False
                    in
                    Expect.equal expected result
            , test "should return true for voicings that contain compound fifth intervals" <|
                \_ ->
                    let
                        voicingA =
                            Voicing.fourPart
                                Pitch.c4
                                { voiceOne = Interval.majorThird |> Interval.addOctave |> Interval.addOctave
                                , voiceTwo = Interval.perfectFifth |> Interval.addOctave
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.fourPart
                                Pitch.d4
                                { voiceOne = Interval.majorThird |> Interval.addOctave |> Interval.addOctave
                                , voiceTwo = Interval.perfectFifth |> Interval.addOctave
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            FourPart.containsParallelFifths voicingA voicingB

                        expected =
                            True
                    in
                    Expect.equal expected result
            ]
        ]
