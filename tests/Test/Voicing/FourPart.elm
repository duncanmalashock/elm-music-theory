module Test.Voicing.FourPart exposing (..)

import Expect
import MusicTheory.Analyze.JazzChord as AnalyzeChord
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.Generate.JazzVoicing as GenerateVoicing
import MusicTheory.InstrumentRanges as InstrumentRanges
import MusicTheory.Interval as Interval
import MusicTheory.Octave as Octave
import MusicTheory.Pitch as Pitch
import MusicTheory.PitchClass as PitchClass
import MusicTheory.Voicing as Voicing
import MusicTheory.Voicing.FourPart as FourPart
import MusicTheory.Voicing.FourPart.Classical as Classical
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
        [ describe "commonTones"
            [ test "should return three common tones between a Cmaj6 and a Cmaj7" <|
                \_ ->
                    let
                        voicingA =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordClass.majorSeventh)
                                Octave.four
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordClass.majorSix)
                                Octave.four
                                { voiceOne = Interval.majorSixth
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            Voicing.commonTones FourPart.allVoices voicingA voicingB

                        expected =
                            3
                    in
                    Expect.equal expected result
            , test "should return no common tones between seventh chords a step apart" <|
                \_ ->
                    let
                        voicingA =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordClass.majorSeventh)
                                Octave.four
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.d ChordClass.majorSeventh)
                                Octave.four
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            Voicing.commonTones FourPart.allVoices voicingA voicingB

                        expected =
                            0
                    in
                    Expect.equal expected result
            , test "should return no common tones between the same chord an octave apart" <|
                \_ ->
                    let
                        voicingA =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordClass.majorSeventh)
                                Octave.four
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordClass.majorSeventh)
                                Octave.five
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            Voicing.commonTones FourPart.allVoices voicingA voicingB

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
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordClass.major)
                                Octave.three
                                { voiceOne = Interval.majorTenth |> Interval.addOctave
                                , voiceTwo = Interval.perfectTwelfth
                                , voiceThree = Interval.perfectOctave
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            Classical.rootPosition
                                { ranges =
                                    satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.g
                                        ChordClass.major
                                }
                                |> List.sortWith (Voicing.compareByCommonTones FourPart.allVoices voicingFrom)
                                |> List.head

                        expected =
                            Voicing.voicing
                                (Chord.chord PitchClass.g ChordClass.major)
                                Octave.three
                                { voiceOne = Interval.perfectTwelfth
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
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.d ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            Voicing.containsParallelFifths Voicing.root FourPart.allFactors voicingA voicingB

                        expected =
                            True
                    in
                    Expect.equal expected result
            , test "should return false for voicings without parallel fifths" <|
                \_ ->
                    let
                        voicingA =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.majorTenth |> Interval.addOctave
                                , voiceTwo = Interval.perfectTwelfth
                                , voiceThree = Interval.perfectOctave
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.g ChordClass.major)
                                Octave.three
                                { voiceOne = Interval.perfectTwelfth |> Interval.addOctave
                                , voiceTwo = Interval.perfectOctave |> Interval.addOctave
                                , voiceThree = Interval.majorThird |> Interval.addOctave
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            Voicing.containsParallelFifths Voicing.root FourPart.allFactors voicingA voicingB

                        expected =
                            False
                    in
                    Expect.equal expected result
            , test "should return true for voicings that contain compound fifth intervals" <|
                \_ ->
                    let
                        voicingA =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.majorTenth |> Interval.addOctave
                                , voiceTwo = Interval.perfectTwelfth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.d ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.majorTenth |> Interval.addOctave
                                , voiceTwo = Interval.perfectTwelfth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            Voicing.containsParallelFifths Voicing.root FourPart.allFactors voicingA voicingB

                        expected =
                            True
                    in
                    Expect.equal expected result
            ]
        , describe "containsParallelOctaves"
            [ test "should return true for voicing of chords (with octaves) a step apart" <|
                \_ ->
                    let
                        voicingA =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.d ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.minorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            Voicing.containsParallelOctaves Voicing.root FourPart.allFactors voicingA voicingB

                        expected =
                            True
                    in
                    Expect.equal expected result
            , test "should return false for voicing of chords (without octaves) a step apart" <|
                \_ ->
                    let
                        voicingA =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordClass.minor)
                                Octave.four
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.minorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            Voicing.containsParallelOctaves Voicing.root FourPart.allFactors voicingA voicingB

                        expected =
                            False
                    in
                    Expect.equal expected result
            ]
        , describe "totalSemitoneDistance"
            [ test "should return 0 for identical voicings" <|
                \_ ->
                    let
                        voicingA =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            Voicing.totalSemitoneDistance FourPart.allVoices voicingA voicingB

                        expected =
                            0
                    in
                    Expect.equal expected result
            , test "should return 1 for a voice changed by a semitone" <|
                \_ ->
                    let
                        voicingA =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordClass.majorSeventh)
                                Octave.four
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            Voicing.totalSemitoneDistance FourPart.allVoices voicingA voicingB

                        expected =
                            1
                    in
                    Expect.equal expected result
            , test "should return 4 for voices changed by 2 semitones in different directions" <|
                \_ ->
                    let
                        voicingA =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordClass.majorSix)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.majorSixth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordClass.majorAddNine)
                                Octave.four
                                { voiceOne = Interval.majorNinth
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            Voicing.totalSemitoneDistance FourPart.allVoices voicingA voicingB

                        expected =
                            4
                    in
                    Expect.equal expected result
            ]
        , describe "usesContraryMotion"
            [ test "should return true when voices three and four move in different directions" <|
                \_ ->
                    let
                        voicingA =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.g ChordClass.major)
                                Octave.three
                                { voiceOne = Interval.majorTenth
                                , voiceTwo = Interval.perfectTwelfth
                                , voiceThree = Interval.perfectOctave
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            Voicing.usesContraryMotion FourPart.getVoiceFour FourPart.getVoiceThree voicingA voicingB

                        expected =
                            True
                    in
                    Expect.equal expected result
            , test "should return false when voices three and four move in oblique motion" <|
                \_ ->
                    let
                        voicingA =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.majorTenth
                                , voiceTwo = Interval.perfectOctave
                                , voiceThree = Interval.perfectFifth
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            Voicing.usesContraryMotion FourPart.getVoiceFour FourPart.getVoiceThree voicingA voicingB

                        expected =
                            False
                    in
                    Expect.equal expected result
            , test "should return false when voices three and four move in parallel motion" <|
                \_ ->
                    let
                        voicingA =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.d ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            Voicing.usesContraryMotion FourPart.getVoiceFour FourPart.getVoiceThree voicingA voicingB

                        expected =
                            False
                    in
                    Expect.equal expected result
            ]
        , describe "containsPitch"
            [ test "voicing of D7 should contain F#5" <|
                \_ ->
                    let
                        voicing =
                            Voicing.voicing
                                (Chord.chord PitchClass.d ChordClass.dominantSeventh)
                                Octave.three
                                { voiceOne = Interval.majorTenth |> Interval.addOctave
                                , voiceTwo = Interval.perfectTwelfth
                                , voiceThree = Interval.minorSeventh
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            Voicing.containsPitch Pitch.fSharp5 FourPart.allVoices voicing

                        expected =
                            True
                    in
                    Expect.equal expected result
            , test "voicing of C7 should not contain F#5" <|
                \_ ->
                    let
                        voicing =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordClass.dominantSeventh)
                                Octave.three
                                { voiceOne = Interval.majorTenth |> Interval.addOctave
                                , voiceTwo = Interval.perfectTwelfth
                                , voiceThree = Interval.minorSeventh
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            Voicing.containsPitch Pitch.fSharp5 FourPart.allVoices voicing

                        expected =
                            False
                    in
                    Expect.equal expected result
            ]
        , describe "allIntervalsFourPart"
            [ test "correct intervals for four-way close voicing of Cmaj6" <|
                \_ ->
                    let
                        availables =
                            AnalyzeChord.availables
                                (Chord.chord PitchClass.c ChordClass.majorSix)
                                |> Result.mapError GenerateVoicing.AnalyzeChordError

                        result =
                            availables
                                |> Result.andThen
                                    (GenerateVoicing.fourWayClose Interval.majorThird)
                                |> Result.map FourPart.allIntervals

                        expected =
                            { fourToOne = Interval.majorSixth
                            , fourToTwo = Interval.perfectFourth
                            , threeToOne = Interval.perfectFifth
                            , fourToThree = Interval.majorSecond
                            , threeToTwo = Interval.minorThird
                            , twoToOne = Interval.majorThird
                            }
                                |> Ok
                    in
                    Expect.equal expected result
            ]
        ]
