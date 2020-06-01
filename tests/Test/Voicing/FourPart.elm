module Test.Voicing.FourPart exposing (..)

import Expect
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.Interval as Interval
import MusicTheory.Octave as Octave
import MusicTheory.Pitch as Pitch
import MusicTheory.PitchClass as PitchClass
import MusicTheory.Voicing as Voicing
import MusicTheory.Voicing.FourPart as FourPart
import MusicTheory.Voicing.FourPart.Classical as Classical
import Test exposing (..)
import Test.Util


all : Test
all =
    describe "all"
        [ describe "commonTones"
            [ test "should return three common tones between a Cmaj6 and a Cmaj7" <|
                \_ ->
                    let
                        voicingA =
                            FourPart.voicing
                                (Chord.chord PitchClass.c ChordClass.majorSeventh)
                                Octave.four
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            FourPart.voicing
                                (Chord.chord PitchClass.c ChordClass.majorSix)
                                Octave.four
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
                            FourPart.voicing
                                (Chord.chord PitchClass.c ChordClass.majorSeventh)
                                Octave.four
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            FourPart.voicing
                                (Chord.chord PitchClass.d ChordClass.majorSeventh)
                                Octave.four
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
                            FourPart.voicing
                                (Chord.chord PitchClass.c ChordClass.majorSeventh)
                                Octave.four
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            FourPart.voicing
                                (Chord.chord PitchClass.c ChordClass.majorSeventh)
                                Octave.five
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
                            FourPart.voicing
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
                                    Classical.satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.g
                                        ChordClass.major
                                }
                                |> List.sortWith (FourPart.compareByCommonTones voicingFrom)
                                |> List.head

                        expected =
                            FourPart.voicing
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
                            FourPart.voicing
                                (Chord.chord PitchClass.c ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            FourPart.voicing
                                (Chord.chord PitchClass.d ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
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
                            FourPart.voicing
                                (Chord.chord PitchClass.c ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.majorTenth |> Interval.addOctave
                                , voiceTwo = Interval.perfectTwelfth
                                , voiceThree = Interval.perfectOctave
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            FourPart.voicing
                                (Chord.chord PitchClass.g ChordClass.major)
                                Octave.three
                                { voiceOne = Interval.perfectTwelfth |> Interval.addOctave
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
                            FourPart.voicing
                                (Chord.chord PitchClass.c ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.majorTenth |> Interval.addOctave
                                , voiceTwo = Interval.perfectTwelfth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            FourPart.voicing
                                (Chord.chord PitchClass.d ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.majorTenth |> Interval.addOctave
                                , voiceTwo = Interval.perfectTwelfth
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
        , describe "containsParallelOctaves"
            [ test "should return true for voicing of chords (with octaves) a step apart" <|
                \_ ->
                    let
                        voicingA =
                            FourPart.voicing
                                (Chord.chord PitchClass.c ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            FourPart.voicing
                                (Chord.chord PitchClass.d ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.minorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            FourPart.containsParallelOctaves voicingA voicingB

                        expected =
                            True
                    in
                    Expect.equal expected result
            , test "should return false for voicing of chords (without octaves) a step apart" <|
                \_ ->
                    let
                        voicingA =
                            FourPart.voicing
                                (Chord.chord PitchClass.c ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            FourPart.voicing
                                (Chord.chord PitchClass.c ChordClass.minor)
                                Octave.four
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.minorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            FourPart.containsParallelOctaves voicingA voicingB

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
                            FourPart.voicing
                                (Chord.chord PitchClass.c ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            FourPart.voicing
                                (Chord.chord PitchClass.c ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            FourPart.totalSemitoneDistance voicingA voicingB

                        expected =
                            0
                    in
                    Expect.equal expected result
            , test "should return 1 for a voice changed by a semitone" <|
                \_ ->
                    let
                        voicingA =
                            FourPart.voicing
                                (Chord.chord PitchClass.c ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            FourPart.voicing
                                (Chord.chord PitchClass.c ChordClass.majorSeventh)
                                Octave.four
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            FourPart.totalSemitoneDistance voicingA voicingB

                        expected =
                            1
                    in
                    Expect.equal expected result
            , test "should return 4 for voices changed by 2 semitones in different directions" <|
                \_ ->
                    let
                        voicingA =
                            FourPart.voicing
                                (Chord.chord PitchClass.c ChordClass.majorSix)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.majorSixth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            FourPart.voicing
                                (Chord.chord PitchClass.c ChordClass.majorAddNine)
                                Octave.four
                                { voiceOne = Interval.majorNinth
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            FourPart.totalSemitoneDistance voicingA voicingB

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
                            FourPart.voicing
                                (Chord.chord PitchClass.c ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            FourPart.voicing
                                (Chord.chord PitchClass.g ChordClass.major)
                                Octave.three
                                { voiceOne = Interval.majorTenth
                                , voiceTwo = Interval.perfectTwelfth
                                , voiceThree = Interval.perfectOctave
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            FourPart.usesContraryMotion voicingA voicingB

                        expected =
                            True
                    in
                    Expect.equal expected result
            , test "should return false when voices three and four move in oblique motion" <|
                \_ ->
                    let
                        voicingA =
                            FourPart.voicing
                                (Chord.chord PitchClass.c ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            FourPart.voicing
                                (Chord.chord PitchClass.c ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.majorTenth
                                , voiceTwo = Interval.perfectOctave
                                , voiceThree = Interval.perfectFifth
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            FourPart.usesContraryMotion voicingA voicingB

                        expected =
                            False
                    in
                    Expect.equal expected result
            , test "should return false when voices three and four move in parallel motion" <|
                \_ ->
                    let
                        voicingA =
                            FourPart.voicing
                                (Chord.chord PitchClass.c ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            FourPart.voicing
                                (Chord.chord PitchClass.d ChordClass.major)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            FourPart.usesContraryMotion voicingA voicingB

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
                            FourPart.voicing
                                (Chord.chord PitchClass.d ChordClass.dominantSeventh)
                                Octave.three
                                { voiceOne = Interval.majorTenth |> Interval.addOctave
                                , voiceTwo = Interval.perfectTwelfth
                                , voiceThree = Interval.minorSeventh
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            FourPart.containsPitch Pitch.fSharp5 voicing

                        expected =
                            True
                    in
                    Expect.equal expected result
            , test "voicing of C7 should not contain F#5" <|
                \_ ->
                    let
                        voicing =
                            FourPart.voicing
                                (Chord.chord PitchClass.c ChordClass.dominantSeventh)
                                Octave.three
                                { voiceOne = Interval.majorTenth |> Interval.addOctave
                                , voiceTwo = Interval.perfectTwelfth
                                , voiceThree = Interval.minorSeventh
                                , voiceFour = Interval.perfectUnison
                                }

                        result =
                            FourPart.containsPitch Pitch.fSharp5 voicing

                        expected =
                            False
                    in
                    Expect.equal expected result
            ]

        --, skip <|
        --    describe "execute" <|
        --        [ describe "using Classical.optimizeVoiceLeading"
        --            [ test "just seeing what this ordering is like" <|
        --                \_ ->
        --                     let
        --                        fromVoicing =
        --                            FourPart.fourPart
        --                                Pitch.c3
        --                                { voiceOne = Interval.majorTenth |> Interval.addOctave
        --                                , voiceTwo = Interval.perfectTwelfth
        --                                , voiceThree = Interval.perfectOctave
        --                                , voiceFour = Interval.perfectUnison
        --                                }
        --
        --                        result =
        --                            FourPart.config
        --                                { ranges = Classical.satbRanges
        --                                , techniques =
        --                                    [ Classical.rootPosition
        --                                    , Classical.firstInversion
        --                                    , Classical.secondInversion
        --                                    , Classical.thirdInversion
        --                                    ]
        --                                }
        --                                |> Classical.optimizeVoiceLeading fromVoicing
        --                                |> FourPart.execute (Chord.chord PitchClass.g ChordClass.major)
        --                                |> List.map
        --                                    (\v ->
        --                                        { voicing =
        --                                            Test.Util.voicingToString v
        --                                        , semitoneDistance = FourPart.totalSemitoneDistance fromVoicing v
        --                                        , commonTones = FourPart.commonTones fromVoicing v
        --                                        , useContraryMotion = FourPart.usesContraryMotion fromVoicing v
        --                                        }
        --                                    )
        --
        --                        expected =
        --                            []
        --                    in
        --                    Expect.equal expected result
        --            ]
        --        ]
        ]
