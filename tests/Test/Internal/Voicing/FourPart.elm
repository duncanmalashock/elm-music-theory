module Test.Internal.Voicing.FourPart exposing (..)

import Expect
import Music.Internal.Chord as Chord
import Music.Internal.ChordType as ChordType
import Music.Internal.InstrumentRanges as InstrumentRanges
import Music.Internal.Interval as Interval
import Music.Internal.Octave as Octave
import Music.Internal.Pitch as Pitch
import Music.Internal.PitchClass as PitchClass
import Music.Internal.Voicing as Voicing
import Music.Internal.Voicing.FourPart as FourPart
import Music.Internal.Voicing.FourPart.Classical as Classical
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
                                (Chord.chord PitchClass.c ChordType.majorSeventh)
                                Octave.four
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordType.majorSix)
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
                                (Chord.chord PitchClass.c ChordType.majorSeventh)
                                Octave.four
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.d ChordType.majorSeventh)
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
                                (Chord.chord PitchClass.c ChordType.majorSeventh)
                                Octave.four
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordType.majorSeventh)
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
                                (Chord.chord PitchClass.c ChordType.major)
                                Octave.three
                                { voiceOne = Interval.majorTenth |> Interval.addOctave
                                , voiceTwo = Interval.perfectTwelfth
                                , voiceThree = Interval.perfectOctave
                                , voiceFour = Interval.perfectFifth
                                }

                        result =
                            Classical.rootPosition
                                { ranges =
                                    satbRanges
                                , chord =
                                    Chord.chord
                                        PitchClass.g
                                        ChordType.major
                                }
                                |> List.sortWith (Voicing.compareByCommonTones FourPart.allVoices voicingFrom)
                                |> List.head
                                |> Maybe.map (Voicing.toString FourPart.allVoices)

                        expected =
                            Just "B4 G4 D4 G3"
                    in
                    Expect.equal expected result
            ]
        , describe "containsParallelFifths"
            [ test "should return true for same voicing of chords a step apart" <|
                \_ ->
                    let
                        voicingA =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordType.major)
                                Octave.four
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.d ChordType.major)
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
                                (Chord.chord PitchClass.c ChordType.major)
                                Octave.four
                                { voiceOne = Interval.majorTenth |> Interval.addOctave
                                , voiceTwo = Interval.perfectTwelfth
                                , voiceThree = Interval.perfectOctave
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.g ChordType.major)
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
                                (Chord.chord PitchClass.c ChordType.major)
                                Octave.four
                                { voiceOne = Interval.majorTenth |> Interval.addOctave
                                , voiceTwo = Interval.perfectTwelfth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.d ChordType.major)
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
                                (Chord.chord PitchClass.c ChordType.major)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.d ChordType.major)
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
                                (Chord.chord PitchClass.c ChordType.major)
                                Octave.four
                                { voiceOne = Interval.majorSeventh
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordType.minor)
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
                                (Chord.chord PitchClass.c ChordType.major)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordType.major)
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
                                (Chord.chord PitchClass.c ChordType.major)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordType.majorSeventh)
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
                                (Chord.chord PitchClass.c ChordType.majorSix)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.majorSixth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordType.majorAddNine)
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
                                (Chord.chord PitchClass.c ChordType.major)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.g ChordType.major)
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
                                (Chord.chord PitchClass.c ChordType.major)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.c ChordType.major)
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
                                (Chord.chord PitchClass.c ChordType.major)
                                Octave.four
                                { voiceOne = Interval.perfectOctave
                                , voiceTwo = Interval.perfectFifth
                                , voiceThree = Interval.majorThird
                                , voiceFour = Interval.perfectUnison
                                }

                        voicingB =
                            Voicing.voicing
                                (Chord.chord PitchClass.d ChordType.major)
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
                                (Chord.chord PitchClass.d ChordType.dominantSeventh)
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
                                (Chord.chord PitchClass.c ChordType.dominantSeventh)
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
        , describe "adjustToBeAboveMinimum"
            [ test "places upper voice above the lower voice plus the minimum interval" <|
                \_ ->
                    let
                        result : Interval.Interval
                        result =
                            Interval.majorNinth

                        expected : Interval.Interval
                        expected =
                            FourPart.adjustToBeAboveMinimum
                                { minimum = Interval.perfectUnison
                                , lower = Interval.majorThird
                                , upper = Interval.majorSecond
                                }
                    in
                    Expect.equal expected result
            , test "places the same interval in unison when the the minimum interval is unison" <|
                \_ ->
                    let
                        result : Interval.Interval
                        result =
                            Interval.majorSecond

                        expected : Interval.Interval
                        expected =
                            FourPart.adjustToBeAboveMinimum
                                { minimum = Interval.perfectUnison
                                , lower = Interval.majorSecond
                                , upper = Interval.majorSecond
                                }
                    in
                    Expect.equal expected result
            , test "places the same interval an octave apart when the the minimum interval is larger than unison" <|
                \_ ->
                    let
                        result : Interval.Interval
                        result =
                            Interval.majorNinth

                        expected : Interval.Interval
                        expected =
                            FourPart.adjustToBeAboveMinimum
                                { minimum = Interval.augmentedUnison
                                , lower = Interval.majorSecond
                                , upper = Interval.majorSecond
                                }
                    in
                    Expect.equal expected result
            ]
        , describe "placeFactors"
            [ test "places chord factors in ascending order" <|
                \_ ->
                    let
                        result : List String
                        result =
                            [ "C6 E5 G4 B3"
                            ]

                        expected : List String
                        expected =
                            { voiceOne = Interval.perfectUnison
                            , voiceTwo = Interval.majorThird
                            , voiceThree = Interval.perfectFifth
                            , voiceFour = Interval.majorSeventh
                            }
                                |> FourPart.placeFactors
                                    { twoToOne =
                                        { min = Interval.augmentedUnison
                                        , max = Interval.perfectOctave
                                        }
                                    , threeToTwo =
                                        { min = Interval.augmentedUnison
                                        , max = Interval.perfectOctave
                                        }
                                    , fourToThree =
                                        { min = Interval.augmentedUnison
                                        , max = Interval.perfectOctave
                                        }
                                    }
                                |> List.map
                                    (Voicing.voicing
                                        (Chord.chord
                                            PitchClass.c
                                            ChordType.majorSixNine
                                        )
                                        Octave.three
                                    )
                                |> List.map (Voicing.toString FourPart.allVoices)
                    in
                    Expect.equal expected result
            , test "lists multiple options when they are within interval limits" <|
                \_ ->
                    let
                        result : List String
                        result =
                            [ "C3 C3 C3 C3"
                            , "C4 C3 C3 C3"
                            , "C4 C4 C3 C3"
                            , "C5 C4 C3 C3"
                            , "C4 C4 C4 C3"
                            , "C5 C4 C4 C3"
                            , "C5 C5 C4 C3"
                            , "C6 C5 C4 C3"
                            ]

                        expected : List String
                        expected =
                            { voiceOne = Interval.perfectUnison
                            , voiceTwo = Interval.perfectUnison
                            , voiceThree = Interval.perfectUnison
                            , voiceFour = Interval.perfectUnison
                            }
                                |> FourPart.placeFactors
                                    { twoToOne =
                                        { min = Interval.perfectUnison
                                        , max = Interval.perfectOctave
                                        }
                                    , threeToTwo =
                                        { min = Interval.perfectUnison
                                        , max = Interval.perfectOctave
                                        }
                                    , fourToThree =
                                        { min = Interval.perfectUnison
                                        , max = Interval.perfectOctave
                                        }
                                    }
                                |> List.map
                                    (Voicing.voicing
                                        (Chord.chord
                                            PitchClass.c
                                            ChordType.majorSixNine
                                        )
                                        Octave.three
                                    )
                                |> List.map (Voicing.toString FourPart.allVoices)
                    in
                    Expect.equal expected result
            ]
        ]
