module Generate.Voicing exposing (all)

import Expect
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.Generate.Voicing as GenerateVoicing exposing (VoicingError(..))
import MusicTheory.Interval as Interval
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.Pitch as Pitch
import MusicTheory.PitchClass as PitchClass exposing (natural)
import Test exposing (..)


all : Test
all =
    describe "all"
        [ describe "fourWayClose"
            [ test "All valid voicings for C major 6/9 chord" <|
                \_ ->
                    let
                        cMajorSixNineChord =
                            Chord.chord PitchClass.c ChordClass.majorSixNine

                        expected =
                            Ok 66

                        result =
                            GenerateVoicing.fourWayClose cMajorSixNineChord
                                |> Result.map List.length
                    in
                    Expect.equal expected result
            , test "Should not be able to create voicing plans for chord without all four voice categories" <|
                \_ ->
                    let
                        cMajorChord =
                            Chord.chord (PitchClass.pitchClass C natural) ChordClass.major

                        expected =
                            Err MissingVoiceCategory

                        result =
                            GenerateVoicing.fourWayClose cMajorChord
                    in
                    Expect.equal expected result
            , test "Should not be able to create voicings for non-tertian chords" <|
                \_ ->
                    let
                        nonTertianChordClass =
                            ChordClass.nonTertian
                                [ Interval.perfectUnison
                                , Interval.perfectFifth
                                ]

                        cMajorChord =
                            Chord.chord (PitchClass.pitchClass C natural)
                                nonTertianChordClass

                        expected =
                            Err <|
                                CantVoiceNonTertianChord <|
                                    ChordClass.ChordClassIsNonTertian
                                        nonTertianChordClass

                        result =
                            GenerateVoicing.fourWayClose cMajorChord
                    in
                    Expect.equal expected result
            ]
        , describe "drop2"
            [ test "All valid voicings for C major 6/9 chord" <|
                \_ ->
                    let
                        cMajorSixNineChord =
                            Chord.chord PitchClass.c ChordClass.majorSixNine

                        expected =
                            Ok 62

                        result =
                            GenerateVoicing.drop2 cMajorSixNineChord
                                |> Result.map List.length
                    in
                    Expect.equal expected result
            ]
        , describe "drop2and4"
            [ test "All valid voicings for C major 6/9 chord" <|
                \_ ->
                    let
                        cMajorSixNineChord =
                            Chord.chord PitchClass.c ChordClass.majorSixNine

                        expected =
                            Ok 58

                        result =
                            GenerateVoicing.drop2and4 cMajorSixNineChord
                                |> Result.map List.length
                    in
                    Expect.equal expected result
            ]
        , describe "diffFourParts"
            [ test "First inversion of C major seventh should be 12 semitones different from root position" <|
                \_ ->
                    let
                        cMaj7root =
                            { voiceOne = Pitch.c4
                            , voiceTwo = Pitch.e4
                            , voiceThree = Pitch.g4
                            , voiceFour = Pitch.b4
                            }

                        cMaj7FirstInv =
                            { voiceOne = Pitch.e4
                            , voiceTwo = Pitch.g4
                            , voiceThree = Pitch.b4
                            , voiceFour = Pitch.c5
                            }

                        result =
                            GenerateVoicing.diffFourParts cMaj7root cMaj7FirstInv

                        expected =
                            12
                    in
                    Expect.equal result expected
            , test "C major six should be 2 semitones different from C major seven" <|
                \_ ->
                    let
                        cMaj7 =
                            { voiceOne = Pitch.c4
                            , voiceTwo = Pitch.e4
                            , voiceThree = Pitch.g4
                            , voiceFour = Pitch.b4
                            }

                        cMaj6 =
                            { voiceOne = Pitch.c4
                            , voiceTwo = Pitch.e4
                            , voiceThree = Pitch.g4
                            , voiceFour = Pitch.a4
                            }

                        result =
                            GenerateVoicing.diffFourParts cMaj6 cMaj7

                        expected =
                            2
                    in
                    Expect.equal result expected
            ]
        , describe "containsIntervalFourParts"
            [ test "First inversion of C major seventh should contain a minor second" <|
                \_ ->
                    let
                        cMaj7FirstInv =
                            { voiceOne = Pitch.e4
                            , voiceTwo = Pitch.g4
                            , voiceThree = Pitch.b4
                            , voiceFour = Pitch.c5
                            }

                        result =
                            GenerateVoicing.containsIntervalFourParts Interval.minorSecond cMaj7FirstInv

                        expected =
                            1
                    in
                    Expect.equal result expected
            , test "C diminished seven should contain three minor thirds" <|
                \_ ->
                    let
                        cDim7 =
                            { voiceOne = Pitch.c4
                            , voiceTwo = Pitch.eFlat4
                            , voiceThree = Pitch.gFlat4
                            , voiceFour = Pitch.bDoubleFlat4
                            }

                        result =
                            GenerateVoicing.containsIntervalFourParts Interval.minorThird cDim7

                        expected =
                            3
                    in
                    Expect.equal result expected
            ]
        , describe "passesMinorNinthRule"
            [ test "Open voicing of C major seventh should not pass the minor ninth rule" <|
                \_ ->
                    let
                        cMaj7FirstInv =
                            { voiceOne = Pitch.b3
                            , voiceTwo = Pitch.e4
                            , voiceThree = Pitch.g4
                            , voiceFour = Pitch.c5
                            }

                        result =
                            GenerateVoicing.passesMinorNinthRule cMaj7FirstInv

                        expected =
                            False
                    in
                    Expect.equal result expected
            , test "Close voicing of C major seventh should pass the minor ninth rule" <|
                \_ ->
                    let
                        cMaj7Close =
                            { voiceOne = Pitch.c4
                            , voiceTwo = Pitch.e4
                            , voiceThree = Pitch.g4
                            , voiceFour = Pitch.b4
                            }

                        result =
                            GenerateVoicing.passesMinorNinthRule cMaj7Close

                        expected =
                            True
                    in
                    Expect.equal result expected
            ]
        , describe "semitoneDistancesContainedFourParts"
            [ test "Semitone distances contained in a voicing of C major seventh" <|
                \_ ->
                    let
                        cMaj7Open =
                            { voiceOne = Pitch.b3
                            , voiceTwo = Pitch.e4
                            , voiceThree = Pitch.g4
                            , voiceFour = Pitch.c5
                            }

                        result =
                            GenerateVoicing.semitoneDistancesContainedFourParts
                                cMaj7Open

                        expected =
                            [ 5, 8, 13, 3, 8, 5 ]
                    in
                    Expect.equal result expected
            ]
        ]
