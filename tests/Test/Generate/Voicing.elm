module Test.Generate.Voicing exposing (all)

import Expect
import MusicTheory.Analyze.Chord as AnalyzeChord
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.Generate.Voicing as GenerateVoicing
import MusicTheory.Pitch as Pitch
import MusicTheory.PitchClass as PitchClass
import Test exposing (..)


all : Test
all =
    describe "all"
        [ describe "fourWayClose"
            [ test "four-way close voicing of Cmaj6 should include correct chord tones" <|
                \_ ->
                    let
                        expected =
                            Ok
                                { voiceOne = Pitch.e5
                                , voiceTwo = Pitch.c5
                                , voiceThree = Pitch.a4
                                , voiceFour = Pitch.g4
                                }

                        availablePitchClasses =
                            AnalyzeChord.availablePitchClassesFor
                                (Chord.chord PitchClass.c ChordClass.majorSix)
                                |> Result.mapError GenerateVoicing.AnalyzeChordError

                        result =
                            Result.andThen
                                (GenerateVoicing.fourWayClose Pitch.e5)
                                availablePitchClasses
                    in
                    Expect.equal result expected
            ]
        , describe "drop2"
            [ test "four-way drop-2 voicing of Cmaj6 should include correct chord tones" <|
                \_ ->
                    let
                        expected =
                            Ok
                                { voiceOne = Pitch.e5
                                , voiceTwo = Pitch.a4
                                , voiceThree = Pitch.g4
                                , voiceFour = Pitch.c4
                                }

                        availablePitchClasses =
                            AnalyzeChord.availablePitchClassesFor
                                (Chord.chord PitchClass.c ChordClass.majorSix)
                                |> Result.mapError GenerateVoicing.AnalyzeChordError

                        result =
                            Result.andThen
                                (GenerateVoicing.drop2 Pitch.e5)
                                availablePitchClasses
                    in
                    Expect.equal result expected
            ]
        , describe "drop3"
            [ test "four-way drop-3 voicing of Cmaj6 should include correct chord tones" <|
                \_ ->
                    let
                        expected =
                            Ok
                                { voiceOne = Pitch.e5
                                , voiceTwo = Pitch.c5
                                , voiceThree = Pitch.g4
                                , voiceFour = Pitch.a3
                                }

                        availablePitchClasses =
                            AnalyzeChord.availablePitchClassesFor
                                (Chord.chord PitchClass.c ChordClass.majorSix)
                                |> Result.mapError GenerateVoicing.AnalyzeChordError

                        result =
                            Result.andThen
                                (GenerateVoicing.drop3 Pitch.e5)
                                availablePitchClasses
                    in
                    Expect.equal result expected
            ]
        , describe "drop2and4"
            [ test "four-way drop-2-and-4 voicing of Cmaj6 should include correct chord tones" <|
                \_ ->
                    let
                        expected =
                            Ok
                                { voiceOne = Pitch.e5
                                , voiceTwo = Pitch.a4
                                , voiceThree = Pitch.c4
                                , voiceFour = Pitch.g3
                                }

                        availablePitchClasses =
                            AnalyzeChord.availablePitchClassesFor
                                (Chord.chord PitchClass.c ChordClass.majorSix)
                                |> Result.mapError GenerateVoicing.AnalyzeChordError

                        result =
                            Result.andThen
                                (GenerateVoicing.drop2and4 Pitch.e5)
                                availablePitchClasses
                    in
                    Expect.equal result expected
            ]
        , describe "fourWayCloseDoubleLead"
            [ test "four-way close double-lead voicing of Cmaj6 should include correct chord tones" <|
                \_ ->
                    let
                        expected =
                            Ok
                                { voiceOne = Pitch.e5
                                , voiceTwo = Pitch.c5
                                , voiceThree = Pitch.a4
                                , voiceFour = Pitch.g4
                                , voiceFive = Pitch.e4
                                }

                        availablePitchClasses =
                            AnalyzeChord.availablePitchClassesFor
                                (Chord.chord PitchClass.c ChordClass.majorSix)
                                |> Result.mapError GenerateVoicing.AnalyzeChordError

                        result =
                            Result.andThen
                                (GenerateVoicing.fourWayCloseDoubleLead Pitch.e5)
                                availablePitchClasses
                    in
                    Expect.equal result expected
            ]
        , describe "fourWaySpread"
            [ test "four-way spread voicing of Cmaj6 should include correct chord tones" <|
                \_ ->
                    let
                        expected =
                            Ok
                                { voiceOne = Pitch.d4
                                , voiceTwo = Pitch.a3
                                , voiceThree = Pitch.e3
                                , voiceFour = Pitch.c3
                                }

                        availablePitchClasses =
                            AnalyzeChord.availablePitchClassesFor
                                (Chord.chord PitchClass.c ChordClass.majorSix)
                                |> Result.mapError GenerateVoicing.AnalyzeChordError

                        result =
                            Result.andThen
                                (GenerateVoicing.fourWaySpread Pitch.c3)
                                availablePitchClasses
                    in
                    Expect.equal result expected
            ]
        , describe "fiveWaySpread"
            [ test "five-way spread voicing of Cmaj6 should include correct chord tones" <|
                \_ ->
                    let
                        expected =
                            Ok
                                { voiceOne = Pitch.fSharp4
                                , voiceTwo = Pitch.d4
                                , voiceThree = Pitch.a3
                                , voiceFour = Pitch.e3
                                , voiceFive = Pitch.c3
                                }

                        availablePitchClasses =
                            AnalyzeChord.availablePitchClassesFor
                                (Chord.chord PitchClass.c ChordClass.majorSix)
                                |> Result.mapError GenerateVoicing.AnalyzeChordError

                        result =
                            Result.andThen
                                (GenerateVoicing.fiveWaySpread Pitch.c3)
                                availablePitchClasses
                    in
                    Expect.equal result expected
            ]
        ]
