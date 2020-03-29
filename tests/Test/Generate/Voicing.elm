module Test.Generate.Voicing exposing (all)

import Expect
import MusicTheory.Analyze.Chord as AnalyzeChord
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.Generate.Voicing as GenerateVoicing
import MusicTheory.Pitch as Pitch
import MusicTheory.PitchClass as PitchClass
import Result.Extra
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
        , describe "fourWayDrop2"
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
                                (GenerateVoicing.fourWayClose Pitch.e5)
                                availablePitchClasses
                                |> Result.map2
                                    GenerateVoicing.fourWayDrop2
                                    availablePitchClasses
                                |> Result.Extra.join
                    in
                    Expect.equal result expected
            ]
        , describe "fourWayDrop3"
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
                                (GenerateVoicing.fourWayClose Pitch.e5)
                                availablePitchClasses
                                |> Result.map2
                                    GenerateVoicing.fourWayDrop3
                                    availablePitchClasses
                                |> Result.Extra.join
                    in
                    Expect.equal result expected
            ]
        , describe "fourWayDrop2and4"
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
                                (GenerateVoicing.fourWayClose Pitch.e5)
                                availablePitchClasses
                                |> Result.map2
                                    GenerateVoicing.fourWayDrop2and4
                                    availablePitchClasses
                                |> Result.Extra.join
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
            [ test "five-way spread voicing of C7b9b13 should include correct chord tones" <|
                \_ ->
                    let
                        expected =
                            Ok
                                { voiceOne = Pitch.aFlat4
                                , voiceTwo = Pitch.dFlat4
                                , voiceThree = Pitch.bFlat3
                                , voiceFour = Pitch.e3
                                , voiceFive = Pitch.c3
                                }

                        availablePitchClasses =
                            AnalyzeChord.availablePitchClassesFor
                                (Chord.chord PitchClass.c ChordClass.dominantSeventhFlatNineFlatThirteen)
                                |> Result.mapError GenerateVoicing.AnalyzeChordError

                        result =
                            Result.andThen
                                (GenerateVoicing.fiveWaySpread Pitch.c3)
                                availablePitchClasses
                    in
                    Expect.equal result expected
            ]
        , describe "substituteDoubleLead"
            [ test "substitute double lead voicing of Cm7 should include correct chord tones" <|
                \_ ->
                    let
                        expected =
                            Ok
                                { voiceOne = Pitch.d5
                                , voiceTwo = Pitch.bFlat4
                                , voiceThree = Pitch.g4
                                , voiceFour = Pitch.eFlat4
                                , voiceFive = Pitch.c4
                                }

                        availablePitchClasses =
                            AnalyzeChord.availablePitchClassesFor
                                (Chord.chord PitchClass.c ChordClass.minorSeventh)
                                |> Result.mapError GenerateVoicing.AnalyzeChordError

                        result =
                            Result.andThen
                                (GenerateVoicing.substituteDoubleLead Pitch.d5)
                                availablePitchClasses
                    in
                    Expect.equal result expected
            ]
        ]
