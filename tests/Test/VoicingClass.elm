module Test.VoicingClass exposing (..)

import Expect
import MusicTheory.Analyze.Chord as AnalyzeChord
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.Generate.JazzVoicing as GenerateVoicing
import MusicTheory.Interval as Interval
import MusicTheory.PitchClass as PitchClass
import MusicTheory.VoicingClass as VoicingClass
import Test exposing (Test, describe, test)


all : Test
all =
    describe "VoicingClass Tests"
        [ describe "allIntervalsFourPart"
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
                                |> Result.map VoicingClass.allIntervalsFourPart

                        expected =
                            { voiceFourToVoiceOne = Interval.majorSixth
                            , voiceFourToVoiceTwo = Interval.perfectFourth
                            , voiceThreeToVoiceOne = Interval.perfectFifth
                            , voiceFourToVoiceThree = Interval.majorSecond
                            , voiceThreeToVoiceTwo = Interval.minorThird
                            , voiceTwoToVoiceOne = Interval.majorThird
                            }
                                |> Ok
                    in
                    Expect.equal expected result
            ]
        , describe "allIntervalsFivePart"
            [ test "correct intervals for five-way spread voicing of C7b9b13" <|
                \_ ->
                    let
                        availables =
                            AnalyzeChord.availables
                                (Chord.chord PitchClass.c ChordClass.dominantSeventhFlatNineFlatThirteen)
                                |> Result.mapError GenerateVoicing.AnalyzeChordError

                        result =
                            availables
                                |> Result.andThen
                                    (GenerateVoicing.fiveWaySpread Interval.perfectUnison)
                                |> Result.map VoicingClass.allIntervalsFivePart

                        expected =
                            { voiceFiveToVoiceOne = Interval.minorThirteenth
                            , voiceFiveToVoiceTwo = Interval.minorNinth
                            , voiceFourToVoiceOne = Interval.diminishedFourth |> Interval.addOctave
                            , voiceFiveToVoiceThree = Interval.minorSeventh
                            , voiceFourToVoiceTwo = Interval.diminishedSeventh
                            , voiceThreeToVoiceOne = Interval.minorSeventh
                            , voiceFiveToVoiceFour = Interval.majorThird
                            , voiceFourToVoiceThree = Interval.diminishedFifth
                            , voiceThreeToVoiceTwo = Interval.minorThird
                            , voiceTwoToVoiceOne = Interval.perfectFifth
                            }
                                |> Ok
                    in
                    Expect.equal expected result
            ]
        ]
