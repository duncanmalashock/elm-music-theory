module Test.Voicing.FivePart exposing (..)

import Expect
import MusicTheory.Analyze.JazzChord as AnalyzeChord
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.Generate.JazzVoicing as GenerateVoicing
import MusicTheory.Interval as Interval
import MusicTheory.PitchClass as PitchClass
import MusicTheory.Voicing.FivePart as FivePart
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Five-part voicing Tests"
        [ describe "allIntervalsFivePart"
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
                                |> Result.map FivePart.allIntervals

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
