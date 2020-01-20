module PitchClassTests exposing (all)

import Expect
import Fuzz
import Maybe.Extra
import MusicTheory.Interval as Interval
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.PitchClass as PitchClass exposing (PitchClass)
import MusicTheory.PitchClass.Enharmonic as Enharmonic
import MusicTheory.PitchClass.Spelling as Spelling exposing (Accidental(..))
import Test exposing (..)
import Util.IntervalFuzzer as IntervalFuzzer
import Util.PitchClassFuzzer as PitchClassFuzzer


all : Test
all =
    describe "Pitch Class Tests"
        [ fuzz PitchClassFuzzer.pitchClass "a pitch class's semitones should be within -1 and 12" <|
            \pc ->
                PitchClass.semitones pc
                    |> Expect.all [ Expect.atLeast -3, Expect.atMost 14 ]
        , fuzz2 PitchClassFuzzer.pitchClass IntervalFuzzer.interval "transpose pitch class by interval, result should have correct number of semitones" <|
            \pc interval ->
                pc
                    |> PitchClass.transposeUp interval
                    |> PitchClass.semitones
                    |> modBy 12
                    |> Expect.equal ((Interval.semitones interval + PitchClass.semitones pc) |> modBy 12)
        , fuzz2 PitchClassFuzzer.pitchClass IntervalFuzzer.interval "transpose a pitch class up and down by the same interval should result in the original pitch class" <|
            \pc interval ->
                pc
                    |> PitchClass.transposeUp interval
                    |> PitchClass.transposeDown interval
                    |> Expect.equal pc
        , fuzz PitchClassFuzzer.pitchClass "transpose pitch class up an octave should result in the original pitch class" <|
            \pc ->
                pc
                    |> PitchClass.transposeUp Interval.perfectOctave
                    |> Expect.equal pc
        , fuzz PitchClassFuzzer.pitchClass "transpose pitch class down an octave should result in the original pitch class" <|
            \pc ->
                pc
                    |> PitchClass.transposeDown Interval.perfectOctave
                    |> Expect.equal pc
        , fuzz PitchClassFuzzer.pitchClass "transpose pitch class up a perfect unison should result in the original pitch class" <|
            \pc ->
                pc
                    |> PitchClass.transposeUp Interval.perfectUnison
                    |> Expect.equal pc
        , fuzz PitchClassFuzzer.pitchClass "transpose pitch class down a perfect unison result in the original pitch class" <|
            \pc ->
                pc
                    |> PitchClass.transposeDown Interval.perfectUnison
                    |> Expect.equal pc
        , fuzz3 PitchClassFuzzer.pitchClass IntervalFuzzer.interval IntervalFuzzer.interval "transpose up and down 2 intervals, expect original pitch class " <|
            \pc i1 i2 ->
                pc
                    |> PitchClass.transposeUp i1
                    |> PitchClass.transposeUp i2
                    |> PitchClass.transposeDown i1
                    |> PitchClass.transposeDown i2
                    |> Expect.equal pc
        , test "toString" <|
            \_ ->
                let
                    testCases =
                        [ ( PitchClass.pitchClass C PitchClass.tripleSharp, "D♯" )
                        , ( PitchClass.pitchClass C PitchClass.flat, "B" )
                        , ( PitchClass.pitchClass C PitchClass.flat, "B" )
                        , ( PitchClass.pitchClass E PitchClass.sharp, "F" )
                        , ( PitchClass.pitchClass A PitchClass.tripleFlat, "G♭" )
                        ]

                    input =
                        testCases |> List.map Tuple.first

                    expected =
                        testCases |> List.map Tuple.second
                in
                input
                    |> List.map PitchClass.toString
                    |> Expect.equal expected
        ]
