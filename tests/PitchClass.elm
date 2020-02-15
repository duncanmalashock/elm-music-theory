module PitchClass exposing (all)

import Expect
import MusicTheory.Interval as Interval
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.PitchClass as PitchClass
import Test exposing (Test, describe)


all : Test
all =
    describe "Pitch Class Tests"
        []



-- [ List.map PitchClass.semitones PitchClass.all
--     |> Expect.all [ Expect.atLeast -3, Expect.atMost 14 ]
-- , fuzz2
--     PitchClassFuzzer.pitchClass
--     IntervalFuzzer.interval
--     "transpose pitch class by interval, result should have correct number of semitones"
--   <|
--     \pc interval ->
--         pc
--             |> PitchClass.transposeUp interval
--             |> PitchClass.semitones
--             |> modBy 12
--             |> Expect.equal ((Interval.semitones interval + PitchClass.semitones pc) |> modBy 12)
-- , fuzz2
--     PitchClassFuzzer.pitchClass
--     IntervalFuzzer.interval
--     "transpose a pitch class up and down by the same interval should result in the original pitch class"
--   <|
--     \pc interval ->
--         pc
--             |> PitchClass.transposeUp interval
--             |> PitchClass.transposeDown interval
--             |> Expect.equal pc
-- , fuzz
--     PitchClassFuzzer.pitchClass
--     "transpose pitch class up an octave should result in the original pitch class"
--   <|
--     \pc ->
--         pc
--             |> PitchClass.transposeUp Interval.perfectOctave
--             |> Expect.equal pc
-- , fuzz
--     PitchClassFuzzer.pitchClass
--     "transpose pitch class down an octave should result in the original pitch class"
--   <|
--     \pc ->
--         pc
--             |> PitchClass.transposeDown Interval.perfectOctave
--             |> Expect.equal pc
-- , fuzz
--     PitchClassFuzzer.pitchClass
--     "transpose pitch class up a perfect unison should result in the original pitch class"
--   <|
--     \pc ->
--         pc
--             |> PitchClass.transposeUp Interval.perfectUnison
--             |> Expect.equal pc
-- , fuzz
--     PitchClassFuzzer.pitchClass
--     "transpose pitch class down a perfect unison result in the original pitch class"
--   <|
--     \pc ->
--         pc
--             |> PitchClass.transposeDown Interval.perfectUnison
--             |> Expect.equal pc
-- , fuzz3
--     PitchClassFuzzer.pitchClass
--     IntervalFuzzer.interval
--     IntervalFuzzer.interval
--     "transpose up and down 2 intervals, expect original pitch class "
--   <|
--     \pc i1 i2 ->
--         pc
--             |> PitchClass.transposeUp i1
--             |> PitchClass.transposeUp i2
--             |> PitchClass.transposeDown i1
--             |> PitchClass.transposeDown i2
--             |> Expect.equal pc
-- ]
