module PitchClass.EnharmonicTests exposing (all)

import Expect
import Fuzz
import List.Extra
import Maybe.Extra
import MusicTheory.Internal.PitchClass as Internal
import MusicTheory.Interval as Interval
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.PitchClass as PitchClass
import MusicTheory.PitchClass.Enharmonic as Enharmonic
import MusicTheory.PitchClass.Spelling as Spelling exposing (Accidental(..))
import Test exposing (..)
import Util.IntervalFuzzer as IntervalFuzzer
import Util.PitchClassFuzzer as PitchClassFuzzer


all : Test
all =
    describe "Enharmonic Tests"
        [ fuzz PitchClassFuzzer.pitchClass "all enharmonic equivalents should have same number of semitones" <|
            \pc ->
                Enharmonic.equivalents pc
                    |> List.all (PitchClass.semitones >> (==) (PitchClass.semitones pc))
                    |> Expect.true "semitones should be equal"
        , test "pitch class with sharps, simple with octave offset should yield correct enharmonic equivalent with correct offset offset" <|
            \_ ->
                let
                    testCases =
                        [ ( ( B, 0 ), ( B, PitchClass.natural, 0 ) )
                        , ( ( B, 1 ), ( C, PitchClass.natural, 1 ) )
                        , ( ( B, 2 ), ( C, PitchClass.sharp, 1 ) )
                        , ( ( B, 13 ), ( C, PitchClass.natural, 2 ) )
                        , ( ( A, 3 ), ( C, PitchClass.natural, 1 ) )
                        ]

                    input =
                        testCases
                            |> List.map
                                (Tuple.first >> (\( letter, offset ) -> List.repeat offset Interval.augmentedUnison |> List.foldl PitchClass.transposeUp (PitchClass.pitchClass letter PitchClass.natural)))
                            |> List.map Enharmonic.simpleWithOctaveOffset

                    expected =
                        testCases |> List.map (Tuple.second >> (\( letter, accidental, octaveOffset ) -> ( PitchClass.pitchClass letter accidental, octaveOffset )))
                in
                Expect.equal input expected
        , test "pitch class with flats, simple with octave offset should yield correct enharmonic equivalent with correct offset offset" <|
            \_ ->
                let
                    testCases =
                        [ ( ( C, 0 ), ( C, PitchClass.natural, 0 ) )
                        , ( ( C, -1 ), ( B, PitchClass.natural, -1 ) )
                        , ( ( C, -2 ), ( B, PitchClass.flat, -1 ) )
                        , ( ( C, -13 ), ( B, PitchClass.natural, -2 ) )
                        ]

                    input =
                        testCases
                            |> List.map
                                (Tuple.first >> (\( letter, offset ) -> List.repeat (abs offset) Interval.augmentedUnison |> List.foldl PitchClass.transposeDown (PitchClass.pitchClass letter PitchClass.natural)))
                            |> List.map Enharmonic.simpleWithOctaveOffset

                    expected =
                        testCases |> List.map (Tuple.second >> (\( letter, accidental, octaveOffset ) -> ( PitchClass.pitchClass letter accidental, octaveOffset )))
                in
                Expect.equal input expected
        ]
