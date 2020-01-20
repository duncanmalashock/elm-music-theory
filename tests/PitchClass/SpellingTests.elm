module PitchClass.SpellingTests exposing (all)

import Expect
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.PitchClass as PitchClass
import MusicTheory.PitchClass.Spelling as Spelling exposing (Accidental(..))
import Test exposing (..)
import Util.PitchClassFuzzer as Fuzzer


all : Test
all =
    describe "Spelling Tests"
        [ fuzz Fuzzer.pitchClass "simple spelling has same number of semitones" <|
            \pc ->
                Spelling.simple pc
                    |> Expect.all
                        [ Spelling.toPitchClass
                            >> PitchClass.semitones
                            >> Expect.equal (PitchClass.semitones pc |> modBy 12)
                        ]
        , fuzz Fuzzer.pitchClass "naturalOrElseSharp has same number of semitones" <|
            \pc ->
                Spelling.naturalOrElseSharp pc
                    |> Expect.all
                        [ Spelling.toPitchClass
                            >> PitchClass.semitones
                            >> Expect.equal (PitchClass.semitones pc |> modBy 12)
                        ]
        , fuzz Fuzzer.pitchClass "naturalOrElseFlat has same number of semitones" <|
            \pc ->
                Spelling.naturalOrElseFlat pc
                    |> Expect.all
                        [ Spelling.toPitchClass
                            >> PitchClass.semitones
                            >> Expect.equal (PitchClass.semitones pc |> modBy 12)
                        ]
        , test "simple spelling" <|
            \_ ->
                let
                    testCases =
                        [ ( PitchClass.pitchClass C PitchClass.tripleFlat, ( A, Natural ) )
                        , ( PitchClass.pitchClass C PitchClass.doubleFlat, ( B, Flat ) )
                        , ( PitchClass.pitchClass C PitchClass.flat, ( B, Natural ) )
                        , ( PitchClass.pitchClass C PitchClass.natural, ( C, Natural ) )
                        , ( PitchClass.pitchClass C PitchClass.sharp, ( C, Sharp ) )
                        , ( PitchClass.pitchClass C PitchClass.doubleSharp, ( D, Natural ) )
                        , ( PitchClass.pitchClass C PitchClass.tripleSharp, ( D, Sharp ) )
                        , ( PitchClass.pitchClass D PitchClass.tripleFlat, ( B, Natural ) )
                        , ( PitchClass.pitchClass D PitchClass.doubleFlat, ( C, Natural ) )
                        , ( PitchClass.pitchClass D PitchClass.flat, ( D, Flat ) )
                        , ( PitchClass.pitchClass D PitchClass.natural, ( D, Natural ) )
                        , ( PitchClass.pitchClass D PitchClass.sharp, ( D, Sharp ) )
                        , ( PitchClass.pitchClass D PitchClass.doubleSharp, ( E, Natural ) )
                        , ( PitchClass.pitchClass D PitchClass.tripleSharp, ( F, Natural ) )
                        , ( PitchClass.pitchClass E PitchClass.tripleFlat, ( D, Flat ) )
                        , ( PitchClass.pitchClass E PitchClass.doubleFlat, ( D, Natural ) )
                        , ( PitchClass.pitchClass E PitchClass.flat, ( E, Flat ) )
                        , ( PitchClass.pitchClass E PitchClass.natural, ( E, Natural ) )
                        , ( PitchClass.pitchClass E PitchClass.sharp, ( F, Natural ) )
                        , ( PitchClass.pitchClass E PitchClass.doubleSharp, ( F, Sharp ) )
                        , ( PitchClass.pitchClass E PitchClass.tripleSharp, ( G, Natural ) )
                        , ( PitchClass.pitchClass F PitchClass.tripleFlat, ( D, Natural ) )
                        , ( PitchClass.pitchClass F PitchClass.doubleFlat, ( E, Flat ) )
                        , ( PitchClass.pitchClass F PitchClass.flat, ( E, Natural ) )
                        , ( PitchClass.pitchClass F PitchClass.natural, ( F, Natural ) )
                        , ( PitchClass.pitchClass F PitchClass.sharp, ( F, Sharp ) )
                        , ( PitchClass.pitchClass F PitchClass.doubleSharp, ( G, Natural ) )
                        , ( PitchClass.pitchClass F PitchClass.tripleSharp, ( G, Sharp ) )
                        , ( PitchClass.pitchClass G PitchClass.tripleFlat, ( E, Natural ) )
                        , ( PitchClass.pitchClass G PitchClass.doubleFlat, ( F, Natural ) )
                        , ( PitchClass.pitchClass G PitchClass.flat, ( G, Flat ) )
                        , ( PitchClass.pitchClass G PitchClass.natural, ( G, Natural ) )
                        , ( PitchClass.pitchClass G PitchClass.sharp, ( G, Sharp ) )
                        , ( PitchClass.pitchClass G PitchClass.doubleSharp, ( A, Natural ) )
                        , ( PitchClass.pitchClass G PitchClass.tripleSharp, ( A, Sharp ) )
                        , ( PitchClass.pitchClass A PitchClass.tripleFlat, ( G, Flat ) )
                        , ( PitchClass.pitchClass A PitchClass.doubleFlat, ( G, Natural ) )
                        , ( PitchClass.pitchClass A PitchClass.flat, ( A, Flat ) )
                        , ( PitchClass.pitchClass A PitchClass.natural, ( A, Natural ) )
                        , ( PitchClass.pitchClass A PitchClass.sharp, ( A, Sharp ) )
                        , ( PitchClass.pitchClass A PitchClass.doubleSharp, ( B, Natural ) )
                        , ( PitchClass.pitchClass A PitchClass.tripleSharp, ( C, Natural ) )
                        , ( PitchClass.pitchClass B PitchClass.tripleFlat, ( A, Flat ) )
                        , ( PitchClass.pitchClass B PitchClass.doubleFlat, ( A, Natural ) )
                        , ( PitchClass.pitchClass B PitchClass.flat, ( B, Flat ) )
                        , ( PitchClass.pitchClass B PitchClass.natural, ( B, Natural ) )
                        , ( PitchClass.pitchClass B PitchClass.sharp, ( C, Natural ) )
                        , ( PitchClass.pitchClass B PitchClass.doubleSharp, ( C, Sharp ) )
                        , ( PitchClass.pitchClass B PitchClass.tripleSharp, ( D, Natural ) )
                        ]

                    input =
                        testCases |> List.map Tuple.first

                    expected =
                        testCases |> List.map (Tuple.second >> (\( l, a ) -> { letter = l, accidental = a }))
                in
                input
                    |> List.map Spelling.simple
                    |> Expect.equal expected
        ]
