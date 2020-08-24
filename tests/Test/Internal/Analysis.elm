module Test.Internal.Analysis exposing (all)

import Expect
import MusicTheory.Chord as Chord
import MusicTheory.ChordType as ChordType
import MusicTheory.Internal.Analysis as Analysis
import MusicTheory.Internal.Key as Key
import MusicTheory.Internal.PitchClass as PitchClass
import Test exposing (..)


all : Test
all =
    describe "Analysis Tests"
        [ test "fromChord with default chord type" <|
            \_ ->
                let
                    expected =
                        Analysis.iii

                    result =
                        Analysis.fromChord Key.c (Chord.minor PitchClass.e)
                in
                Expect.equal expected result
        , test "fromChord with alternate chord type" <|
            \_ ->
                let
                    expected =
                        Analysis.iii
                            |> Analysis.withChordType ChordType.majorNinth

                    result =
                        Analysis.fromChord Key.c (Chord.majorNinth PitchClass.e)
                in
                Expect.equal expected result
        , test "toChord with default chord type (triads default)" <|
            \_ ->
                let
                    expected =
                        Chord.minor PitchClass.e

                    result =
                        Analysis.toChord Analysis.triadsByDefault Key.c Analysis.iii
                in
                Expect.equal expected result
        , test "toChord with default chord type (sevenths default)" <|
            \_ ->
                let
                    expected =
                        Chord.minorSeventh PitchClass.e

                    result =
                        Analysis.toChord Analysis.seventhsByDefault Key.c Analysis.iii
                in
                Expect.equal expected result
        , test "toChord with alternate chord type" <|
            \_ ->
                let
                    expected =
                        Chord.majorNinth PitchClass.e

                    result =
                        Analysis.toChord Analysis.triadsByDefault
                            Key.c
                            (Analysis.iii |> Analysis.withChordType ChordType.majorNinth)
                in
                Expect.equal expected result
        ]
