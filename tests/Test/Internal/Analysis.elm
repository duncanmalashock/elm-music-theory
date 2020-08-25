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
        [ describe "fromChord" <|
            [ test "default chord type" <|
                \_ ->
                    let
                        expected =
                            Analysis.iii

                        result =
                            Analysis.fromChord Key.c (Chord.minor PitchClass.e)
                    in
                    Expect.equal expected result
            , test "alternate chord type" <|
                \_ ->
                    let
                        expected =
                            Analysis.iii
                                |> Analysis.withChordType ChordType.majorNinth

                        result =
                            Analysis.fromChord Key.c (Chord.majorNinth PitchClass.e)
                    in
                    Expect.equal expected result
            ]
        , describe "toChord" <|
            [ test "default chord type (triads default)" <|
                \_ ->
                    let
                        expected =
                            Chord.minor PitchClass.e

                        result =
                            Analysis.toChord Analysis.triadsByDefault Key.c Analysis.iii
                    in
                    Expect.equal expected result
            , test "default chord type (sevenths default)" <|
                \_ ->
                    let
                        expected =
                            Chord.minorSeventh PitchClass.e

                        result =
                            Analysis.toChord Analysis.seventhsByDefault Key.c Analysis.iii
                    in
                    Expect.equal expected result
            , test "alternate chord type" <|
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
        , describe "symbol" <|
            [ test "default chord type, major key" <|
                \_ ->
                    let
                        expected =
                            "iii"

                        result =
                            Analysis.symbol Key.c Analysis.iii
                    in
                    Expect.equal expected result
            , test "default chord type, minor key" <|
                \_ ->
                    let
                        expected =
                            "III"

                        result =
                            Analysis.symbol Key.cMinor Analysis.iii
                    in
                    Expect.equal expected result
            , test "non-default chord type, major key" <|
                \_ ->
                    let
                        expected =
                            "IIIM7"

                        result =
                            Analysis.symbol Key.c (Analysis.iii |> Analysis.withChordType ChordType.majorSeventh)
                    in
                    Expect.equal expected result
            , test "non-default chord type, minor key" <|
                \_ ->
                    let
                        expected =
                            "iiim7"

                        result =
                            Analysis.symbol Key.cMinor (Analysis.iii |> Analysis.withChordType ChordType.minorSeventh)
                    in
                    Expect.equal expected result
            , test "V7 chord, major key" <|
                \_ ->
                    let
                        expected =
                            "V7"

                        result =
                            Analysis.symbol Key.c (Analysis.v |> Analysis.withChordType ChordType.dominantSeventh)
                    in
                    Expect.equal expected result
            , test "V7 chord, minor key" <|
                \_ ->
                    let
                        expected =
                            "V7"

                        result =
                            Analysis.symbol Key.cMinor (Analysis.v |> Analysis.withChordType ChordType.dominantSeventh)
                    in
                    Expect.equal expected result
            ]
        ]
