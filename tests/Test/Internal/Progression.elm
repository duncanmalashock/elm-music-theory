module Test.Internal.Progression exposing (..)

import Expect
import Internal.Analysis as Analysis
import Internal.Chord as Chord
import Internal.Key as Key
import Internal.Modulation as Modulation
import Internal.Progression as Progression
import Music.Duration as Duration
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Progression Tests"
        [ test "Giant steps" <|
            \_ ->
                let
                    fragment1 =
                        Progression.progressionType
                            [ Progression.chord Analysis.i Duration.quarter
                            , Progression.modulate (Modulation.downByFifths 4)
                            , Progression.chord Analysis.v Duration.quarter
                            ]

                    fragment2 =
                        Progression.progressionType
                            [ Progression.chord Analysis.i Duration.half
                            , Progression.modulate (Modulation.downByFifths 8)
                            , Progression.chord Analysis.ii Duration.quarter
                            , Progression.chord Analysis.v Duration.quarter
                            ]

                    fragment3 =
                        Progression.progressionType
                            [ Progression.chord Analysis.i Duration.half
                            , Progression.modulate (Modulation.downByFifths 4)
                            , Progression.chord Analysis.ii Duration.quarter
                            , Progression.chord Analysis.v Duration.quarter
                            ]

                    progressionType =
                        Progression.join
                            [ fragment1
                            , fragment1
                            , fragment2
                            , fragment1
                            , fragment1
                            , fragment2
                            , fragment2
                            , fragment2
                            , fragment2
                            , fragment3
                            ]

                    expected =
                        [ "BM7"
                        , "D7"
                        , "GM7"
                        , "B♭7"
                        , "E♭M7"
                        , "Am7"
                        , "D7"
                        , "GM7"
                        , "B♭7"
                        , "E♭M7"
                        , "F♯7"
                        , "BM7"
                        , "Fm7"
                        , "B♭7"
                        , "E♭M7"
                        , "Am7"
                        , "D7"
                        , "GM7"
                        , "C♯m7"
                        , "F♯7"
                        , "BM7"
                        , "Fm7"
                        , "B♭7"
                        , "E♭M7"
                        , "C♯m7"
                        , "F♯7"
                        ]

                    result =
                        Progression.fromProgressionType
                            Key.b
                            Analysis.seventhsByDefault
                            Modulation.simplifyAndPreferGFlat
                            progressionType
                            |> (\p ->
                                    Progression.chords p
                                        |> List.map Chord.symbol
                               )
                in
                Expect.equal expected result
        ]
