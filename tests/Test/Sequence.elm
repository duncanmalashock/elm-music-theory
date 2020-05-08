module Test.Sequence exposing (..)

import Expect
import MusicTheory.Note as Note
import MusicTheory.Pitch as Pitch
import MusicTheory.Sequence as Sequence
import MusicTheory.Time as Time
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Sequence Tests"
        [ describe "allTempoEventPoints"
            [ test "should contain start points and end points for note entries, and times for tempo change entries, de-duplicated" <|
                \_ ->
                    let
                        expected =
                            [ Time.zero
                            , Time.whole
                            , Time.add Time.whole Time.quarter
                            ]

                        result =
                            Sequence.init
                                |> Sequence.initialTempo 60
                                |> Sequence.appendRest Time.whole
                                |> Sequence.appendNote (Note.quarter Pitch.c4)
                                |> Sequence.appendTempoChange 120
                                |> Sequence.allTempoEventPoints
                    in
                    Expect.equal expected result
            ]
        ]
