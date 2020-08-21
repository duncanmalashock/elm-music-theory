module Test.Internal.Spelling exposing (..)

import Expect
import MusicTheory.Internal.Interval as Interval
import MusicTheory.Internal.Pitch as Pitch
import MusicTheory.Internal.Spelling as Spelling
import Test exposing (..)


all : Test
all =
    describe "Spelling tests"
        [ describe "simplify"
            [ test "with no scale specified, and 1 maximum accidental" <|
                \_ ->
                    let
                        chromaticRun : List Pitch.Pitch
                        chromaticRun =
                            List.foldl
                                (\interval pitchList ->
                                    case List.reverse pitchList of
                                        [] ->
                                            []

                                        head :: tail ->
                                            ([ Pitch.transposeUp interval head ] ++ [ head ] ++ tail)
                                                |> List.reverse
                                )
                                [ Pitch.c4 ]
                                [ Interval.augmentedUnison
                                , Interval.augmentedUnison
                                , Interval.augmentedUnison
                                , Interval.augmentedUnison
                                , Interval.augmentedUnison
                                ]

                        expected =
                            [ Pitch.c4
                            , Pitch.cSharp4
                            , Pitch.d4
                            , Pitch.dSharp4
                            , Pitch.e4
                            , Pitch.eSharp4
                            ]

                        result =
                            List.map (Spelling.simplify { maxAccidentals = 1, scale = Nothing })
                                chromaticRun
                    in
                    Expect.equal expected result
            ]
        ]
