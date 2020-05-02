module Test.ScaleClass exposing (all)

import Expect
import MusicTheory.Interval as Interval
import MusicTheory.ScaleClass as ScaleClass
import Test exposing (Test, describe, test)


all : Test
all =
    describe "ScaleClass Tests"
        [ describe "toList"
            [ test "should contain the intervals of the scale in order" <|
                \_ ->
                    let
                        expected =
                            [ Interval.perfectUnison
                            , Interval.majorSecond
                            , Interval.majorThird
                            , Interval.perfectFourth
                            , Interval.perfectFifth
                            , Interval.majorSixth
                            , Interval.majorSeventh
                            ]

                        result =
                            ScaleClass.toList ScaleClass.ionian
                    in
                    Expect.equal expected result
            ]
        ]
