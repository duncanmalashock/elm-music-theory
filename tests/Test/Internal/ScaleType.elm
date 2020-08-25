module Test.Internal.ScaleType exposing (all)

import Expect
import Music.Internal.Interval as Interval
import Music.Internal.ScaleType as ScaleType
import Test exposing (Test, describe, test)


all : Test
all =
    describe "ScaleType Tests"
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
                            ScaleType.toList ScaleType.ionian
                    in
                    Expect.equal expected result
            ]
        ]
