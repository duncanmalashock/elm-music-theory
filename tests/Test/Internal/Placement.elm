module Test.Internal.Placement exposing (..)

import Expect
import Music.Internal.Interval as Interval
import Music.Internal.Placement as Placement
import Test exposing (Test, describe, test)


intervalPairs : List { from : Interval.Interval, to : Interval.Interval }
intervalPairs =
    [ { from = Interval.perfectUnison
      , to = Interval.minorThird
      }
    , { from = Interval.perfectUnison
      , to = Interval.majorNinth
      }
    , { from = Interval.perfectUnison
      , to =
            Interval.minorThird
                |> Interval.reverse
      }
    , { from = Interval.perfectUnison
      , to =
            Interval.majorNinth
                |> Interval.reverse
      }
    , { from = Interval.minorThird
      , to = Interval.diminishedFifth
      }
    , { from = Interval.minorThird
      , to = Interval.perfectEleventh
      }
    , { from =
            Interval.minorThird
                |> Interval.reverse
      , to =
            Interval.diminishedFifth
                |> Interval.reverse
      }
    , { from =
            Interval.minorThird
                |> Interval.reverse
      , to =
            Interval.perfectEleventh
                |> Interval.reverse
      }
    ]


all : Test
all =
    describe "Placement Tests"
        [ describe "checkIntervals"
            [ test "'anywhere' should allow any intervals" <|
                \_ ->
                    let
                        placement =
                            Placement.placeAnywhere

                        result =
                            intervalPairs
                                |> List.map
                                    (Placement.checkIntervals placement)

                        expected =
                            [ True
                            , True
                            , True
                            , True
                            , True
                            , True
                            , True
                            , True
                            ]
                    in
                    Expect.equal expected result
            , test "'placeAbove' should allow only where second interval is greater than first" <|
                \_ ->
                    let
                        placement =
                            Placement.placeAbove

                        result =
                            intervalPairs
                                |> List.map
                                    (Placement.checkIntervals placement)

                        expected =
                            [ True
                            , True
                            , False
                            , False
                            , True
                            , True
                            , False
                            , False
                            ]
                    in
                    Expect.equal expected result
            , test "'placeBelow' should allow only where first interval is greater than second" <|
                \_ ->
                    let
                        placement =
                            Placement.placeBelow

                        result =
                            intervalPairs
                                |> List.map
                                    (Placement.checkIntervals placement)

                        expected =
                            [ False
                            , False
                            , True
                            , True
                            , False
                            , False
                            , True
                            , True
                            ]
                    in
                    Expect.equal expected result
            , test "'placeAboveByAtLeast' should allow only where second interval is greater than first by a certain amount" <|
                \_ ->
                    let
                        placement =
                            Placement.placeAboveByAtLeast Interval.majorThird

                        result =
                            intervalPairs
                                |> List.map
                                    (Placement.checkIntervals placement)

                        expected =
                            [ False
                            , True
                            , False
                            , False
                            , False
                            , True
                            , False
                            , False
                            ]
                    in
                    Expect.equal expected result
            , test "'placeAboveByAtMost' should allow only where second interval is greater than first by at most certain amount" <|
                \_ ->
                    let
                        placement =
                            Placement.placeAboveByAtMost Interval.majorThird

                        result =
                            intervalPairs
                                |> List.map
                                    (Placement.checkIntervals placement)

                        expected =
                            [ True
                            , False
                            , False
                            , False
                            , True
                            , False
                            , False
                            , False
                            ]
                    in
                    Expect.equal expected result
            , test "'placeBelowByAtLeast' should allow only where first interval is greater than second by a certain amount" <|
                \_ ->
                    let
                        placement =
                            Placement.placeBelowByAtLeast Interval.majorThird

                        result =
                            intervalPairs
                                |> List.map
                                    (Placement.checkIntervals placement)

                        expected =
                            [ False
                            , False
                            , False
                            , True
                            , False
                            , False
                            , False
                            , True
                            ]
                    in
                    Expect.equal expected result
            , test "'placeBelowByAtMost' should allow only where second interval is less than first by at most a certain amount" <|
                \_ ->
                    let
                        placement =
                            Placement.placeBelowByAtMost Interval.majorThird

                        result =
                            intervalPairs
                                |> List.map
                                    (Placement.checkIntervals placement)

                        expected =
                            [ False
                            , False
                            , True
                            , False
                            , False
                            , False
                            , True
                            , False
                            ]
                    in
                    Expect.equal expected result
            , test "'placeWithin' should allow only where the difference between first and second is within an certain amount" <|
                \_ ->
                    let
                        placement =
                            Placement.placeWithin Interval.perfectFifth

                        result =
                            intervalPairs
                                |> List.map
                                    (Placement.checkIntervals placement)

                        expected =
                            [ True
                            , False
                            , True
                            , False
                            , True
                            , False
                            , True
                            , False
                            ]
                    in
                    Expect.equal expected result
            ]
        ]
