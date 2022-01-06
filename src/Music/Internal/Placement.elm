module Music.Internal.Placement exposing
    ( Placement(..)
    , checkIntervals
    , placeAbove
    , placeAboveByAtLeast
    , placeAboveByAtMost
    , placeAnywhere
    , placeBelow
    , placeBelowByAtLeast
    , placeBelowByAtMost
    , placeWithin
    , toString
    )

import Music.Internal.Interval as Interval


type Placement
    = PlacementAnywhere
    | PlacementAbove
    | PlacementAboveByAtLeast Interval.Interval
    | PlacementAboveByAtMost Interval.Interval
    | PlacementBelow
    | PlacementBelowByAtLeast Interval.Interval
    | PlacementBelowByAtMost Interval.Interval
    | PlacementWithin Interval.Interval


checkIntervals : Placement -> { from : Interval.Interval, to : Interval.Interval } -> Bool
checkIntervals placement { from, to } =
    case placement of
        PlacementAnywhere ->
            True

        PlacementAbove ->
            Interval.subtract from to
                |> Interval.isGreaterThan Interval.perfectUnison

        PlacementAboveByAtLeast minInterval ->
            Interval.subtract from to
                |> Interval.isGreaterThan minInterval

        PlacementAboveByAtMost maxInterval ->
            let
                difference =
                    Interval.subtract from to
            in
            Interval.isGreaterThan Interval.perfectUnison difference
                && Interval.isLessThanOrEqualTo maxInterval difference

        PlacementBelow ->
            Interval.subtract to from
                |> Interval.isGreaterThan Interval.perfectUnison

        PlacementBelowByAtLeast minInterval ->
            Interval.subtract to from
                |> Interval.isGreaterThan minInterval

        PlacementBelowByAtMost maxInterval ->
            let
                difference =
                    Interval.subtract to from
            in
            Interval.isGreaterThan Interval.perfectUnison difference
                && Interval.isLessThanOrEqualTo maxInterval difference

        PlacementWithin range ->
            let
                difference =
                    Interval.subtract from to
                        |> Interval.absoluteValue
            in
            Interval.isLessThanOrEqualTo range difference


placeAnywhere : Placement
placeAnywhere =
    PlacementAnywhere


placeAbove : Placement
placeAbove =
    PlacementAbove


placeAboveByAtLeast : Interval.Interval -> Placement
placeAboveByAtLeast interval =
    PlacementAboveByAtLeast interval


placeAboveByAtMost : Interval.Interval -> Placement
placeAboveByAtMost interval =
    PlacementAboveByAtMost interval


placeBelow : Placement
placeBelow =
    PlacementBelow


placeBelowByAtLeast : Interval.Interval -> Placement
placeBelowByAtLeast interval =
    PlacementBelowByAtLeast interval


placeBelowByAtMost : Interval.Interval -> Placement
placeBelowByAtMost interval =
    PlacementBelowByAtMost interval


placeWithin : Interval.Interval -> Placement
placeWithin range =
    PlacementWithin range


toString : Placement -> String
toString placement =
    case placement of
        PlacementAnywhere ->
            "A"

        PlacementAbove ->
            ">"

        PlacementAboveByAtLeast interval ->
            ">" ++ Interval.shortName interval

        PlacementAboveByAtMost interval ->
            ">|" ++ Interval.shortName interval

        PlacementBelow ->
            "<"

        PlacementBelowByAtLeast interval ->
            "<" ++ Interval.shortName interval

        PlacementBelowByAtMost interval ->
            "|<" ++ Interval.shortName interval

        PlacementWithin interval ->
            "<" ++ Interval.shortName interval ++ ">"
