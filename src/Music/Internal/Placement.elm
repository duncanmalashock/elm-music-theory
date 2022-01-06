module Music.Internal.Placement exposing
    ( Placement(..)
    , checkIntervals
    , placeAbove
    , placeAboveByAtLeast
    , placeAnywhere
    , placeBelow
    , placeBelowByAtLeast
    , placeWithin
    , toString
    )

import Music.Internal.Interval as Interval


type Placement
    = PlacementAnywhere
    | PlacementAbove
    | PlacementAboveByAtLeast Interval.Interval
    | PlacementBelow
    | PlacementBelowByAtLeast Interval.Interval
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

        PlacementBelow ->
            Interval.subtract from to
                |> Interval.isLessThan Interval.perfectUnison

        PlacementBelowByAtLeast minInterval ->
            Interval.subtract to from
                |> Interval.isGreaterThan minInterval

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


placeBelow : Placement
placeBelow =
    PlacementBelow


placeBelowByAtLeast : Interval.Interval -> Placement
placeBelowByAtLeast interval =
    PlacementBelowByAtLeast interval


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

        PlacementBelow ->
            "<"

        PlacementBelowByAtLeast interval ->
            "<" ++ Interval.shortName interval

        PlacementWithin interval ->
            "<" ++ Interval.shortName interval ++ ">"
