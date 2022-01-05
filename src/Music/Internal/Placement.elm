module Music.Internal.Placement exposing
    ( Placement(..)
    , placeAbove
    , placeAboveByAtLeast
    , placeAnywhere
    , placeBelow
    , placeBelowByAtLeast
    , placeWithin
    )

import Music.Internal.Interval as Interval


type Placement
    = PlacementAnywhere
    | PlacementAbove
    | PlacementAboveByAtLeast Interval.Interval
    | PlacementBelow
    | PlacementBelowByAtLeast Interval.Interval
    | PlacementWithin Interval.Interval


placeAnywhere : Placement
placeAnywhere =
    PlacementAbove


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
