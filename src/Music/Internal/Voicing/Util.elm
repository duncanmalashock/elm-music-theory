module Music.Internal.Voicing.Util exposing (..)

import Music.Internal.ChordType as ChordType
import Music.Internal.Interval as Interval


thirds : List Interval.Interval
thirds =
    [ Interval.majorThird
    , Interval.minorThird
    ]


fifths : List Interval.Interval
fifths =
    [ Interval.diminishedFifth
    , Interval.perfectFifth
    , Interval.augmentedFifth
    ]


sevenths : List Interval.Interval
sevenths =
    [ Interval.diminishedSeventh
    , Interval.minorSeventh
    , Interval.majorSeventh
    ]


getFactor : List Interval.Interval -> ChordType.ChordType -> Maybe Interval.Interval
getFactor factorMembers chordType =
    ChordType.toIntervals chordType
        |> List.filter (\i -> List.member i factorMembers)
        |> List.head
