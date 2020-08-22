module MusicTheory.Internal.Voicing.Util exposing (..)

import MusicTheory.Internal.Chord as Chord
import MusicTheory.Internal.ChordClass as ChordClass
import MusicTheory.Internal.Interval as Interval


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


getFactor : List Interval.Interval -> Chord.Chord -> Maybe Interval.Interval
getFactor factorMembers chord =
    ChordClass.toIntervals (Chord.chordClass chord)
        |> List.filter (\i -> List.member i factorMembers)
        |> List.head
