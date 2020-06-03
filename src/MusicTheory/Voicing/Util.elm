module MusicTheory.Voicing.Util exposing (..)

import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.Interval as Interval
import MusicTheory.Pitch as Pitch


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


type alias LowIntervalLimit =
    { intervalInSemitones : Interval.Interval
    , lowestAllowedPitch : Pitch.Pitch
    }


lowIntervalLimitForInterval : Interval.Interval -> Pitch.Pitch -> LowIntervalLimit
lowIntervalLimitForInterval theInterval thePitch =
    LowIntervalLimit theInterval thePitch


lowIntervalLimits : List LowIntervalLimit
lowIntervalLimits =
    [ lowIntervalLimitForInterval Interval.minorSecond Pitch.e3
    , lowIntervalLimitForInterval Interval.majorSecond Pitch.eFlat3
    , lowIntervalLimitForInterval Interval.minorThird Pitch.c3
    , lowIntervalLimitForInterval Interval.majorThird Pitch.bFlat2
    , lowIntervalLimitForInterval Interval.perfectFourth Pitch.bFlat2
    , lowIntervalLimitForInterval Interval.augmentedFourth Pitch.bFlat2
    , lowIntervalLimitForInterval Interval.perfectFifth Pitch.bFlat1
    , lowIntervalLimitForInterval Interval.minorSixth Pitch.g2
    , lowIntervalLimitForInterval Interval.majorSixth Pitch.f2
    , lowIntervalLimitForInterval Interval.minorSeventh Pitch.f2
    , lowIntervalLimitForInterval Interval.majorSeventh Pitch.f2
    , lowIntervalLimitForInterval Interval.minorNinth Pitch.e2
    , lowIntervalLimitForInterval Interval.majorNinth Pitch.eFlat2
    , lowIntervalLimitForInterval Interval.minorTenth Pitch.c2
    , lowIntervalLimitForInterval Interval.majorTenth Pitch.bFlat1
    ]
