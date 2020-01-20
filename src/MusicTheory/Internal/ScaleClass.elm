module MusicTheory.Internal.ScaleClass exposing
    ( HeptatonicIntervals
    , HexatonicIntervals
    , OctatonicIntervals
    , PentatonicIntervals
    , ScaleClass(..)
    )

import MusicTheory.Interval as Interval exposing (Interval)
import MusicTheory.PitchClass as PitchClass exposing (PitchClass)


type ScaleClass
    = Pentatonic PentatonicIntervals
    | Hexatonic HexatonicIntervals
    | Heptatonic HeptatonicIntervals
    | Octatonic OctatonicIntervals


type alias PentatonicIntervals =
    { first : Interval
    , second : Interval
    , third : Interval
    , fourth : Interval
    }


type alias HexatonicIntervals =
    { first : Interval
    , second : Interval
    , third : Interval
    , fourth : Interval
    , fifth : Interval
    }


type alias HeptatonicIntervals =
    { first : Interval
    , second : Interval
    , third : Interval
    , fourth : Interval
    , fifth : Interval
    , sixth : Interval
    }


type alias OctatonicIntervals =
    { first : Interval
    , second : Interval
    , third : Interval
    , fourth : Interval
    , fifth : Interval
    , sixth : Interval
    , seventh : Interval
    }
