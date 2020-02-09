module MusicTheory.Internal.ChordClass exposing (ChordClass(..), toIntervals)

import MusicTheory.Interval exposing (Interval)
import MusicTheory.TertianFactors as TertianFactors exposing (TertianFactors)


type ChordClass
    = Tertian TertianFactors
    | NonTertian (List Interval)


toIntervals : ChordClass -> List Interval
toIntervals chordClass =
    case chordClass of
        Tertian tertianFactors ->
            TertianFactors.toIntervals tertianFactors

        NonTertian intervals ->
            intervals
