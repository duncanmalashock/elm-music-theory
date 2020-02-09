module MusicTheory.Internal.ChordClass exposing (ChordClass(..), toIntervals)

import MusicTheory.Internal.TertianFactors as TertianFactors exposing (TertianFactors)
import MusicTheory.Interval exposing (Interval)


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
