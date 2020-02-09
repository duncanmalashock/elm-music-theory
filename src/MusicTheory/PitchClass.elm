module MusicTheory.PitchClass exposing
    ( PitchClass
    , all
    , areEnharmonicEquivalents
    , doubleFlat
    , doubleSharp
    , flat
    , natural
    , pitchClass
    , semitones
    , sharp
    , transposeDown
    , transposeUp
    , tripleFlat
    , tripleSharp
    )

import MusicTheory.Internal.PitchClass as Internal
import MusicTheory.Interval exposing (Interval)
import MusicTheory.Letter exposing (Letter)


type alias Offset =
    Internal.Offset


type alias PitchClass =
    Internal.PitchClass


tripleFlat : Offset
tripleFlat =
    Internal.tripleFlat


doubleFlat : Offset
doubleFlat =
    Internal.doubleFlat


flat : Offset
flat =
    Internal.flat


natural : Offset
natural =
    Internal.natural


sharp : Offset
sharp =
    Internal.sharp


doubleSharp : Offset
doubleSharp =
    Internal.doubleSharp


tripleSharp : Offset
tripleSharp =
    Internal.tripleSharp


pitchClass : Letter -> Offset -> PitchClass
pitchClass =
    Internal.pitchClass


all : List PitchClass
all =
    Internal.all


semitones : PitchClass -> Int
semitones =
    Internal.semitones


transposeUp : Interval -> PitchClass -> PitchClass
transposeUp =
    Internal.transposeUp


transposeDown : Interval -> PitchClass -> PitchClass
transposeDown =
    Internal.transposeDown


areEnharmonicEquivalents : PitchClass -> PitchClass -> Bool
areEnharmonicEquivalents lhs rhs =
    semitones lhs == semitones rhs
