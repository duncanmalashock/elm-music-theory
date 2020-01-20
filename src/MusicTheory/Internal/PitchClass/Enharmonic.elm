module MusicTheory.Internal.PitchClass.Enharmonic exposing (NaturalOrSingleAccidental(..), semitonesToNaturalOrAccidental)

import MusicTheory.Internal.PitchClass as Internal
import MusicTheory.Letter as Letter exposing (Letter(..))


type NaturalOrSingleAccidental
    = Nat Letter Int
    | SharpFlat Letter Letter Int


semitonesToNaturalOrAccidental : Int -> Int -> NaturalOrSingleAccidental
semitonesToNaturalOrAccidental octaveOffset semitones =
    if semitones == 0 then
        Nat C octaveOffset

    else if semitones == 1 then
        SharpFlat C D octaveOffset

    else if semitones == 2 then
        Nat D octaveOffset

    else if semitones == 3 then
        SharpFlat D E octaveOffset

    else if semitones == 4 then
        Nat E octaveOffset

    else if semitones == 5 then
        Nat F octaveOffset

    else if semitones == 6 then
        SharpFlat F G octaveOffset

    else if semitones == 7 then
        Nat G octaveOffset

    else if semitones == 8 then
        SharpFlat G A octaveOffset

    else if semitones == 9 then
        Nat A octaveOffset

    else if semitones == 10 then
        SharpFlat A B octaveOffset

    else if semitones == 11 then
        Nat B octaveOffset

    else if semitones > 11 then
        semitonesToNaturalOrAccidental (octaveOffset + 1) (semitones - 12)

    else
        semitonesToNaturalOrAccidental (octaveOffset - 1) (semitones + 12)
