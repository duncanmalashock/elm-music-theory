module Fuzzers.PitchClassFuzzer exposing (pitchClass)

import Fuzz exposing (Fuzzer)
import MusicTheory.Internal.PitchClass as Internal
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.PitchClass as PitchClass exposing (PitchClass)


numberToLetter : Int -> Letter
numberToLetter n =
    case n of
        0 ->
            C

        1 ->
            D

        2 ->
            E

        3 ->
            F

        4 ->
            G

        5 ->
            A

        6 ->
            B

        other ->
            if other > 0 then
                numberToLetter (other - 7)

            else
                numberToLetter (other + 7)


numberToAccidental : Int -> Internal.Offset
numberToAccidental n =
    if n == -3 then
        PitchClass.tripleFlat

    else if n == -2 then
        PitchClass.doubleFlat

    else if n == -1 then
        PitchClass.flat

    else if n == 0 then
        PitchClass.natural

    else if n == 1 then
        PitchClass.sharp

    else if n == 2 then
        PitchClass.doubleSharp

    else if n == 3 then
        PitchClass.tripleSharp

    else if n < -3 then
        numberToAccidental (n + 7)

    else
        numberToAccidental (n - 7)


pitchClass : Fuzzer PitchClass
pitchClass =
    Fuzz.map2
        PitchClass.pitchClass
        (Fuzz.intRange 0 6
            |> Fuzz.map numberToLetter
        )
        (Fuzz.intRange -3 3
            |> Fuzz.map numberToAccidental
        )
