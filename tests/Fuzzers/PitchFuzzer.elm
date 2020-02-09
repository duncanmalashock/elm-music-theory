module Fuzzers.PitchFuzzer exposing (pitch, pitchWithOctave)

import Fuzz exposing (Fuzzer)
import Fuzzers.OctaveFuzzer
import Fuzzers.PitchClassFuzzer
import MusicTheory.Octave exposing (Octave)
import MusicTheory.Pitch as Pitch exposing (Pitch)


pitch : Fuzzer Pitch
pitch =
    Fuzz.map2
        Pitch.fromPitchClass
        Fuzzers.OctaveFuzzer.octave
        Fuzzers.PitchClassFuzzer.pitchClass


pitchWithOctave : Octave -> Fuzzer Pitch
pitchWithOctave octave =
    Fuzzers.PitchClassFuzzer.pitchClass
        |> Fuzz.map (Pitch.fromPitchClass octave)
