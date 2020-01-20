module Util.PitchFuzzer exposing (pitch, pitchWithOctave)

import Fuzz exposing (Fuzzer)
import MusicTheory.Interval as Interval
import MusicTheory.Octave exposing (Octave)
import MusicTheory.Pitch as Pitch exposing (Pitch)
import Util.OctaveFuzzer
import Util.PitchClassFuzzer


pitch : Fuzzer Pitch
pitch =
    Fuzz.map2 Pitch.fromPitchClass Util.OctaveFuzzer.octave Util.PitchClassFuzzer.pitchClass


pitchWithOctave : Octave -> Fuzzer Pitch
pitchWithOctave octave =
    Util.PitchClassFuzzer.pitchClass
        |> Fuzz.map (Pitch.fromPitchClass octave)
