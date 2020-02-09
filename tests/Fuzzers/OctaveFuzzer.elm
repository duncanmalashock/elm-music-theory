module Fuzzers.OctaveFuzzer exposing (octave)

import Fuzz exposing (Fuzzer)
import Fuzzers.Fuzzer
import MusicTheory.Octave as Octave exposing (Octave)


octave : Fuzzer Octave
octave =
    Fuzzers.Fuzzer.fromList Octave.all
