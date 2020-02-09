module Fuzzers.IntervalFuzzer exposing (interval)

import Fuzz exposing (Fuzzer)
import Fuzzers.Fuzzer
import MusicTheory.Interval as Interval


interval : Fuzzer Interval.Interval
interval =
    Fuzzers.Fuzzer.fromList Interval.all
