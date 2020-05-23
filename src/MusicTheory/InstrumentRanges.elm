module MusicTheory.InstrumentRanges exposing (..)

import MusicTheory.Pitch as Pitch


sopranoVoice : Pitch.Range
sopranoVoice =
    Pitch.range Pitch.c4 Pitch.c6


altoVoice : Pitch.Range
altoVoice =
    Pitch.range Pitch.f3 Pitch.f5


tenorVoice : Pitch.Range
tenorVoice =
    Pitch.range Pitch.c3 Pitch.c5


bassVoice : Pitch.Range
bassVoice =
    Pitch.range Pitch.e2 Pitch.e4
