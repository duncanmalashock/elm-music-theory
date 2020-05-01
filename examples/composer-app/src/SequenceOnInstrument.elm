module SequenceOnInstrument exposing (..)

import MusicTheory.Sequence


type alias SequenceOnInstrument =
    { sequence : MusicTheory.Sequence.Sequence
    , instrumentId : Int
    }


type alias NoteEventWithInstrumentId =
    { time : Float
    , pitch : Int
    , duration : Float
    , instrumentId : Int
    }


toNoteEvents : Int -> SequenceOnInstrument -> List NoteEventWithInstrumentId
toNoteEvents tempo sequenceOnInstrument =
    sequenceOnInstrument.sequence
        |> MusicTheory.Sequence.toEvents tempo
        |> List.map
            (\event ->
                { time = event.time
                , pitch = event.pitch
                , duration = event.duration
                , instrumentId = sequenceOnInstrument.instrumentId
                }
            )
