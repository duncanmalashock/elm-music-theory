module Voice exposing (..)

import MusicTheory.Sequence


type alias Voice =
    { sequence : MusicTheory.Sequence.Sequence
    , instrumentId : Int
    }


type alias NoteEventWithInstrumentId =
    { time : Float
    , pitch : Int
    , duration : Float
    , volume : Int
    , instrumentId : Int
    }


toNoteEvents : Int -> Voice -> List NoteEventWithInstrumentId
toNoteEvents tempo sequenceOnInstrument =
    sequenceOnInstrument.sequence
        |> MusicTheory.Sequence.toEvents tempo
        |> List.map
            (\event ->
                { time = event.time
                , pitch = event.pitch
                , duration = event.duration
                , volume = event.volume
                , instrumentId = sequenceOnInstrument.instrumentId
                }
            )
