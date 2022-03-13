module Music exposing (Music, new)

{-|

@docs Music, new

-}

import Music.Chord exposing (Chord)
import Music.Duration as Duration exposing (Duration)
import Music.Key as Key exposing (Key)
import Music.Meter as Meter exposing (Meter)
import Music.Note as Note
import Music.Range as Range
import Music.Tempo as Tempo exposing (Tempo)


type Music
    = Music Details


type alias Details =
    { tempoEvents : List (Event Tempo)
    , keyEvents : List (Event Key)
    , meterEvents : List (Event Meter)
    , chordEvents : List (Event Chord)
    , noteEvents : List (Event Note)
    , instruments : List Instrument
    }


new :
    { tempo : Tempo.Tempo
    , key : Key.Key
    , meter : Meter.Meter
    }
    -> Music
new { tempo, key, meter } =
    Music
        { tempoEvents =
            [ event Duration.zero tempo newId
            ]
        , keyEvents =
            [ event Duration.zero key newId
            ]
        , meterEvents =
            [ event Duration.zero meter newId
            ]
        , chordEvents = []
        , noteEvents = []
        , instruments = []
        }


type Event a
    = Event
        { at : Duration
        , id : Id
        , value : a
        }


type Id
    = Id Int


newId : Id
newId =
    Id 0


type alias Note =
    { note : Note.Note
    , instrumentId : Id
    }


type alias Instrument =
    { id : Id
    , name : String
    , range : Range.Range
    }


addNote :
    { note : Note.Note
    , instrumentId : Id
    , at : Duration
    }
    -> Music
    -> Music
addNote options (Music music) =
    let
        newNoteEvent : Event Note
        newNoteEvent =
            event options.at
                { note = options.note
                , instrumentId = options.instrumentId
                }
                newId
    in
    Music
        { music
            | noteEvents =
                newNoteEvent :: music.noteEvents
        }


event : Duration -> a -> Id -> Event a
event at value id =
    Event
        { at = at
        , id = id
        , value = value
        }
