module Music exposing
    ( Music, new
    , addNote, removeNote
    , noteEvents
    )

{-|

@docs Music, new

@docs addNote, removeNote

@docs NoteEvent noteEvents

-}

import Music.Chord as Chord
import Music.Duration as Duration
import Music.Event as Event
import Music.Key as Key
import Music.Meter as Meter
import Music.Note as Note
import Music.Tempo as Tempo


type Music
    = Music Details


type alias Details =
    { tempoEvents : List (Event.Event Tempo.Tempo)
    , keyEvents : List (Event.Event Key.Key)
    , meterEvents : List (Event.Event Meter.Meter)
    , chordEvents : List (Event.Event Chord.Chord)
    , noteEvents : List (Event.Event Note.Note)
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
            [ Event.new Duration.zero tempo
            ]
        , keyEvents =
            [ Event.new Duration.zero key
            ]
        , meterEvents =
            [ Event.new Duration.zero meter
            ]
        , chordEvents = []
        , noteEvents = []
        }


toSerial : Music -> Serial
toSerial (Music music) =
    { tempoEvents = List.map (Event.toSerial Tempo.toSerial) music.tempoEvents
    , keyEvents = List.map (Event.toSerial Key.toSerial) music.keyEvents
    , meterEvents = List.map (Event.toSerial Meter.toSerial) music.meterEvents
    , chordEvents = List.map (Event.toSerial Chord.toSerial) music.chordEvents
    , noteEvents = List.map (Event.toSerial Note.toSerial) music.noteEvents
    }


type alias Serial =
    { tempoEvents : List (Event.Serial Tempo.Serial)
    , keyEvents : List (Event.Serial Key.Serial)
    , meterEvents : List (Event.Serial Meter.Serial)
    , chordEvents : List (Event.Serial Chord.Serial)
    , noteEvents : List (Event.Serial Note.Serial)
    }


addNote :
    { note : Note.Note
    , at : Duration.Duration
    }
    -> Music
    -> Music
addNote options (Music music) =
    let
        newNoteEvent : Event.Event Note.Note
        newNoteEvent =
            Event.new options.at options.note
    in
    Music
        { music
            | noteEvents =
                newNoteEvent :: music.noteEvents
        }


removeNote :
    { note : Note.Note
    , at : Duration.Duration
    }
    -> Music
    -> Music
removeNote options (Music music) =
    let
        matches : Event.Event Note.Note -> Bool
        matches current =
            (current.value == options.note)
                && (current.at == options.at)
    in
    Music
        { music
            | noteEvents =
                List.filter (\current -> not (matches current)) music.noteEvents
        }


noteEvents : Music -> List (Event.Event Note.Note)
noteEvents (Music music) =
    music.noteEvents
