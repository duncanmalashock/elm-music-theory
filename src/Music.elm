module Music exposing
    ( Music, new
    , addNote, removeNote, addNoteEvents, setTempo
    , noteEvents, tempoEvents
    )

{-|

@docs Music, new

@docs addNote, removeNote, addNoteEvents, setTempo

@docs noteEvents, tempoEvents

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
    , duration : Duration.Duration
    }


new :
    { tempo : Tempo.Tempo
    , key : Key.Key
    , meter : Meter.Meter
    , measureLength : Int
    }
    -> Music
new options =
    Music
        { tempoEvents =
            [ Event.new Duration.zero options.tempo
            ]
        , keyEvents =
            [ Event.new Duration.zero options.key
            ]
        , meterEvents =
            [ Event.new Duration.zero options.meter
            ]
        , chordEvents = []
        , noteEvents = []
        , duration = Meter.measuresToDuration options.measureLength options.meter
        }


addNoteEvents :
    List (Event.Event Note.Note)
    -> Music
    -> Music
addNoteEvents eventsToAdd (Music music) =
    Music
        { music | noteEvents = music.noteEvents ++ eventsToAdd }


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
    Event.Event Note.Note
    -> Music
    -> Music
addNote options (Music music) =
    let
        newNoteEvent : Event.Event Note.Note
        newNoteEvent =
            Event.new options.at options.value
    in
    Music
        { music
            | noteEvents =
                newNoteEvent :: music.noteEvents
        }


removeNote :
    Event.Event Note.Note
    -> Music
    -> Music
removeNote options (Music music) =
    let
        matches : Event.Event Note.Note -> Bool
        matches current =
            (current.value == options.value)
                && (current.at == options.at)
    in
    Music
        { music
            | noteEvents =
                List.filter (\current -> not (matches current)) music.noteEvents
        }


setTempo :
    Tempo.Tempo
    -> Music
    -> Music
setTempo newTempo (Music music) =
    Music
        { music
            | tempoEvents = [ Event.new Duration.zero newTempo ]
        }


noteEvents : Music -> List (Event.Event Note.Note)
noteEvents (Music music) =
    music.noteEvents


tempoEvents : Music -> List (Event.Event Tempo.Tempo)
tempoEvents (Music music) =
    music.tempoEvents
