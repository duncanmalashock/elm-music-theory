module Music exposing
    ( Music, new
    , addNote, removeNote, addNoteEvents, setInitialTempo
    , noteEvents, tempoEvents
    , Measure, measures
    )

{-|

@docs Music, new

@docs addNote, removeNote, addNoteEvents, setInitialTempo

@docs noteEvents, tempoEvents

@docs Measure, measures

-}

import Music.Chord as Chord
import Music.Duration as Duration
import Music.Event as Event
import Music.Key as Key
import Music.Meter as Meter
import Music.Note as Note
import Music.Tempo as Tempo
import Util.Basic


type Music
    = Music Details


type alias Details =
    { tempoEvents : ( Tempo.Tempo, List (Event.Event Tempo.Tempo) )
    , keyEvents : ( Key.Key, List (Event.Event Key.Key) )
    , measures : ( Meter.Meter, List (Event.Event Meter.Meter) )
    , chordEvents : List (Event.Event Chord.Chord)
    , noteEvents : List (Event.Event Note.Note)
    }


new :
    { tempo : Tempo.Tempo
    , key : Key.Key
    , meter : Meter.Meter
    , measures : Int
    }
    -> Music
new options =
    Music
        { tempoEvents =
            ( options.tempo, [] )
        , keyEvents =
            ( options.key, [] )
        , measures =
            ( options.meter, [] )
        , chordEvents = []
        , noteEvents = []
        }
        |> Util.Basic.applyNTimes (options.measures - 1) addMeasureAtEnd


addMeasureAtEnd : Music -> Music
addMeasureAtEnd (Music music) =
    let
        start : Duration.Duration
        start =
            duration (Music music)

        meter : Meter.Meter
        meter =
            meterAtEnd (Music music)

        newMeasure : Event.Event Meter.Meter
        newMeasure =
            Event.new start meter
    in
    Music
        { music
            | measures =
                case music.measures of
                    ( head, tail ) ->
                        ( head, tail ++ [ newMeasure ] )
        }


meterAtEnd : Music -> Meter.Meter
meterAtEnd (Music music) =
    case music.measures of
        ( head, [] ) ->
            head

        ( head, tail ) ->
            tail
                |> List.map .value
                |> List.reverse
                |> List.head
                |> Maybe.withDefault head


duration : Music -> Duration.Duration
duration (Music music) =
    let
        lastMeasure : Event.Event Meter.Meter
        lastMeasure =
            case music.measures of
                ( head, tail ) ->
                    Event.sort tail
                        |> List.reverse
                        |> List.head
                        |> Maybe.withDefault (Event.new Duration.zero head)
    in
    Meter.toDuration lastMeasure.value
        |> Duration.add lastMeasure.at


addNoteEvents :
    List (Event.Event Note.Note)
    -> Music
    -> Music
addNoteEvents eventsToAdd (Music music) =
    Music
        { music | noteEvents = music.noteEvents ++ eventsToAdd }


toSerial : Music -> Serial
toSerial (Music music) =
    { tempoEvents =
        case music.tempoEvents of
            ( head, tail ) ->
                ( Tempo.toSerial head, List.map (Event.toSerial Tempo.toSerial) tail )
    , keyEvents =
        case music.keyEvents of
            ( head, tail ) ->
                ( Key.toSerial head, List.map (Event.toSerial Key.toSerial) tail )
    , measures =
        case music.measures of
            ( head, tail ) ->
                ( Meter.toSerial head, List.map (Event.toSerial Meter.toSerial) tail )
    , chordEvents = List.map (Event.toSerial Chord.toSerial) music.chordEvents
    , noteEvents = List.map (Event.toSerial Note.toSerial) music.noteEvents
    }


type alias Serial =
    { tempoEvents : ( Tempo.Serial, List (Event.Serial Tempo.Serial) )
    , keyEvents : ( Key.Serial, List (Event.Serial Key.Serial) )
    , measures : ( Meter.Serial, List (Event.Serial Meter.Serial) )
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


setInitialTempo :
    Tempo.Tempo
    -> Music
    -> Music
setInitialTempo newTempo (Music music) =
    Music
        { music
            | tempoEvents =
                case music.tempoEvents of
                    ( _, tail ) ->
                        ( newTempo, tail )
        }


noteEvents : Music -> List (Event.Event Note.Note)
noteEvents (Music music) =
    music.noteEvents


tempoEvents : Music -> List (Event.Event Tempo.Tempo)
tempoEvents (Music music) =
    case music.tempoEvents of
        ( head, tail ) ->
            [ Event.new Duration.zero head ] ++ tail


type alias Measure =
    { start : Duration.Duration
    , meterChange : Maybe Meter.Meter
    }


measures : Music -> List Measure
measures (Music music) =
    case music.measures of
        ( head, tail ) ->
            measuresHelp
                { previous = Nothing
                , remaining = Event.new Duration.zero head :: tail
                }


measuresHelp :
    { previous : Maybe (Event.Event Meter.Meter)
    , remaining : List (Event.Event Meter.Meter)
    }
    -> List Measure
measuresHelp { previous, remaining } =
    let
        eventToMeasure : Event.Event Meter.Meter -> Measure
        eventToMeasure event =
            { start = event.at
            , meterChange =
                if Just event.value == Maybe.map .value previous then
                    Nothing

                else
                    Just event.value
            }
    in
    case remaining of
        [] ->
            []

        head :: tail ->
            eventToMeasure head
                :: measuresHelp
                    { previous = Just head
                    , remaining = tail
                    }
