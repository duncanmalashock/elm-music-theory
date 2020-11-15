module Music.Internal.Sequence exposing
    ( NoteEntry(..)
    , NoteEvent
    , Sequence
    , appendNote
    , appendRest
    , appendTuplet
    , getNoteEntries
    , init
    , toEvents
    , tuplet
    )

import List.Extra
import Music.Internal.Note as Note
import Music.Internal.Time as Time


defaultTempo =
    -- TODO: implement tempo changes correctly
    160


type NoteEntry
    = EntryNote SingleNote
    | EntryTuplet TupletEntry
    | EntryRest RestEntry


type alias SingleNote =
    { startTime : Time.Duration
    , note : Note.Note
    }


type alias Tuplet =
    { duration : Time.Duration
    , entries : List NoteEntry
    }


type alias TupletEntry =
    { startTime : Time.Duration
    , tuplet : Tuplet
    }


type alias RestEntry =
    { startTime : Time.Duration
    , duration : Time.Duration
    }


type alias TempoEntry =
    { time : Time.Duration
    , tempo : Int
    }


type Sequence
    = Sequence
        { noteEntries : List NoteEntry
        , tempoEntries : List TempoEntry
        }


appendTuplet : Tuplet -> Sequence -> Sequence
appendTuplet theTuplet theSequence =
    let
        newEntryAtTime =
            EntryTuplet
                { startTime = firstAvailableStart theSequence
                , tuplet = theTuplet
                }
    in
    sequence <|
        getNoteEntries theSequence
            ++ [ newEntryAtTime ]


tuplet :
    { duration : Time.Duration
    }
    -> Sequence
    -> Tuplet
tuplet { duration } theSequence =
    { duration = duration
    , entries = normalizeStartTimes (getNoteEntries theSequence)
    }


init : Sequence
init =
    Sequence
        { noteEntries = []
        , tempoEntries = []
        }


initialTempo : Int -> Sequence -> Sequence
initialTempo tempo (Sequence { noteEntries, tempoEntries }) =
    let
        newEntryAtTime =
            { time = Time.zero
            , tempo = tempo
            }
    in
    Sequence
        { noteEntries = noteEntries
        , tempoEntries = tempoEntries ++ [ newEntryAtTime ]
        }


appendTempoChange : Int -> Sequence -> Sequence
appendTempoChange tempo ((Sequence { noteEntries, tempoEntries }) as theSequence) =
    let
        newEntryAtTime =
            { time = firstAvailableStart theSequence
            , tempo = tempo
            }
    in
    Sequence
        { noteEntries = noteEntries
        , tempoEntries = tempoEntries ++ [ newEntryAtTime ]
        }


appendNote : Note.Note -> Sequence -> Sequence
appendNote note theSequence =
    let
        newEntryAtTime =
            EntryNote
                { startTime = firstAvailableStart theSequence
                , note = note
                }
    in
    sequence <|
        getNoteEntries theSequence
            ++ [ newEntryAtTime ]


appendRest : Time.Duration -> Sequence -> Sequence
appendRest restDuration theSequence =
    let
        newEntryAtTime =
            EntryRest
                { startTime = firstAvailableStart theSequence
                , duration = restDuration
                }
    in
    sequence <|
        getNoteEntries theSequence
            ++ [ newEntryAtTime ]


sequence : List NoteEntry -> Sequence
sequence theNoteEntries =
    Sequence
        { noteEntries = theNoteEntries
        , tempoEntries = []
        }


type alias NoteEvent =
    { time : Float
    , pitch : Int
    , duration : Float
    , volume : Int
    }


sortEntries : List NoteEntry -> List NoteEntry
sortEntries entries =
    List.sortWith orderEntries entries


orderEntries : NoteEntry -> NoteEntry -> Order
orderEntries entryA entryB =
    compare
        (Time.toFloat <| getStartTime entryA)
        (Time.toFloat <| getStartTime entryB)


normalizeStartTimes : List NoteEntry -> List NoteEntry
normalizeStartTimes entries =
    let
        startTimeOffset =
            sortEntries entries
                |> List.head
                |> Maybe.map getStartTime
                |> Maybe.withDefault Time.zero
    in
    List.map (addStartTime startTimeOffset) entries


totalDuration : List SingleNote -> Time.Duration
totalDuration entries =
    let
        addDurationToStartTime : SingleNote -> Time.Duration
        addDurationToStartTime note =
            Time.add (Note.duration note.note) note.startTime
    in
    entries
        |> List.reverse
        |> List.head
        |> Maybe.map addDurationToStartTime
        |> Maybe.withDefault Time.zero


addStartTime : Time.Duration -> NoteEntry -> NoteEntry
addStartTime timeToAdd entry =
    case entry of
        EntryNote { startTime, note } ->
            EntryNote
                { startTime = Time.add timeToAdd startTime
                , note = note
                }

        EntryTuplet tupletEntry ->
            EntryTuplet
                { startTime = Time.add timeToAdd tupletEntry.startTime
                , tuplet = tupletEntry.tuplet
                }

        EntryRest { startTime, duration } ->
            EntryRest
                { startTime = Time.add timeToAdd startTime
                , duration = duration
                }


subdivideAt : Time.Duration -> Sequence -> Sequence
subdivideAt theStartTime (Sequence { noteEntries, tempoEntries }) =
    Sequence
        { noteEntries =
            List.concatMap
                (\noteEntry ->
                    if theStartTime == getStartTime noteEntry then
                        case noteEntry of
                            EntryNote { startTime, note } ->
                                let
                                    halfDuration =
                                        Time.multiply Time.half (Note.duration note)
                                in
                                [ EntryNote
                                    { startTime = startTime
                                    , note = Note.setDuration note halfDuration
                                    }
                                , EntryNote
                                    { startTime = Time.add startTime halfDuration
                                    , note = Note.setDuration note halfDuration
                                    }
                                ]

                            EntryTuplet tupletEntry ->
                                [ EntryTuplet
                                    { startTime = tupletEntry.startTime
                                    , tuplet = tupletEntry.tuplet
                                    }
                                ]

                            EntryRest { startTime, duration } ->
                                [ EntryRest
                                    { startTime = startTime
                                    , duration = duration
                                    }
                                ]

                    else
                        [ noteEntry ]
                )
                noteEntries
        , tempoEntries = tempoEntries
        }


addStartTimeToNoteEntry : Time.Duration -> SingleNote -> SingleNote
addStartTimeToNoteEntry timeToAdd entry =
    { startTime = Time.add timeToAdd entry.startTime
    , note = entry.note
    }


getNoteEntries : Sequence -> List NoteEntry
getNoteEntries (Sequence { noteEntries }) =
    noteEntries


getTempoEntries : Sequence -> List TempoEntry
getTempoEntries (Sequence { tempoEntries }) =
    tempoEntries


toEvents : Sequence -> List NoteEvent
toEvents theSequence =
    getNoteEntries theSequence
        |> flattenToNoteEntries
        |> noteEntriesToNoteEvents (getTempoEntries theSequence)


tempoToCoefficient : Int -> Float
tempoToCoefficient tempo =
    4 / (toFloat tempo / 60)


flattenToNoteEntries : List NoteEntry -> List SingleNote
flattenToNoteEntries entriesToConvert =
    entriesToConvert
        |> List.concatMap
            (\entry ->
                case entry of
                    EntryNote noteEntry ->
                        [ noteEntry ]

                    EntryTuplet tupletEntry ->
                        tupletEntry.tuplet.entries
                            |> flattenToNoteEntries
                            |> stretchToDuration tupletEntry.tuplet.duration
                            |> List.map (addStartTimeToNoteEntry tupletEntry.startTime)

                    EntryRest { startTime, duration } ->
                        []
            )


stretchToDuration : Time.Duration -> List SingleNote -> List SingleNote
stretchToDuration duration noteEntries =
    let
        ratio : Time.Duration
        ratio =
            Time.divide duration (totalDuration noteEntries)

        stretch : Time.Duration -> SingleNote -> SingleNote
        stretch ratioToStretch noteEntry =
            { startTime = Time.multiply noteEntry.startTime ratioToStretch
            , note = Note.multiplyDuration noteEntry.note ratioToStretch
            }
    in
    noteEntries
        |> List.map (stretch ratio)


noteEntriesToNoteEvents : List TempoEntry -> List SingleNote -> List NoteEvent
noteEntriesToNoteEvents tempoEntries singleNoteEntries =
    singleNoteEntries
        |> List.map
            (\{ startTime, note } ->
                { time =
                    startTime
                        |> Time.toFloat
                        |> (*) (tempoToCoefficient defaultTempo)
                , pitch = Note.toMidiNoteNumber note
                , duration =
                    Note.duration note
                        |> Time.toFloat
                        |> (*) (tempoToCoefficient defaultTempo)
                , volume = Note.toMidiVelocity note
                }
            )


allTempoEventPoints : Sequence -> List Time.Duration
allTempoEventPoints (Sequence { noteEntries, tempoEntries }) =
    let
        tempoEntryTimes =
            List.map .time tempoEntries

        noteEntryStartTimes =
            List.map getStartTime noteEntries

        noteEntryEndTimes =
            List.map getEndTime noteEntries
    in
    (tempoEntryTimes ++ noteEntryStartTimes ++ noteEntryEndTimes)
        |> List.Extra.uniqueBy Time.toFloat
        |> Time.sort


getStartTime : NoteEntry -> Time.Duration
getStartTime entry =
    case entry of
        EntryNote noteEntry ->
            noteEntry.startTime

        EntryTuplet tupletEntry ->
            tupletEntry.startTime

        EntryRest restEntry ->
            restEntry.startTime


getDuration : NoteEntry -> Time.Duration
getDuration entry =
    case entry of
        EntryNote noteEntry ->
            Note.duration noteEntry.note

        EntryTuplet tupletEntry ->
            tupletEntry.tuplet.duration

        EntryRest restEntry ->
            restEntry.duration


getEndTime : NoteEntry -> Time.Duration
getEndTime entry =
    getStartTime entry
        |> Time.add (getDuration entry)


firstAvailableStart : Sequence -> Time.Duration
firstAvailableStart theSequence =
    getNoteEntries theSequence
        |> List.reverse
        |> List.head
        |> Maybe.map
            (\entry ->
                Time.add (getStartTime entry) (getDuration entry)
            )
        |> Maybe.withDefault Time.zero
