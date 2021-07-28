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
import Music.Internal.Duration as Duration
import Music.Internal.Note as Note


defaultTempo =
    120


type NoteEntry
    = EntryNote SingleNote
    | EntryTuplet TupletEntry
    | EntryRest RestEntry


type alias SingleNote =
    { startTime : Duration.Duration
    , note : Note.Note
    }


type alias Tuplet =
    { duration : Duration.Duration
    , entries : List NoteEntry
    }


type alias TupletEntry =
    { startTime : Duration.Duration
    , tuplet : Tuplet
    }


type alias RestEntry =
    { startTime : Duration.Duration
    , duration : Duration.Duration
    }


type Sequence
    = Sequence
        { noteEntries : List NoteEntry
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
    { duration : Duration.Duration
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


appendRest : Duration.Duration -> Sequence -> Sequence
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
        (Duration.toFloat <| getStartTime entryA)
        (Duration.toFloat <| getStartTime entryB)


normalizeStartTimes : List NoteEntry -> List NoteEntry
normalizeStartTimes entries =
    let
        startTimeOffset =
            sortEntries entries
                |> List.head
                |> Maybe.map getStartTime
                |> Maybe.withDefault Duration.zero
    in
    List.map (addStartTime startTimeOffset) entries


totalDuration : List SingleNote -> Duration.Duration
totalDuration entries =
    let
        addDurationToStartTime : SingleNote -> Duration.Duration
        addDurationToStartTime note =
            Duration.add (Note.duration note.note) note.startTime
    in
    entries
        |> List.reverse
        |> List.head
        |> Maybe.map addDurationToStartTime
        |> Maybe.withDefault Duration.zero


addStartTime : Duration.Duration -> NoteEntry -> NoteEntry
addStartTime timeToAdd entry =
    case entry of
        EntryNote { startTime, note } ->
            EntryNote
                { startTime = Duration.add timeToAdd startTime
                , note = note
                }

        EntryTuplet tupletEntry ->
            EntryTuplet
                { startTime = Duration.add timeToAdd tupletEntry.startTime
                , tuplet = tupletEntry.tuplet
                }

        EntryRest { startTime, duration } ->
            EntryRest
                { startTime = Duration.add timeToAdd startTime
                , duration = duration
                }


subdivideAt : Duration.Duration -> Sequence -> Sequence
subdivideAt theStartTime (Sequence { noteEntries }) =
    Sequence
        { noteEntries =
            List.concatMap
                (\noteEntry ->
                    if theStartTime == getStartTime noteEntry then
                        case noteEntry of
                            EntryNote { startTime, note } ->
                                let
                                    halfDuration =
                                        Duration.multiply Duration.half (Note.duration note)
                                in
                                [ EntryNote
                                    { startTime = startTime
                                    , note = Note.setDuration note halfDuration
                                    }
                                , EntryNote
                                    { startTime = Duration.add startTime halfDuration
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
        }


addStartTimeToNoteEntry : Duration.Duration -> SingleNote -> SingleNote
addStartTimeToNoteEntry timeToAdd entry =
    { startTime = Duration.add timeToAdd entry.startTime
    , note = entry.note
    }


getNoteEntries : Sequence -> List NoteEntry
getNoteEntries (Sequence { noteEntries }) =
    noteEntries


toEvents : Sequence -> List NoteEvent
toEvents theSequence =
    getNoteEntries theSequence
        |> flattenToNoteEntries
        |> noteEntriesToNoteEvents


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


stretchToDuration : Duration.Duration -> List SingleNote -> List SingleNote
stretchToDuration duration noteEntries =
    let
        ratio : Duration.Duration
        ratio =
            Duration.divide duration (totalDuration noteEntries)

        stretch : Duration.Duration -> SingleNote -> SingleNote
        stretch ratioToStretch noteEntry =
            { startTime = Duration.multiply noteEntry.startTime ratioToStretch
            , note = Note.multiplyDuration noteEntry.note ratioToStretch
            }
    in
    noteEntries
        |> List.map (stretch ratio)


noteEntriesToNoteEvents : List SingleNote -> List NoteEvent
noteEntriesToNoteEvents singleNoteEntries =
    singleNoteEntries
        |> List.map
            (\{ startTime, note } ->
                { time =
                    startTime
                        |> Duration.toFloat
                        |> (*) (tempoToCoefficient defaultTempo)
                , pitch = Note.toMidiNoteNumber note
                , duration =
                    Note.duration note
                        |> Duration.toFloat
                        |> (*) (tempoToCoefficient defaultTempo)
                , volume = Note.toMidiVelocity note
                }
            )


getStartTime : NoteEntry -> Duration.Duration
getStartTime entry =
    case entry of
        EntryNote noteEntry ->
            noteEntry.startTime

        EntryTuplet tupletEntry ->
            tupletEntry.startTime

        EntryRest restEntry ->
            restEntry.startTime


getDuration : NoteEntry -> Duration.Duration
getDuration entry =
    case entry of
        EntryNote noteEntry ->
            Note.duration noteEntry.note

        EntryTuplet tupletEntry ->
            tupletEntry.tuplet.duration

        EntryRest restEntry ->
            restEntry.duration


getEndTime : NoteEntry -> Duration.Duration
getEndTime entry =
    getStartTime entry
        |> Duration.add (getDuration entry)


firstAvailableStart : Sequence -> Duration.Duration
firstAvailableStart theSequence =
    getNoteEntries theSequence
        |> List.reverse
        |> List.head
        |> Maybe.map
            (\entry ->
                Duration.add (getStartTime entry) (getDuration entry)
            )
        |> Maybe.withDefault Duration.zero
