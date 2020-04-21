port module Ports exposing (..)

import Json.Encode
import MusicTheory.Note
import MusicTheory.NoteSequence


port play : List Json.Encode.Value -> Cmd msg


encode : MusicTheory.NoteSequence.NoteEvent -> Json.Encode.Value
encode noteEvent =
    Json.Encode.object
        [ ( "time", Json.Encode.float noteEvent.time )
        , ( "pitch", Json.Encode.int noteEvent.pitch )
        , ( "duration", Json.Encode.float noteEvent.duration )
        ]


playInBrowser : List MusicTheory.NoteSequence.NoteEvent -> Cmd msg
playInBrowser events =
    events
        |> List.map encode
        |> play
