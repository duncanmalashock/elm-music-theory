port module Ports exposing (..)

import Json.Encode
import SequenceOnInstrument exposing (NoteEventWithInstrumentId)


port play : List Json.Encode.Value -> Cmd msg


port loadInstrumentById : Int -> Cmd msg


encode : NoteEventWithInstrumentId -> Json.Encode.Value
encode noteEvent =
    Json.Encode.object
        [ ( "time", Json.Encode.float noteEvent.time )
        , ( "pitch", Json.Encode.int noteEvent.pitch )
        , ( "duration", Json.Encode.float noteEvent.duration )
        , ( "instrumentId", Json.Encode.int noteEvent.instrumentId )
        ]


playInBrowser : List NoteEventWithInstrumentId -> Cmd msg
playInBrowser noteEvents =
    noteEvents
        |> List.map encode
        |> play


loadInstrument : Int -> Cmd msg
loadInstrument id =
    loadInstrumentById id
