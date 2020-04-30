port module Ports exposing (..)

import Json.Encode
import MusicTheory.NoteSequence


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


type alias NoteEventWithInstrumentId =
    { time : Float
    , pitch : Int
    , duration : Float
    , instrumentId : Int
    }


playInBrowser : Int -> List MusicTheory.NoteSequence.NoteEvent -> Cmd msg
playInBrowser instrumentId events =
    events
        |> List.map
            (\event ->
                { time = event.time
                , pitch = event.pitch
                , duration = event.duration
                , instrumentId = instrumentId
                }
            )
        |> List.map encode
        |> play


loadInstrument : Int -> Cmd msg
loadInstrument id =
    loadInstrumentById id
