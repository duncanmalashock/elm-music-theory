port module Ports exposing (..)

import Json.Encode
import MusicTheory.Note


port play : List Json.Encode.Value -> Cmd msg


type alias NoteEvent =
    { time : Float
    , pitch : Int
    , duration : Float
    }


encode : NoteEvent -> Json.Encode.Value
encode noteEvent =
    Json.Encode.object
        [ ( "time", Json.Encode.float noteEvent.time )
        , ( "pitch", Json.Encode.int noteEvent.pitch )
        , ( "duration", Json.Encode.float noteEvent.duration )
        ]


playInBrowser : List MusicTheory.Note.Note -> Cmd msg
playInBrowser notes =
    notes
        |> List.map
            (\theNote ->
                { time = 0
                , pitch = MusicTheory.Note.toMidiNote theNote
                , duration = 4
                }
            )
        |> List.map encode
        |> play
