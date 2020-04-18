port module Ports exposing (..)

import Json.Encode


port play : List Json.Encode.Value -> Cmd msg


encode : { time : Float, pitch : Int, duration : Float } -> Json.Encode.Value
encode event =
    Json.Encode.object
        [ ( "time", Json.Encode.float event.time )
        , ( "pitch", Json.Encode.int event.pitch )
        , ( "duration", Json.Encode.float event.duration )
        ]


playback : Cmd msg
playback =
    [ { time = 0
      , pitch = 31
      , duration = 4
      }
    , { time = 0
      , pitch = 43
      , duration = 4
      }
    , { time = 0
      , pitch = 62
      , duration = 4
      }
    , { time = 0.05
      , pitch = 64
      , duration = 4
      }
    , { time = 0.1
      , pitch = 69
      , duration = 4
      }
    , { time = 0.15
      , pitch = 73
      , duration = 4
      }
    ]
        |> List.map encode
        |> play
