module Music.Event exposing
    ( Event, new
    , Serial, toSerial, fromSerial
    )

{-|

@docs Event, new

@docs Serial, toSerial, fromSerial

-}

import Music.Duration as Duration


type alias Event a =
    { at : Duration.Duration
    , value : a
    }


new : Duration.Duration -> a -> Event a
new at value =
    { at = at
    , value = value
    }


toSerial : (a -> serialized) -> Event a -> Serial serialized
toSerial serializeValue event =
    { at = Duration.toSerial event.at
    , value = serializeValue event.value
    }


fromSerial : (serialized -> Maybe a) -> Serial serialized -> Maybe (Event a)
fromSerial fromValue serial =
    Maybe.map2 Event
        (Just (Duration.fromSerial serial.at))
        (fromValue serial.value)


type alias Serial a =
    { at : Duration.Serial
    , value : a
    }
