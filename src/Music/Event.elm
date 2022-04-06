module Music.Event exposing
    ( Event, new
    , sort
    , Serial, toSerial, fromSerial
    )

{-|

@docs Event, new

@docs sort

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


sort : List (Event a) -> List (Event a)
sort events =
    List.sortBy
        (\e ->
            Duration.toFloat e.at
        )
        events


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
