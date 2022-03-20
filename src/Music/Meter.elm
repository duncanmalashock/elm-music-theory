module Music.Meter exposing
    ( Meter
    , twoTwo
    , threeFour, fourFour, sixFour
    , sixEight, nineEight, twelveEight
    , custom
    , Serial, toSerial
    )

{-|

@docs Meter

@docs twoTwo

@docs threeFour, fourFour, sixFour

@docs sixEight, nineEight, twelveEight

@docs custom

@docs Serial, toSerial

-}

import Music.Duration as Duration exposing (Duration)


type Meter
    = Meter
        { beatsInMeasure : Int
        , beatUnit : Duration
        }


type alias Serial =
    { beatsInMeasure : Int
    , beatUnit : Duration.Serial
    }


toSerial : Meter -> Serial
toSerial (Meter details) =
    { beatsInMeasure = details.beatsInMeasure
    , beatUnit = Duration.toSerial details.beatUnit
    }


twoTwo : Meter
twoTwo =
    custom 2 Duration.half


threeFour : Meter
threeFour =
    custom 3 Duration.quarter


fourFour : Meter
fourFour =
    custom 4 Duration.quarter


sixFour : Meter
sixFour =
    custom 6 Duration.quarter


sixEight : Meter
sixEight =
    custom 6 Duration.eighth


nineEight : Meter
nineEight =
    custom 9 Duration.eighth


twelveEight : Meter
twelveEight =
    custom 12 Duration.eighth


custom : Int -> Duration -> Meter
custom beatsInMeasure beatUnit =
    Meter
        { beatsInMeasure = beatsInMeasure
        , beatUnit = beatUnit
        }
