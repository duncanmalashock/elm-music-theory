module Music.Internal.Duration exposing
    ( Duration
    , add
    , divide
    , eighth
    , half
    , multiply
    , oneHundredTwentyEighth
    , quarter
    , sixteenth
    , sixtyFourth
    , sort
    , subdivide
    , thirtySecond
    , toFloat
    , twoHundredFiftySixth
    , whole
    , zero
    )

import Fraction


type Duration
    = Duration Fraction.Fraction


sort : List Duration -> List Duration
sort timeList =
    timeList
        |> List.map (\(Duration fraction) -> fraction)
        |> Fraction.sort
        |> List.map Duration


toFloat : Duration -> Float
toFloat (Duration fraction) =
    Fraction.toFloat fraction


zero : Duration
zero =
    Fraction.createUnsafe 0 1
        |> Duration


subdivide : Duration -> Duration
subdivide (Duration fraction) =
    Fraction.multiply fraction (Fraction.createUnsafe 1 2)
        |> Duration


add : Duration -> Duration -> Duration
add (Duration fractionA) (Duration fractionB) =
    Fraction.add fractionA fractionB
        |> Duration


multiply : Duration -> Duration -> Duration
multiply (Duration fractionA) (Duration fractionB) =
    Fraction.multiply fractionA fractionB
        |> Duration


divide : Duration -> Duration -> Duration
divide (Duration fractionA) (Duration fractionB) =
    Fraction.divide fractionA fractionB
        |> Maybe.withDefault (Fraction.createUnsafe 0 1)
        |> Duration


whole : Duration
whole =
    Fraction.createUnsafe 1 1
        |> Duration


half : Duration
half =
    Fraction.createUnsafe 1 2
        |> Duration


quarter : Duration
quarter =
    Fraction.createUnsafe 1 4
        |> Duration


eighth : Duration
eighth =
    Fraction.createUnsafe 1 8
        |> Duration


sixteenth : Duration
sixteenth =
    Fraction.createUnsafe 1 16
        |> Duration


thirtySecond : Duration
thirtySecond =
    Fraction.createUnsafe 1 32
        |> Duration


sixtyFourth : Duration
sixtyFourth =
    Fraction.createUnsafe 1 64
        |> Duration


oneHundredTwentyEighth : Duration
oneHundredTwentyEighth =
    Fraction.createUnsafe 1 128
        |> Duration


twoHundredFiftySixth : Duration
twoHundredFiftySixth =
    Fraction.createUnsafe 1 256
        |> Duration
