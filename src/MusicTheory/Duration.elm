module MusicTheory.Duration exposing
    ( Duration
    , add
    , eighth
    , half
    , oneHundredTwentyEighth
    , quarter
    , sixteenth
    , sixtyFourth
    , subdivide
    , thirtySecond
    , twoHundredFiftySixth
    , whole
    )

import Fraction


type Duration
    = Duration Fraction.Fraction


subdivide : Duration -> Duration
subdivide (Duration fraction) =
    Fraction.multiply fraction (Fraction.createUnsafe 1 2)
        |> Duration


add : Duration -> Duration -> Duration
add (Duration fractionA) (Duration fractionB) =
    Fraction.add fractionA fractionB
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
