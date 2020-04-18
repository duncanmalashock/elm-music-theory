module MusicTheory.Time exposing
    ( Time
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


type Time
    = Duration Fraction.Fraction


subdivide : Time -> Time
subdivide (Duration fraction) =
    Fraction.multiply fraction (Fraction.createUnsafe 1 2)
        |> Duration


add : Time -> Time -> Time
add (Duration fractionA) (Duration fractionB) =
    Fraction.add fractionA fractionB
        |> Duration


whole : Time
whole =
    Fraction.createUnsafe 1 1
        |> Duration


half : Time
half =
    Fraction.createUnsafe 1 2
        |> Duration


quarter : Time
quarter =
    Fraction.createUnsafe 1 4
        |> Duration


eighth : Time
eighth =
    Fraction.createUnsafe 1 8
        |> Duration


sixteenth : Time
sixteenth =
    Fraction.createUnsafe 1 16
        |> Duration


thirtySecond : Time
thirtySecond =
    Fraction.createUnsafe 1 32
        |> Duration


sixtyFourth : Time
sixtyFourth =
    Fraction.createUnsafe 1 64
        |> Duration


oneHundredTwentyEighth : Time
oneHundredTwentyEighth =
    Fraction.createUnsafe 1 128
        |> Duration


twoHundredFiftySixth : Time
twoHundredFiftySixth =
    Fraction.createUnsafe 1 256
        |> Duration
