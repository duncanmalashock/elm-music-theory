module Internal.Time exposing
    ( Time
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


type Time
    = Time Fraction.Fraction


sort : List Time -> List Time
sort timeList =
    timeList
        |> List.map (\(Time fraction) -> fraction)
        |> Fraction.sort
        |> List.map Time


toFloat : Time -> Float
toFloat (Time fraction) =
    Fraction.toFloat fraction


zero : Time
zero =
    Fraction.createUnsafe 0 1
        |> Time


subdivide : Time -> Time
subdivide (Time fraction) =
    Fraction.multiply fraction (Fraction.createUnsafe 1 2)
        |> Time


add : Time -> Time -> Time
add (Time fractionA) (Time fractionB) =
    Fraction.add fractionA fractionB
        |> Time


multiply : Time -> Time -> Time
multiply (Time fractionA) (Time fractionB) =
    Fraction.multiply fractionA fractionB
        |> Time


divide : Time -> Time -> Time
divide (Time fractionA) (Time fractionB) =
    Fraction.divide fractionA fractionB
        |> Maybe.withDefault (Fraction.createUnsafe 0 1)
        |> Time


whole : Time
whole =
    Fraction.createUnsafe 1 1
        |> Time


half : Time
half =
    Fraction.createUnsafe 1 2
        |> Time


quarter : Time
quarter =
    Fraction.createUnsafe 1 4
        |> Time


eighth : Time
eighth =
    Fraction.createUnsafe 1 8
        |> Time


sixteenth : Time
sixteenth =
    Fraction.createUnsafe 1 16
        |> Time


thirtySecond : Time
thirtySecond =
    Fraction.createUnsafe 1 32
        |> Time


sixtyFourth : Time
sixtyFourth =
    Fraction.createUnsafe 1 64
        |> Time


oneHundredTwentyEighth : Time
oneHundredTwentyEighth =
    Fraction.createUnsafe 1 128
        |> Time


twoHundredFiftySixth : Time
twoHundredFiftySixth =
    Fraction.createUnsafe 1 256
        |> Time
