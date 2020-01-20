module MusicTheory.Duration exposing
    ( Division(..)
    , Duration(..)
    , add
    , divisionToRational
    , dotted
    , doubleDotted
    , eighthNote
    , fromList
    , halfNote
    , oneHundredTwentyEighthNote
    , quarterNote
    , sixteenthNote
    , sixtyFourthNote
    , thirtySecondNote
    , toList
    , toRational
    , tripleDotted
    , wholeNote
    )

import Libs.Ratio as Ratio exposing (Rational)


type Division
    = Whole
    | Half
    | Quarter
    | Eighth
    | Sixteenth
    | ThirtySecond
    | SixtyFourth
    | OneHundredTwentyEighth
    | Zero


type Duration
    = Normal Division
    | Dotted Division
    | DoubleDotted Division
    | TripleDotted Division
    | Sum Duration Duration


wholeNote : Duration
wholeNote =
    Normal Whole


halfNote : Duration
halfNote =
    Normal Half


quarterNote : Duration
quarterNote =
    Normal Quarter


eighthNote : Duration
eighthNote =
    Normal Eighth


sixteenthNote : Duration
sixteenthNote =
    Normal Sixteenth


thirtySecondNote : Duration
thirtySecondNote =
    Normal ThirtySecond


sixtyFourthNote : Duration
sixtyFourthNote =
    Normal SixtyFourth


oneHundredTwentyEighthNote : Duration
oneHundredTwentyEighthNote =
    Normal OneHundredTwentyEighth


zero : Duration
zero =
    Normal Zero


divisionToRational : Division -> Rational
divisionToRational division =
    case division of
        Whole ->
            Ratio.over 1 1

        Half ->
            Ratio.over 1 2

        Quarter ->
            Ratio.over 1 4

        Eighth ->
            Ratio.over 1 8

        Sixteenth ->
            Ratio.over 1 16

        ThirtySecond ->
            Ratio.over 1 32

        SixtyFourth ->
            Ratio.over 1 64

        OneHundredTwentyEighth ->
            Ratio.over 1 128

        Zero ->
            Ratio.over 0 1


toRational : Duration -> Rational
toRational duration =
    case duration of
        Normal d ->
            divisionToRational d

        Dotted d ->
            divisionToRational d |> Ratio.multiply (Ratio.over 3 2)

        DoubleDotted d ->
            divisionToRational d |> Ratio.multiply (Ratio.over 7 4)

        TripleDotted d ->
            divisionToRational d |> Ratio.multiply (Ratio.over 15 8)

        Sum d1 d2 ->
            toRational d1 |> Ratio.add (toRational d2)


toList : Duration -> List Duration
toList duration =
    case duration of
        Normal d ->
            [ Normal d ]

        Dotted d ->
            [ Dotted d ]

        DoubleDotted d ->
            [ DoubleDotted d ]

        TripleDotted d ->
            [ TripleDotted d ]

        Sum d1 d2 ->
            toList d1 ++ toList d2


fromList : List Duration -> Duration
fromList =
    List.foldr Sum zero


dotted : Duration -> Duration
dotted duration =
    case duration of
        Normal d ->
            Dotted d

        Dotted d ->
            Dotted d

        DoubleDotted d ->
            Dotted d

        TripleDotted d ->
            Dotted d

        Sum d1 d2 ->
            Sum (dotted d1) (dotted d2)


doubleDotted : Duration -> Duration
doubleDotted duration =
    case duration of
        Normal d ->
            DoubleDotted d

        Dotted d ->
            DoubleDotted d

        DoubleDotted d ->
            DoubleDotted d

        TripleDotted d ->
            DoubleDotted d

        Sum d1 d2 ->
            Sum (doubleDotted d1) (doubleDotted d2)


tripleDotted : Duration -> Duration
tripleDotted duration =
    case duration of
        Normal d ->
            TripleDotted d

        Dotted d ->
            TripleDotted d

        DoubleDotted d ->
            TripleDotted d

        TripleDotted d ->
            TripleDotted d

        Sum d1 d2 ->
            Sum (tripleDotted d1) (tripleDotted d2)


add : Duration -> Duration -> Duration
add =
    Sum
