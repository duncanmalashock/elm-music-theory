module Internal.Interval exposing
    ( Interval, interval
    , isEqualTo, isGreaterThan, isLessThan, isGreaterThanOrEqualTo, isLessThanOrEqualTo
    , add, subtract, addOctave, absoluteValue
    , reverse
    , semitones, shortName
    , perfectUnison
    , minorSecond, majorSecond
    , minorThird, majorThird
    , perfectFourth
    , perfectFifth
    , minorSixth, majorSixth
    , minorSeventh, majorSeventh
    , perfectOctave
    , augmentedUnison, augmentedSecond, augmentedThird, augmentedFourth, augmentedFifth, augmentedSixth, augmentedSeventh
    , diminishedSecond, diminishedThird, diminishedFourth, diminishedFifth, diminishedSixth, diminishedSeventh, diminishedOctave
    , minorNinth, majorNinth, minorTenth, majorTenth, perfectEleventh, perfectTwelfth, minorThirteenth, majorThirteenth
    , augmentedOctave, augmentedNinth, augmentedEleventh, augmentedTwelfth
    , diminishedTwelfth
    , toSimple
    , allSimple
    , Direction(..), direction, up, down, directionToInteger, isNegative, isPositive
    , IntervalNumber(..), number, indexToIntervalNumber, intervalNumberIndex
    , IntervalQuality(..), quality, numberToQuality
    , Range, range, max, min
    , addOffset
    , Serial, toSerial
    )

{-|

@docs Interval, interval

@docs isEqualTo, isGreaterThan, isLessThan, isGreaterThanOrEqualTo, isLessThanOrEqualTo

@docs add, subtract, simplify, addOctave, absoluteValue

@docs reverse, isUp, isDown

@docs semitones, shortName

@docs perfectUnison
@docs minorSecond, majorSecond
@docs minorThird, majorThird
@docs perfectFourth
@docs perfectFifth
@docs minorSixth, majorSixth
@docs minorSeventh, majorSeventh
@docs perfectOctave

@docs augmentedUnison, augmentedSecond, augmentedThird, augmentedFourth, augmentedFifth, augmentedSixth, augmentedSeventh
@docs diminishedSecond, diminishedThird, diminishedFourth, diminishedFifth, diminishedSixth, diminishedSeventh, diminishedOctave
@docs minorNinth, majorNinth, minorTenth, majorTenth, perfectEleventh, perfectTwelfth, minorThirteenth, majorThirteenth
@docs augmentedOctave, augmentedNinth, augmentedEleventh, augmentedTwelfth
@docs diminishedTwelfth

@docs toSimple
@docs allSimple

@docs Direction, direction, up, down, directionToInteger, isNegative, isPositive
@docs IntervalNumber, number, indexToIntervalNumber, intervalNumberIndex
@docs IntervalQuality, quality, numberToQuality
@docs Range, range, max, min

@docs betweenPitches, addOffset

@docs Serial, toSerial

-}


type IntervalNumber
    = Unison
    | Second
    | Third
    | Fourth
    | Fifth
    | Sixth
    | Seventh
    | Octave IntervalNumber


type IntervalQuality
    = Perfect Offset
    | Imperfect Offset


type Offset
    = Offset Int


type Interval
    = Interval Direction IntervalQuality IntervalNumber


type alias Serial =
    { direction : Int
    , quality : String
    , offset : Int
    , number : Int
    }


toSerial : Interval -> Serial
toSerial (Interval d q n) =
    let
        ( qual, offset_ ) =
            case q of
                Perfect (Offset o) ->
                    ( "perfect", o )

                Imperfect (Offset o) ->
                    ( "perfect", o )

        direction_ =
            case d of
                Up ->
                    1

                Down ->
                    -1
    in
    { direction = direction_
    , quality = qual
    , offset = offset_
    , number = intervalNumberIndex n
    }


type Direction
    = Up
    | Down


type Range
    = Range Interval Interval


range : Interval -> Interval -> Range
range a b =
    if semitones a < semitones b then
        Range a b

    else
        Range b a


min : Range -> Interval
min (Range rangeMin rangeMax) =
    rangeMin


max : Range -> Interval
max (Range rangeMin rangeMax) =
    rangeMax


interval : Direction -> IntervalQuality -> IntervalNumber -> Interval
interval dir qual num =
    Interval dir qual num


toSimple : Interval -> Interval
toSimple (Interval dir qual num) =
    case num of
        Octave innerNum ->
            toSimple (Interval dir qual innerNum)

        _ ->
            Interval dir qual num


isEqualTo : Interval -> Interval -> Bool
isEqualTo a b =
    semitones b == semitones a


isGreaterThan : Interval -> Interval -> Bool
isGreaterThan a b =
    subtract b a
        |> semitones
        |> (>) (semitones perfectUnison)


isLessThan : Interval -> Interval -> Bool
isLessThan a b =
    subtract b a
        |> semitones
        |> (<) (semitones perfectUnison)


isGreaterThanOrEqualTo : Interval -> Interval -> Bool
isGreaterThanOrEqualTo a b =
    subtract b a
        |> semitones
        |> (>=) (semitones perfectUnison)


isLessThanOrEqualTo : Interval -> Interval -> Bool
isLessThanOrEqualTo a b =
    subtract b a
        |> semitones
        |> (<=) (semitones perfectUnison)


offset : Interval -> Int
offset (Interval dir qual num) =
    case qual of
        Perfect (Offset int) ->
            int

        Imperfect (Offset int) ->
            int


addOffset : Int -> Interval -> Interval
addOffset int (Interval dir qual num) =
    case qual of
        Perfect (Offset offsetInt) ->
            Interval dir (Perfect (Offset (offsetInt + int))) num

        Imperfect (Offset offsetInt) ->
            Interval dir (Imperfect (Offset (offsetInt + int))) num


up : Direction
up =
    Up


down : Direction
down =
    Down


major : IntervalQuality
major =
    Imperfect (Offset 0)


minor : IntervalQuality
minor =
    Imperfect (Offset -1)


perfect : IntervalQuality
perfect =
    Perfect (Offset 0)


perfectAugmented : IntervalQuality
perfectAugmented =
    Perfect (Offset 1)


perfectDiminished : IntervalQuality
perfectDiminished =
    Perfect (Offset -1)


imperfectAugmented : IntervalQuality
imperfectAugmented =
    Imperfect (Offset 1)


imperfectDiminished : IntervalQuality
imperfectDiminished =
    Imperfect (Offset -2)


quality : Interval -> IntervalQuality
quality (Interval _ q _) =
    q


number : Interval -> IntervalNumber
number (Interval _ _ n) =
    n


numberToQuality : IntervalNumber -> IntervalQuality
numberToQuality num =
    let
        imperfect =
            major
    in
    case num of
        Unison ->
            perfect

        Second ->
            imperfect

        Third ->
            imperfect

        Fourth ->
            perfect

        Fifth ->
            perfect

        Sixth ->
            imperfect

        Seventh ->
            imperfect

        Octave remainingNum ->
            numberToQuality remainingNum


allSimple : List Interval
allSimple =
    [ augmentedFifth
    , augmentedFourth
    , augmentedUnison
    , augmentedSecond
    , augmentedThird
    , augmentedSixth
    , augmentedSeventh
    , diminishedFifth
    , diminishedSecond
    , diminishedThird
    , diminishedFourth
    , diminishedSixth
    , diminishedSeventh
    , diminishedOctave
    , majorSecond
    , majorSeventh
    , majorSixth
    , majorThird
    , minorSecond
    , minorSeventh
    , minorSixth
    , minorThird
    , perfectFifth
    , perfectFourth
    , perfectOctave
    , perfectUnison
    ]


direction : Interval -> Direction
direction (Interval dir intervalQuality intervalNumber) =
    dir


semitones : Interval -> Int
semitones (Interval dir intervalQuality intervalNumber) =
    (intervalNumberSemitones
        intervalNumber
        + intervalQualitySemitones intervalQuality
    )
        * directionToInteger dir


directionToInteger : Direction -> Int
directionToInteger dir =
    case dir of
        Up ->
            1

        Down ->
            -1


isPositive : Interval -> Bool
isPositive i =
    direction i
        |> (==) Up


isNegative : Interval -> Bool
isNegative i =
    direction i
        |> (==) Down


shortName : Interval -> String
shortName (Interval dir intervalQuality intervalNumber) =
    let
        dirAbbreviation =
            case dir of
                Up ->
                    ""

                Down ->
                    "-"

        qualityAbbreviation =
            case intervalQuality of
                Perfect (Offset off) ->
                    if off == 0 then
                        "P"

                    else if off > 0 then
                        List.repeat off "A"
                            |> String.join ""

                    else
                        List.repeat (abs off) "d"
                            |> String.join ""

                Imperfect (Offset off) ->
                    case off of
                        0 ->
                            "M"

                        _ ->
                            if off == -1 then
                                "m"

                            else if off > 0 then
                                List.repeat off "A"
                                    |> String.join ""

                            else
                                List.repeat (abs off - 1) "d"
                                    |> String.join ""

        numberAbbreviation =
            case intervalNumberToNumber intervalNumber of
                1 ->
                    "U"

                8 ->
                    "O"

                anythingElse ->
                    String.fromInt anythingElse
    in
    dirAbbreviation ++ qualityAbbreviation ++ numberAbbreviation


addOctave : Interval -> Interval
addOctave (Interval dir intervalQuality intervalNumber) =
    Interval dir intervalQuality (Octave intervalNumber)


add : Interval -> Interval -> Interval
add ((Interval directionA qualityA numberA) as intA) ((Interval directionB qualityB numberB) as intB) =
    let
        indexA =
            intervalNumberIndex numberA * directionToInteger directionA

        indexB =
            intervalNumberIndex numberB * directionToInteger directionB

        indexSum =
            indexA + indexB

        dir =
            if indexSum >= 0 then
                Up

            else
                Down

        finalNumber =
            indexToIntervalNumber indexSum

        initialQuality =
            numberToQuality finalNumber

        initialResult =
            Interval dir initialQuality finalNumber

        semitoneTarget =
            semitones intA + semitones intB

        semitonesActual =
            semitones initialResult

        semitonesError =
            (semitoneTarget - semitonesActual)
                * directionToInteger dir
    in
    initialResult
        |> addOffset semitonesError


subtract : Interval -> Interval -> Interval
subtract subtrahend minuend =
    add (reverse subtrahend) minuend


reverse : Interval -> Interval
reverse (Interval dir intervalQuality intervalNumber) =
    case dir of
        Up ->
            Interval Down intervalQuality intervalNumber

        Down ->
            Interval Up intervalQuality intervalNumber


absoluteValue : Interval -> Interval
absoluteValue (Interval dir intervalQuality intervalNumber) =
    Interval Up intervalQuality intervalNumber


perfectUnison : Interval
perfectUnison =
    Interval up perfect Unison


diminishedSecond : Interval
diminishedSecond =
    Interval up imperfectDiminished Second


minorSecond : Interval
minorSecond =
    Interval up minor Second


augmentedUnison : Interval
augmentedUnison =
    Interval up perfectAugmented Unison


majorSecond : Interval
majorSecond =
    Interval up major Second


diminishedThird : Interval
diminishedThird =
    Interval up imperfectDiminished Third


minorThird : Interval
minorThird =
    Interval up minor Third


augmentedSecond : Interval
augmentedSecond =
    Interval up imperfectAugmented Second


majorThird : Interval
majorThird =
    Interval up major Third


diminishedFourth : Interval
diminishedFourth =
    Interval up perfectDiminished Fourth


perfectFourth : Interval
perfectFourth =
    Interval up perfect Fourth


augmentedThird : Interval
augmentedThird =
    Interval up imperfectAugmented Third


augmentedFourth : Interval
augmentedFourth =
    Interval up perfectAugmented Fourth


diminishedFifth : Interval
diminishedFifth =
    Interval up perfectDiminished Fifth


perfectFifth : Interval
perfectFifth =
    Interval up perfect Fifth


diminishedSixth : Interval
diminishedSixth =
    Interval up imperfectDiminished Sixth


augmentedFifth : Interval
augmentedFifth =
    Interval up perfectAugmented Fifth


minorSixth : Interval
minorSixth =
    Interval up minor Sixth


majorSixth : Interval
majorSixth =
    Interval up major Sixth


diminishedSeventh : Interval
diminishedSeventh =
    Interval up imperfectDiminished Seventh


minorSeventh : Interval
minorSeventh =
    Interval up minor Seventh


augmentedSixth : Interval
augmentedSixth =
    Interval up imperfectAugmented Sixth


majorSeventh : Interval
majorSeventh =
    Interval up major Seventh


augmentedOctave : Interval
augmentedOctave =
    Interval up perfectAugmented (Octave Unison)


diminishedOctave : Interval
diminishedOctave =
    Interval up perfectDiminished (Octave Unison)


perfectOctave : Interval
perfectOctave =
    Interval up perfect (Octave Unison)


augmentedSeventh : Interval
augmentedSeventh =
    Interval up imperfectAugmented Seventh


minorNinth : Interval
minorNinth =
    Interval up minor (Octave Second)


majorNinth : Interval
majorNinth =
    Interval up major (Octave Second)


augmentedNinth : Interval
augmentedNinth =
    Interval up imperfectAugmented (Octave Second)


minorTenth : Interval
minorTenth =
    Interval up minor (Octave Third)


majorTenth : Interval
majorTenth =
    Interval up major (Octave Third)


perfectEleventh : Interval
perfectEleventh =
    Interval up perfect (Octave Fourth)


augmentedEleventh : Interval
augmentedEleventh =
    Interval up perfectAugmented (Octave Fourth)


diminishedTwelfth : Interval
diminishedTwelfth =
    Interval up perfectDiminished (Octave Fifth)


perfectTwelfth : Interval
perfectTwelfth =
    Interval up perfect (Octave Fifth)


augmentedTwelfth : Interval
augmentedTwelfth =
    Interval up perfectAugmented (Octave Fifth)


minorThirteenth : Interval
minorThirteenth =
    Interval up minor (Octave Sixth)


majorThirteenth : Interval
majorThirteenth =
    Interval up major (Octave Sixth)


intervalNumberSemitones : IntervalNumber -> Int
intervalNumberSemitones intervalNumber =
    case intervalNumber of
        Unison ->
            0

        Second ->
            2

        Third ->
            4

        Fourth ->
            5

        Fifth ->
            7

        Sixth ->
            9

        Seventh ->
            11

        Octave anotherIntervalNumber ->
            12 + intervalNumberSemitones anotherIntervalNumber


intervalQualitySemitones : IntervalQuality -> Int
intervalQualitySemitones intervalQuality =
    case intervalQuality of
        Perfect (Offset off) ->
            off

        Imperfect (Offset off) ->
            off


intervalNumberIndex : IntervalNumber -> Int
intervalNumberIndex intervalNumber =
    case intervalNumber of
        Unison ->
            0

        Second ->
            1

        Third ->
            2

        Fourth ->
            3

        Fifth ->
            4

        Sixth ->
            5

        Seventh ->
            6

        Octave anotherIntervalNumber ->
            7 + intervalNumberIndex anotherIntervalNumber


indexToIntervalNumber : Int -> IntervalNumber
indexToIntervalNumber int =
    case int of
        0 ->
            Unison

        1 ->
            Second

        2 ->
            Third

        3 ->
            Fourth

        4 ->
            Fifth

        5 ->
            Sixth

        6 ->
            Seventh

        _ ->
            let
                absoluteInt =
                    abs int
            in
            if absoluteInt >= 7 then
                Octave (indexToIntervalNumber (absoluteInt - 7))

            else
                indexToIntervalNumber absoluteInt


intervalNumberToNumber : IntervalNumber -> Int
intervalNumberToNumber intervalNumber =
    case intervalNumber of
        Unison ->
            1

        Second ->
            2

        Third ->
            3

        Fourth ->
            4

        Fifth ->
            5

        Sixth ->
            6

        Seventh ->
            7

        Octave anotherIntervalNumber ->
            7 + intervalNumberToNumber anotherIntervalNumber
