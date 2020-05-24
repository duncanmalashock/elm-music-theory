module MusicTheory.Interval exposing
    ( Direction(..)
    , Interval
    , IntervalNumber(..)
    , IntervalQuality(..)
    , add
    , addOctave
    , addOffset
    , allSimple
    , augmentedEleventh
    , augmentedFifth
    , augmentedFourth
    , augmentedNinth
    , augmentedOctave
    , augmentedSecond
    , augmentedSeventh
    , augmentedSixth
    , augmentedThird
    , augmentedTwelfth
    , augmentedUnison
    , diminishedFifth
    , diminishedFourth
    , diminishedOctave
    , diminishedSecond
    , diminishedSeventh
    , diminishedSixth
    , diminishedThird
    , diminishedTwelfth
    , direction
    , directionToInteger
    , down
    , firstAbove
    , firstBelow
    , indexToIntervalNumber
    , interval
    , intervalNumberIndex
    , majorNinth
    , majorSecond
    , majorSeventh
    , majorSixth
    , majorTenth
    , majorThird
    , majorThirteenth
    , minorNinth
    , minorSecond
    , minorSeventh
    , minorSixth
    , minorTenth
    , minorThird
    , minorThirteenth
    , number
    , numberToQuality
    , perfectEleventh
    , perfectFifth
    , perfectFourth
    , perfectOctave
    , perfectTwelfth
    , perfectUnison
    , quality
    , reverse
    , semitones
    , subtract
    , toSimple
    , up
    )

import Util.Basic


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


type Direction
    = Up
    | Down


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


firstBelow : Interval -> Interval -> Interval
firstBelow toFind reference =
    firstBelowHelp
        (toFind
            |> Util.Basic.applyNTimes 10 (add perfectOctave)
        )
        reference


firstBelowHelp : Interval -> Interval -> Interval
firstBelowHelp toFind reference =
    let
        refSemitones =
            semitones reference

        toFindSemitones =
            semitones toFind
    in
    if refSemitones > toFindSemitones then
        toFind

    else
        firstBelowHelp (add (perfectOctave |> reverse) toFind) reference


firstAbove : Interval -> Interval -> Interval
firstAbove toFind reference =
    firstAboveHelp
        (toFind
            |> Util.Basic.applyNTimes 10 (subtract perfectOctave)
        )
        reference


firstAboveHelp : Interval -> Interval -> Interval
firstAboveHelp toFind reference =
    let
        refSemitones =
            semitones reference

        toFindSemitones =
            semitones toFind
    in
    if refSemitones < toFindSemitones then
        toFind

    else
        firstAboveHelp (add perfectOctave toFind) reference


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
