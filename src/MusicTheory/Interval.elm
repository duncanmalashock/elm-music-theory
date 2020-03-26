module MusicTheory.Interval exposing
    ( Interval
    , IntervalNumber(..)
    , IntervalQuality(..)
    , addOctave
    , all
    , augmentedFifth
    , augmentedFourth
    , augmentedSecond
    , augmentedSeventh
    , augmentedSixth
    , augmentedThird
    , augmentedUnison
    , complement
    , diminishedFifth
    , diminishedFourth
    , diminishedOctave
    , diminishedSecond
    , diminishedSeventh
    , diminishedSixth
    , diminishedThird
    , intervalNumberIndex
    , majorSecond
    , majorSeventh
    , majorSixth
    , majorThird
    , minorSecond
    , minorSeventh
    , minorSixth
    , minorThird
    , number
    , perfectFifth
    , perfectFourth
    , perfectOctave
    , perfectUnison
    , quality
    , semitones
    )

-- DEFINITION


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
    = Interval IntervalQuality IntervalNumber


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



-- ACCESSORS


quality : Interval -> IntervalQuality
quality (Interval q _) =
    q


number : Interval -> IntervalNumber
number (Interval _ n) =
    n


all : List Interval
all =
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



-- TRANSFORM


semitones : Interval -> Int
semitones (Interval intervalQuality intervalNumber) =
    intervalNumberSemitones
        intervalNumber
        + intervalQualitySemitones intervalQuality


addOctave : Interval -> Interval
addOctave (Interval intervalQuality intervalNumber) =
    Interval intervalQuality (Octave intervalNumber)



-- INTERVALS


perfectUnison : Interval
perfectUnison =
    Interval perfect Unison


diminishedSecond : Interval
diminishedSecond =
    Interval imperfectDiminished Second


minorSecond : Interval
minorSecond =
    Interval minor Second


augmentedUnison : Interval
augmentedUnison =
    Interval perfectAugmented Unison


majorSecond : Interval
majorSecond =
    Interval major Second


diminishedThird : Interval
diminishedThird =
    Interval imperfectDiminished Third


minorThird : Interval
minorThird =
    Interval minor Third


augmentedSecond : Interval
augmentedSecond =
    Interval imperfectAugmented Second


majorThird : Interval
majorThird =
    Interval major Third


diminishedFourth : Interval
diminishedFourth =
    Interval perfectDiminished Fourth


perfectFourth : Interval
perfectFourth =
    Interval perfect Fourth


augmentedThird : Interval
augmentedThird =
    Interval imperfectAugmented Third


augmentedFourth : Interval
augmentedFourth =
    Interval perfectAugmented Fourth


diminishedFifth : Interval
diminishedFifth =
    Interval perfectDiminished Fifth


perfectFifth : Interval
perfectFifth =
    Interval perfect Fifth


diminishedSixth : Interval
diminishedSixth =
    Interval imperfectDiminished Sixth


augmentedFifth : Interval
augmentedFifth =
    Interval perfectAugmented Fifth


minorSixth : Interval
minorSixth =
    Interval minor Sixth


majorSixth : Interval
majorSixth =
    Interval major Sixth


diminishedSeventh : Interval
diminishedSeventh =
    Interval imperfectDiminished Seventh


minorSeventh : Interval
minorSeventh =
    Interval minor Seventh


augmentedSixth : Interval
augmentedSixth =
    Interval imperfectAugmented Sixth


majorSeventh : Interval
majorSeventh =
    Interval major Seventh


diminishedOctave : Interval
diminishedOctave =
    Interval perfectDiminished (Octave Unison)


perfectOctave : Interval
perfectOctave =
    Interval perfect (Octave Unison)


augmentedSeventh : Interval
augmentedSeventh =
    Interval imperfectAugmented Seventh


complement : Interval -> Interval
complement (Interval intervalQuality intervalNumber) =
    Interval
        (complementaryIntervalQuality intervalQuality)
        (complementaryIntervalNumber intervalNumber)



-- HELPERS


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
        Perfect (Offset offset) ->
            offset

        Imperfect (Offset offset) ->
            offset


complementaryIntervalNumber : IntervalNumber -> IntervalNumber
complementaryIntervalNumber interval =
    case interval of
        Unison ->
            Octave Unison

        Second ->
            Seventh

        Third ->
            Sixth

        Fourth ->
            Fifth

        Fifth ->
            Fourth

        Sixth ->
            Third

        Seventh ->
            Second

        Octave intervalNumber ->
            intervalNumber


complementaryIntervalQuality : IntervalQuality -> IntervalQuality
complementaryIntervalQuality intervalQuality =
    case intervalQuality of
        Perfect (Offset offset) ->
            Perfect (Offset <| offset * -1)

        Imperfect (Offset offset) ->
            Imperfect (Offset <| (offset * -1) - 1)


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
