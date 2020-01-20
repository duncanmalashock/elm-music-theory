module MusicTheory.Interval exposing
    ( Interval
    , IntervalNumber(..)
    , IntervalQuality(..)
    , all
    , augmentedFifth
    , augmentedFourth
    , augmentedSecond
    , augmentedSeventh
    , augmentedSixth
    , augmentedThird
    , augmentedUnison
    , complementary
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
    | Octave


type IntervalQuality
    = Diminished
    | Minor
    | Perfect
    | Major
    | Augmented


type Interval
    = Interval IntervalQuality IntervalNumber



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
    let
        isPerfectIntervalNumber =
            case intervalNumber of
                Fourth ->
                    True

                Fifth ->
                    True

                Octave ->
                    True

                _ ->
                    False
    in
    intervalNumberSemitones intervalNumber + intervalQualitySemitones isPerfectIntervalNumber intervalQuality


complementary : Interval -> Interval
complementary (Interval intervalQuality intervalNumber) =
    Interval (complementaryIntervalQuality intervalQuality) (complementaryIntervalNumber intervalNumber)



-- INTERVALS


perfectUnison : Interval
perfectUnison =
    Interval Perfect Unison


diminishedSecond : Interval
diminishedSecond =
    Interval Diminished Second


minorSecond : Interval
minorSecond =
    Interval Minor Second


augmentedUnison : Interval
augmentedUnison =
    Interval Augmented Unison


majorSecond : Interval
majorSecond =
    Interval Major Second


diminishedThird : Interval
diminishedThird =
    Interval Diminished Third


minorThird : Interval
minorThird =
    Interval Minor Third


augmentedSecond : Interval
augmentedSecond =
    Interval Augmented Second


majorThird : Interval
majorThird =
    Interval Major Third


diminishedFourth : Interval
diminishedFourth =
    Interval Diminished Fourth


perfectFourth : Interval
perfectFourth =
    Interval Perfect Fourth


augmentedThird : Interval
augmentedThird =
    Interval Augmented Third


augmentedFourth : Interval
augmentedFourth =
    Interval Augmented Fourth


diminishedFifth : Interval
diminishedFifth =
    Interval Diminished Fifth


perfectFifth : Interval
perfectFifth =
    Interval Perfect Fifth


diminishedSixth : Interval
diminishedSixth =
    Interval Diminished Sixth


augmentedFifth : Interval
augmentedFifth =
    Interval Augmented Fifth


minorSixth : Interval
minorSixth =
    Interval Minor Sixth


majorSixth : Interval
majorSixth =
    Interval Major Sixth


diminishedSeventh : Interval
diminishedSeventh =
    Interval Diminished Seventh


minorSeventh : Interval
minorSeventh =
    Interval Minor Seventh


augmentedSixth : Interval
augmentedSixth =
    Interval Augmented Sixth


majorSeventh : Interval
majorSeventh =
    Interval Major Seventh


diminishedOctave : Interval
diminishedOctave =
    Interval Diminished Octave


perfectOctave : Interval
perfectOctave =
    Interval Perfect Octave


augmentedSeventh : Interval
augmentedSeventh =
    Interval Augmented Seventh



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

        Octave ->
            12


intervalQualitySemitones : Bool -> IntervalQuality -> Int
intervalQualitySemitones isPerfectIntervalNumber intervalQuality =
    case intervalQuality of
        Diminished ->
            if isPerfectIntervalNumber then
                -1

            else
                -2

        Minor ->
            -1

        Perfect ->
            0

        Major ->
            0

        Augmented ->
            1


complementaryIntervalNumber : IntervalNumber -> IntervalNumber
complementaryIntervalNumber interval =
    case interval of
        Unison ->
            Octave

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

        Octave ->
            Unison


complementaryIntervalQuality : IntervalQuality -> IntervalQuality
complementaryIntervalQuality intervalQuality =
    case intervalQuality of
        Diminished ->
            Augmented

        Minor ->
            Major

        Perfect ->
            Perfect

        Major ->
            Minor

        Augmented ->
            Diminished


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

        Octave ->
            7
