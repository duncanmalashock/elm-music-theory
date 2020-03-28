module MusicTheory.Analyze.Chord exposing
    ( AvailablePitchClasses
    , Error
    , VoiceCategory(..)
    , availablePitchClassesFor
    , containsPitchClass
    )

import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.Interval as Interval
import MusicTheory.PitchClass as PitchClass


containsPitchClass : PitchClass.PitchClass -> Chord.Chord -> Bool
containsPitchClass pitchClass chord =
    List.member pitchClass
        (Chord.toPitchClasses chord)


type Error
    = CouldNotFindAllVoiceCategories (List (Maybe Interval.Interval))
    | CouldNotDetermineChordQuality


availablePitchClassesFor : Chord.Chord -> Result Error AvailablePitchClasses
availablePitchClassesFor chord =
    let
        chordRoot =
            Chord.root chord

        chordClass =
            Chord.chordClass chord

        maybeQuality =
            determineJazzChordQuality chordClass

        maybeRoot =
            Maybe.andThen (chordToneForClass chordClass Root) maybeQuality

        maybeThird =
            Maybe.andThen (chordToneForClass chordClass Third) maybeQuality

        maybeFifth =
            Maybe.andThen (chordToneForClass chordClass Fifth) maybeQuality

        maybeSeventh =
            Maybe.andThen (chordToneForClass chordClass Seventh) maybeQuality

        chordToneWithSubstitutes : VoiceCategory -> Interval.Interval -> JazzChordQuality -> { true : PitchClass.PitchClass, substitutes : List PitchClass.PitchClass }
        chordToneWithSubstitutes voiceCategory chordTone chordQuality =
            { true = PitchClass.transposeUp chordTone chordRoot
            , substitutes =
                availableTensions voiceCategory chordQuality
                    |> List.map
                        (\interval ->
                            PitchClass.transposeUp interval chordRoot
                        )
            }

        toAvailablePitchClasses :
            JazzChordQuality
            -> Interval.Interval
            -> Interval.Interval
            -> Interval.Interval
            -> Interval.Interval
            -> AvailablePitchClasses
        toAvailablePitchClasses chordQuality root third fifth seventh =
            { root = chordToneWithSubstitutes Root root chordQuality
            , third = chordToneWithSubstitutes Third third chordQuality
            , fifth = chordToneWithSubstitutes Fifth fifth chordQuality
            , seventh = chordToneWithSubstitutes Seventh seventh chordQuality
            }
    in
    case maybeQuality of
        Nothing ->
            Err CouldNotDetermineChordQuality

        Just quality ->
            Maybe.map4
                (toAvailablePitchClasses quality)
                maybeRoot
                maybeThird
                maybeFifth
                maybeSeventh
                |> Result.fromMaybe
                    (CouldNotFindAllVoiceCategories
                        [ maybeRoot
                        , maybeThird
                        , maybeFifth
                        , maybeSeventh
                        ]
                    )


type VoiceCategory
    = Root
    | Third
    | Fifth
    | Seventh


chordToneForClass : ChordClass.ChordClass -> VoiceCategory -> JazzChordQuality -> Maybe Interval.Interval
chordToneForClass chordClass voiceCategory jazzChordQuality =
    let
        intervals =
            ChordClass.toIntervals chordClass

        takeFirst available =
            List.filter (\item -> List.member item intervals) available
                |> List.head
    in
    takeFirst (availableTensions voiceCategory jazzChordQuality)


availableTensions : VoiceCategory -> JazzChordQuality -> List Interval.Interval
availableTensions voiceCategory jazzChordQuality =
    case jazzChordQuality of
        Major6 ->
            case voiceCategory of
                Root ->
                    [ Interval.perfectUnison
                    , Interval.majorNinth
                    ]

                Third ->
                    [ Interval.majorThird
                    ]

                Fifth ->
                    [ Interval.perfectFifth
                    , Interval.augmentedEleventh
                    ]

                Seventh ->
                    [ Interval.majorSixth
                    , Interval.majorSeventh
                    ]

        Major7 ->
            case voiceCategory of
                Root ->
                    [ Interval.perfectUnison
                    , Interval.majorNinth
                    ]

                Third ->
                    [ Interval.majorThird
                    ]

                Fifth ->
                    [ Interval.perfectFifth
                    , Interval.augmentedEleventh
                    ]

                Seventh ->
                    [ Interval.majorSeventh
                    , Interval.majorSixth
                    ]

        Minor6 ->
            case voiceCategory of
                Root ->
                    [ Interval.perfectUnison
                    , Interval.majorNinth
                    ]

                Third ->
                    [ Interval.minorThird
                    ]

                Fifth ->
                    [ Interval.perfectFifth
                    , Interval.perfectEleventh
                    ]

                Seventh ->
                    [ Interval.majorSixth
                    , Interval.minorSeventh
                    ]

        Minor7 ->
            case voiceCategory of
                Root ->
                    [ Interval.perfectUnison
                    , Interval.majorNinth
                    ]

                Third ->
                    [ Interval.majorThird
                    ]

                Fifth ->
                    [ Interval.perfectFifth
                    , Interval.perfectEleventh
                    ]

                Seventh ->
                    [ Interval.minorSeventh
                    , Interval.majorSixth
                    ]

        HalfDiminished ->
            case voiceCategory of
                Root ->
                    [ Interval.perfectUnison
                    , Interval.majorNinth
                    , Interval.perfectEleventh
                    ]

                Third ->
                    [ Interval.majorThird
                    ]

                Fifth ->
                    [ Interval.diminishedFifth
                    ]

                Seventh ->
                    [ Interval.minorSeventh
                    ]

        Dominant7 ->
            case voiceCategory of
                Root ->
                    [ Interval.perfectUnison
                    , Interval.majorNinth
                    , Interval.minorNinth
                    , Interval.augmentedNinth
                    ]

                Third ->
                    [ Interval.majorThird
                    , Interval.perfectFourth
                    ]

                Fifth ->
                    [ Interval.perfectFifth
                    , Interval.augmentedEleventh
                    , Interval.majorThirteenth
                    , Interval.minorThirteenth
                    ]

                Seventh ->
                    [ Interval.minorSeventh
                    ]

        Dominant7Sus ->
            case voiceCategory of
                Root ->
                    [ Interval.perfectUnison
                    , Interval.majorNinth
                    , Interval.minorNinth
                    , Interval.augmentedNinth
                    ]

                Third ->
                    [ Interval.perfectFourth
                    , Interval.majorThird
                    ]

                Fifth ->
                    [ Interval.perfectFifth
                    , Interval.augmentedEleventh
                    , Interval.majorThirteenth
                    , Interval.minorThirteenth
                    ]

                Seventh ->
                    [ Interval.minorSeventh
                    ]

        Diminished7 ->
            case voiceCategory of
                Root ->
                    [ Interval.perfectUnison
                    , Interval.majorNinth
                    ]

                Third ->
                    [ Interval.minorThird
                    , Interval.perfectFourth
                    ]

                Fifth ->
                    [ Interval.diminishedFifth
                    , Interval.minorThirteenth
                    ]

                Seventh ->
                    [ Interval.diminishedSeventh
                    , Interval.diminishedOctave
                    ]


determineJazzChordQuality : ChordClass.ChordClass -> Maybe JazzChordQuality
determineJazzChordQuality chordClass =
    let
        intervals =
            ChordClass.toIntervals chordClass

        containsAll intervalsToCheck =
            List.map
                (\interval ->
                    List.member interval intervals
                )
                intervalsToCheck
                |> List.all identity
    in
    if containsAll (minimumIntervalsForJazzChordQuality Major6) then
        Just Major6

    else if containsAll (minimumIntervalsForJazzChordQuality Major7) then
        Just Major7

    else if containsAll (minimumIntervalsForJazzChordQuality Minor6) then
        Just Minor6

    else if containsAll (minimumIntervalsForJazzChordQuality Minor7) then
        Just Minor7

    else if containsAll (minimumIntervalsForJazzChordQuality HalfDiminished) then
        Just HalfDiminished

    else if containsAll (minimumIntervalsForJazzChordQuality Dominant7) then
        Just Dominant7

    else if containsAll (minimumIntervalsForJazzChordQuality Dominant7Sus) then
        Just Dominant7Sus

    else if containsAll (minimumIntervalsForJazzChordQuality Diminished7) then
        Just Diminished7

    else
        Nothing


minimumIntervalsForJazzChordQuality : JazzChordQuality -> List Interval.Interval
minimumIntervalsForJazzChordQuality jazzChordQuality =
    case jazzChordQuality of
        Major6 ->
            [ Interval.majorThird
            , Interval.perfectFifth
            , Interval.majorSixth
            ]

        Major7 ->
            [ Interval.majorThird
            , Interval.perfectFifth
            , Interval.majorSeventh
            ]

        Minor6 ->
            [ Interval.minorThird
            , Interval.perfectFifth
            , Interval.majorSixth
            ]

        Minor7 ->
            [ Interval.minorThird
            , Interval.perfectFifth
            , Interval.minorSeventh
            ]

        HalfDiminished ->
            [ Interval.minorThird
            , Interval.diminishedFifth
            , Interval.minorSeventh
            ]

        Dominant7 ->
            [ Interval.majorThird
            , Interval.minorSeventh
            ]

        Dominant7Sus ->
            [ Interval.perfectFourth
            , Interval.minorSeventh
            ]

        Diminished7 ->
            [ Interval.minorThird
            , Interval.diminishedFifth
            , Interval.diminishedSeventh
            ]


type JazzChordQuality
    = Major6
    | Major7
    | Minor6
    | Minor7
    | HalfDiminished
    | Dominant7
    | Dominant7Sus
    | Diminished7


type alias AvailablePitchClasses =
    { root :
        { true : PitchClass.PitchClass
        , substitutes : List PitchClass.PitchClass
        }
    , third :
        { true : PitchClass.PitchClass
        , substitutes : List PitchClass.PitchClass
        }
    , fifth :
        { true : PitchClass.PitchClass
        , substitutes : List PitchClass.PitchClass
        }
    , seventh :
        { true : PitchClass.PitchClass
        , substitutes : List PitchClass.PitchClass
        }
    }
