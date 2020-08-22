module Internal.Voicing.FourPart.Jazz exposing
    ( close
    , drop2
    , drop2and4
    , drop3
    , spread
    )

import Internal.Chord as Chord
import Internal.ChordClass as ChordClass
import Internal.Interval as Interval
import Internal.Octave as Octave
import Internal.Voicing as Voicing
import Internal.Voicing.FourPart as FourPart
import Internal.VoicingClass as VoicingClass
import List.Extra


close : Voicing.TechniqueInput FourPart.Ranges -> List FourPart.Voicing
close { ranges, chord } =
    case availables (Chord.chordClass chord) of
        Just available ->
            voicingClassesforAllOctaves allCloseVoicingClasses chord ranges available

        Nothing ->
            []


drop2 : Voicing.TechniqueInput FourPart.Ranges -> List FourPart.Voicing
drop2 { ranges, chord } =
    case availables (Chord.chordClass chord) of
        Just available ->
            voicingClassesforAllOctaves allDrop2VoicingClasses chord ranges available

        Nothing ->
            []


drop3 : Voicing.TechniqueInput FourPart.Ranges -> List FourPart.Voicing
drop3 { ranges, chord } =
    case availables (Chord.chordClass chord) of
        Just available ->
            voicingClassesforAllOctaves allDrop3VoicingClasses chord ranges available

        Nothing ->
            []


drop2and4 : Voicing.TechniqueInput FourPart.Ranges -> List FourPart.Voicing
drop2and4 { ranges, chord } =
    case availables (Chord.chordClass chord) of
        Just available ->
            voicingClassesforAllOctaves allDrop2and4VoicingClasses chord ranges available

        Nothing ->
            []


spread : Voicing.TechniqueInput FourPart.Ranges -> List FourPart.Voicing
spread { ranges, chord } =
    case availables (Chord.chordClass chord) of
        Just available ->
            voicingClassesforAllOctaves spreadVoicings chord ranges available

        Nothing ->
            []


allCloseVoicingClasses : AvailableTensions -> List FourPart.VoicingClass
allCloseVoicingClasses available =
    List.concatMap
        mechanicalVoicing
        [ { one = available.root.true :: available.root.substitutes
          , two = available.seventh.true :: available.seventh.substitutes
          , three = available.fifth.true :: available.fifth.substitutes
          , four = available.third.true :: available.third.substitutes
          }
        , { one = available.seventh.true :: available.seventh.substitutes
          , two = available.fifth.true :: available.fifth.substitutes
          , three = available.third.true :: available.third.substitutes
          , four = available.root.true :: available.root.substitutes
          }
        , { one = available.fifth.true :: available.fifth.substitutes
          , two = available.third.true :: available.third.substitutes
          , three = available.root.true :: available.root.substitutes
          , four = available.seventh.true :: available.seventh.substitutes
          }
        , { one = available.third.true :: available.third.substitutes
          , two = available.root.true :: available.root.substitutes
          , three = available.seventh.true :: available.seventh.substitutes
          , four = available.fifth.true :: available.fifth.substitutes
          }
        ]


allDrop2VoicingClasses : AvailableTensions -> List FourPart.VoicingClass
allDrop2VoicingClasses available =
    List.concatMap
        mechanicalVoicing
        [ { one = available.root.true :: available.root.substitutes
          , two = available.fifth.true :: available.fifth.substitutes
          , three = available.third.true :: available.third.substitutes
          , four = available.seventh.true :: available.seventh.substitutes
          }
        , { one = available.seventh.true :: available.seventh.substitutes
          , two = available.third.true :: available.third.substitutes
          , three = available.root.true :: available.root.substitutes
          , four = available.fifth.true :: available.fifth.substitutes
          }
        , { one = available.fifth.true :: available.fifth.substitutes
          , two = available.root.true :: available.root.substitutes
          , three = available.seventh.true :: available.seventh.substitutes
          , four = available.third.true :: available.third.substitutes
          }
        , { one = available.third.true :: available.third.substitutes
          , two = available.seventh.true :: available.seventh.substitutes
          , three = available.fifth.true :: available.fifth.substitutes
          , four = available.root.true :: available.root.substitutes
          }
        ]


allDrop2and4VoicingClasses : AvailableTensions -> List FourPart.VoicingClass
allDrop2and4VoicingClasses available =
    List.concatMap
        mechanicalVoicing
        [ { one = available.root.true :: available.root.substitutes
          , two = available.fifth.true :: available.fifth.substitutes
          , three = available.seventh.true :: available.seventh.substitutes
          , four = available.third.true :: available.third.substitutes
          }
        , { one = available.seventh.true :: available.seventh.substitutes
          , two = available.third.true :: available.third.substitutes
          , three = available.fifth.true :: available.fifth.substitutes
          , four = available.root.true :: available.root.substitutes
          }
        , { one = available.fifth.true :: available.fifth.substitutes
          , two = available.root.true :: available.root.substitutes
          , three = available.third.true :: available.third.substitutes
          , four = available.seventh.true :: available.seventh.substitutes
          }
        , { one = available.third.true :: available.third.substitutes
          , two = available.seventh.true :: available.seventh.substitutes
          , three = available.root.true :: available.root.substitutes
          , four = available.fifth.true :: available.fifth.substitutes
          }
        ]


allDrop3VoicingClasses : AvailableTensions -> List FourPart.VoicingClass
allDrop3VoicingClasses available =
    List.concatMap
        mechanicalVoicing
        [ { one = available.root.true :: available.root.substitutes
          , two = available.seventh.true :: available.seventh.substitutes
          , three = available.third.true :: available.third.substitutes
          , four = available.fifth.true :: available.fifth.substitutes
          }
        , { one = available.seventh.true :: available.seventh.substitutes
          , two = available.fifth.true :: available.fifth.substitutes
          , three = available.root.true :: available.root.substitutes
          , four = available.third.true :: available.third.substitutes
          }
        , { one = available.fifth.true :: available.fifth.substitutes
          , two = available.third.true :: available.third.substitutes
          , three = available.seventh.true :: available.seventh.substitutes
          , four = available.root.true :: available.root.substitutes
          }
        , { one = available.third.true :: available.third.substitutes
          , two = available.root.true :: available.root.substitutes
          , three = available.fifth.true :: available.fifth.substitutes
          , four = available.seventh.true :: available.seventh.substitutes
          }
        ]


mechanicalVoicing :
    { a
        | one : List Interval.Interval
        , two : List Interval.Interval
        , three : List Interval.Interval
        , four : List Interval.Interval
    }
    -> List FourPart.VoicingClass
mechanicalVoicing { one, two, three, four } =
    VoicingClass.builder FourPart.VoicingClass
        |> VoicingClass.withFactorFrom one { mustBeUnique = False }
        |> VoicingClass.withFactorFrom two { mustBeUnique = False }
        |> VoicingClass.withFactorFrom three { mustBeUnique = False }
        |> VoicingClass.withFactorFrom four { mustBeUnique = False }
        |> VoicingClass.execute
            { placeFactors = FourPart.placeFactors intervalLimits }


spreadVoicings : AvailableTensions -> List FourPart.VoicingClass
spreadVoicings available =
    VoicingClass.builder FourPart.VoicingClass
        |> VoicingClass.withFactorFrom
            (List.concat
                [ available.root.substitutes
                , available.third.substitutes
                , available.fifth.true :: available.fifth.substitutes
                , available.seventh.substitutes
                ]
            )
            { mustBeUnique = True }
        |> VoicingClass.withFactorFrom
            [ available.third.true
            , available.fifth.true
            , available.seventh.true
            ]
            { mustBeUnique = True }
        |> VoicingClass.withFactorFrom
            [ available.third.true
            , available.seventh.true
            ]
            { mustBeUnique = True }
        |> VoicingClass.withFactor available.root.true { mustBeUnique = True }
        |> VoicingClass.execute
            { placeFactors = FourPart.placeFactors intervalLimits }


intervalLimits =
    { twoToOne =
        { max = Interval.perfectOctave
        , min = Interval.augmentedUnison
        }
    , threeToTwo =
        { max = Interval.perfectOctave
        , min = Interval.augmentedUnison
        }
    , fourToThree =
        { max = Interval.perfectOctave
        , min = Interval.augmentedUnison
        }
    }


voicingClassesforAllOctaves :
    (AvailableTensions
     -> List FourPart.VoicingClass
    )
    -> Chord.Chord
    -> FourPart.Ranges
    -> AvailableTensions
    -> List (Voicing.Voicing FourPart.VoicingClass)
voicingClassesforAllOctaves generate chord ranges available =
    List.concatMap
        (\oct ->
            List.map
                (\class -> Voicing.voicing chord oct class)
                (generate available)
        )
        Octave.allValid
        |> List.filter (Voicing.withInstrumentRanges FourPart.allVoices FourPart.allRanges ranges)
        |> List.Extra.uniqueBy (Voicing.toString FourPart.allVoices)



-- Analysis


type VoiceCategory
    = Root
    | Third
    | Fifth
    | Seventh


type JazzChordQuality
    = Major6
    | Major7
    | Minor6
    | Minor7
    | HalfDiminished
    | Dominant7
    | Dominant7Sus
    | Diminished7


type alias AvailableTensions =
    { root :
        { true : Interval.Interval
        , substitutes : List Interval.Interval
        }
    , third :
        { true : Interval.Interval
        , substitutes : List Interval.Interval
        }
    , fifth :
        { true : Interval.Interval
        , substitutes : List Interval.Interval
        }
    , seventh :
        { true : Interval.Interval
        , substitutes : List Interval.Interval
        }
    }


availables : ChordClass.ChordClass -> Maybe AvailableTensions
availables chordClass =
    let
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

        chordToneWithSubstitutes :
            VoiceCategory
            -> Interval.Interval
            -> JazzChordQuality
            ->
                { true : Interval.Interval
                , substitutes : List Interval.Interval
                }
        chordToneWithSubstitutes voiceCategory chordTone chordQuality =
            { true = chordTone
            , substitutes =
                availableTensionsForChordQuality chordClass voiceCategory chordQuality
                    |> List.filter ((==) chordTone >> not)
            }

        toAvailables :
            JazzChordQuality
            -> Interval.Interval
            -> Interval.Interval
            -> Interval.Interval
            -> Interval.Interval
            -> AvailableTensions
        toAvailables chordQuality root third fifth seventh =
            { root = chordToneWithSubstitutes Root root chordQuality
            , third = chordToneWithSubstitutes Third third chordQuality
            , fifth = chordToneWithSubstitutes Fifth fifth chordQuality
            , seventh = chordToneWithSubstitutes Seventh seventh chordQuality
            }
    in
    case maybeQuality of
        Nothing ->
            Nothing

        Just quality ->
            Maybe.map4
                (toAvailables quality)
                maybeRoot
                maybeThird
                maybeFifth
                maybeSeventh


availableTensionsForChordQuality :
    ChordClass.ChordClass
    -> VoiceCategory
    -> JazzChordQuality
    -> List Interval.Interval
availableTensionsForChordQuality chordClass voiceCategory jazzChordQuality =
    let
        chordIntervals =
            ChordClass.toIntervals chordClass

        includesAll intervals =
            List.map
                (\interval -> List.member interval chordIntervals)
                intervals
                |> List.all identity
    in
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
                    ]

        Minor7 ->
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
                    [ Interval.minorSeventh
                    ]

        HalfDiminished ->
            case voiceCategory of
                Root ->
                    [ Interval.perfectUnison
                    , Interval.majorNinth
                    , Interval.perfectEleventh
                    ]

                Third ->
                    [ Interval.minorThird
                    ]

                Fifth ->
                    [ Interval.diminishedFifth
                    , Interval.minorThirteenth
                    ]

                Seventh ->
                    [ Interval.minorSeventh
                    ]

        Dominant7 ->
            case voiceCategory of
                Root ->
                    Interval.perfectUnison
                        :: (if includesAll [ Interval.majorNinth ] then
                                [ Interval.majorNinth
                                ]

                            else if includesAll [ Interval.minorNinth ] then
                                [ Interval.minorNinth
                                , Interval.augmentedNinth
                                ]

                            else if includesAll [ Interval.augmentedNinth ] then
                                [ Interval.augmentedNinth
                                , Interval.minorNinth
                                ]

                            else
                                [ Interval.majorNinth ]
                           )

                Third ->
                    [ Interval.majorThird
                    , Interval.perfectFourth
                    ]

                Fifth ->
                    if includesAll [ Interval.augmentedEleventh, Interval.minorThirteenth ] then
                        [ Interval.augmentedEleventh
                        , Interval.minorThirteenth
                        ]

                    else if includesAll [ Interval.augmentedEleventh, Interval.majorThirteenth ] then
                        [ Interval.augmentedEleventh
                        , Interval.majorThirteenth
                        ]

                    else if includesAll [ Interval.augmentedEleventh ] then
                        [ Interval.augmentedEleventh
                        , Interval.minorThirteenth
                        ]

                    else if includesAll [ Interval.minorThirteenth ] then
                        [ Interval.minorThirteenth
                        , Interval.augmentedEleventh
                        ]

                    else
                        [ Interval.perfectFifth
                        , Interval.majorThirteenth
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


chordToneForClass :
    ChordClass.ChordClass
    -> VoiceCategory
    -> JazzChordQuality
    -> Maybe Interval.Interval
chordToneForClass chordClass voiceCategory jazzChordQuality =
    let
        intervals =
            ChordClass.toIntervals chordClass

        takeFirst available =
            List.filter (\item -> List.member item intervals) available
                |> List.head
    in
    takeFirst (availableTensionsForChordQuality chordClass voiceCategory jazzChordQuality)
