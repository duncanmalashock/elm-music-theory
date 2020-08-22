module MusicTheory.Internal.Voicing.FivePart.Jazz exposing
    ( close
    , drop2
    , drop2and4
    , drop3
    , spread
    )

import List.Extra
import MusicTheory.Internal.Chord as Chord
import MusicTheory.Internal.ChordType as ChordType
import MusicTheory.Internal.Interval as Interval
import MusicTheory.Internal.Octave as Octave
import MusicTheory.Internal.Voicing as Voicing
import MusicTheory.Internal.Voicing.FivePart as FivePart
import MusicTheory.Internal.VoicingClass as VoicingClass


close : Voicing.TechniqueInput FivePart.Ranges -> List FivePart.Voicing
close { ranges, chord } =
    case availables (Chord.chordType chord) of
        Just available ->
            voicingClassesforAllOctaves allCloseVoicingClasses chord ranges available

        Nothing ->
            []


drop2 : Voicing.TechniqueInput FivePart.Ranges -> List FivePart.Voicing
drop2 { ranges, chord } =
    case availables (Chord.chordType chord) of
        Just available ->
            voicingClassesforAllOctaves allDrop2VoicingClasses chord ranges available

        Nothing ->
            []


drop3 : Voicing.TechniqueInput FivePart.Ranges -> List FivePart.Voicing
drop3 { ranges, chord } =
    case availables (Chord.chordType chord) of
        Just available ->
            voicingClassesforAllOctaves allDrop3VoicingClasses chord ranges available

        Nothing ->
            []


drop2and4 : Voicing.TechniqueInput FivePart.Ranges -> List FivePart.Voicing
drop2and4 { ranges, chord } =
    case availables (Chord.chordType chord) of
        Just available ->
            voicingClassesforAllOctaves allDrop2and4VoicingClasses chord ranges available

        Nothing ->
            []


spread : Voicing.TechniqueInput FivePart.Ranges -> List FivePart.Voicing
spread { ranges, chord } =
    case availables (Chord.chordType chord) of
        Just available ->
            [ spreadVoicingsMethodOne

            -- TODO: implement methods 2 and 3 for spread voicings
            ]
                |> List.concatMap
                    (\generator ->
                        voicingClassesforAllOctaves generator chord ranges available
                    )

        Nothing ->
            []


allCloseVoicingClasses : AvailableTensions -> List FivePart.VoicingClass
allCloseVoicingClasses available =
    List.concatMap
        mechanicalVoicing
        [ { one = available.root.true :: available.root.substitutes
          , two = available.seventh.true :: available.seventh.substitutes
          , three = available.fifth.true :: available.fifth.substitutes
          , four = available.third.true :: available.third.substitutes
          , five = available.root.true :: available.root.substitutes
          }
        , { one = available.seventh.true :: available.seventh.substitutes
          , two = available.fifth.true :: available.fifth.substitutes
          , three = available.third.true :: available.third.substitutes
          , four = available.root.true :: available.root.substitutes
          , five = available.seventh.true :: available.seventh.substitutes
          }
        , { one = available.fifth.true :: available.fifth.substitutes
          , two = available.third.true :: available.third.substitutes
          , three = available.root.true :: available.root.substitutes
          , four = available.seventh.true :: available.seventh.substitutes
          , five = available.fifth.true :: available.fifth.substitutes
          }
        , { one = available.third.true :: available.third.substitutes
          , two = available.root.true :: available.root.substitutes
          , three = available.seventh.true :: available.seventh.substitutes
          , four = available.fifth.true :: available.fifth.substitutes
          , five = available.third.true :: available.third.substitutes
          }
        ]


allDrop2VoicingClasses : AvailableTensions -> List FivePart.VoicingClass
allDrop2VoicingClasses available =
    List.concatMap
        mechanicalVoicing
        [ { one = available.root.true :: available.root.substitutes
          , two = available.fifth.true :: available.fifth.substitutes
          , three = available.third.true :: available.third.substitutes
          , four = available.seventh.true :: available.seventh.substitutes
          , five = available.root.true :: available.root.substitutes
          }
        , { one = available.seventh.true :: available.seventh.substitutes
          , two = available.third.true :: available.third.substitutes
          , three = available.root.true :: available.root.substitutes
          , four = available.fifth.true :: available.fifth.substitutes
          , five = available.seventh.true :: available.seventh.substitutes
          }
        , { one = available.fifth.true :: available.fifth.substitutes
          , two = available.root.true :: available.root.substitutes
          , three = available.seventh.true :: available.seventh.substitutes
          , four = available.third.true :: available.third.substitutes
          , five = available.fifth.true :: available.fifth.substitutes
          }
        , { one = available.third.true :: available.third.substitutes
          , two = available.seventh.true :: available.seventh.substitutes
          , three = available.fifth.true :: available.fifth.substitutes
          , four = available.root.true :: available.root.substitutes
          , five = available.third.true :: available.third.substitutes
          }
        ]


allDrop2and4VoicingClasses : AvailableTensions -> List FivePart.VoicingClass
allDrop2and4VoicingClasses available =
    List.concatMap
        mechanicalVoicing
        [ { one = available.root.true :: available.root.substitutes
          , two = available.fifth.true :: available.fifth.substitutes
          , three = available.seventh.true :: available.seventh.substitutes
          , four = available.third.true :: available.third.substitutes
          , five = available.root.true :: available.root.substitutes
          }
        , { one = available.seventh.true :: available.seventh.substitutes
          , two = available.third.true :: available.third.substitutes
          , three = available.fifth.true :: available.fifth.substitutes
          , four = available.root.true :: available.root.substitutes
          , five = available.seventh.true :: available.seventh.substitutes
          }
        , { one = available.fifth.true :: available.fifth.substitutes
          , two = available.root.true :: available.root.substitutes
          , three = available.third.true :: available.third.substitutes
          , four = available.seventh.true :: available.seventh.substitutes
          , five = available.fifth.true :: available.fifth.substitutes
          }
        , { one = available.third.true :: available.third.substitutes
          , two = available.seventh.true :: available.seventh.substitutes
          , three = available.root.true :: available.root.substitutes
          , four = available.fifth.true :: available.fifth.substitutes
          , five = available.third.true :: available.third.substitutes
          }
        ]


allDrop3VoicingClasses : AvailableTensions -> List FivePart.VoicingClass
allDrop3VoicingClasses available =
    List.concatMap
        mechanicalVoicing
        [ { one = available.root.true :: available.root.substitutes
          , two = available.seventh.true :: available.seventh.substitutes
          , three = available.third.true :: available.third.substitutes
          , four = available.fifth.true :: available.fifth.substitutes
          , five = available.root.true :: available.root.substitutes
          }
        , { one = available.seventh.true :: available.seventh.substitutes
          , two = available.fifth.true :: available.fifth.substitutes
          , three = available.root.true :: available.root.substitutes
          , four = available.third.true :: available.third.substitutes
          , five = available.seventh.true :: available.seventh.substitutes
          }
        , { one = available.fifth.true :: available.fifth.substitutes
          , two = available.third.true :: available.third.substitutes
          , three = available.seventh.true :: available.seventh.substitutes
          , four = available.root.true :: available.root.substitutes
          , five = available.fifth.true :: available.fifth.substitutes
          }
        , { one = available.third.true :: available.third.substitutes
          , two = available.root.true :: available.root.substitutes
          , three = available.fifth.true :: available.fifth.substitutes
          , four = available.seventh.true :: available.seventh.substitutes
          , five = available.third.true :: available.third.substitutes
          }
        ]


mechanicalVoicing :
    { a
        | one : List Interval.Interval
        , two : List Interval.Interval
        , three : List Interval.Interval
        , four : List Interval.Interval
        , five : List Interval.Interval
    }
    -> List FivePart.VoicingClass
mechanicalVoicing { one, two, three, four, five } =
    VoicingClass.builder FivePart.VoicingClass
        |> VoicingClass.withFactorFrom one { mustBeUnique = False }
        |> VoicingClass.withFactorFrom two { mustBeUnique = False }
        |> VoicingClass.withFactorFrom three { mustBeUnique = False }
        |> VoicingClass.withFactorFrom four { mustBeUnique = False }
        |> VoicingClass.withFactorFrom five { mustBeUnique = False }
        |> VoicingClass.execute
            { placeFactors = FivePart.placeFactors intervalLimits }


spreadVoicingsMethodOne : AvailableTensions -> List FivePart.VoicingClass
spreadVoicingsMethodOne available =
    VoicingClass.builder FivePart.VoicingClass
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
            { placeFactors = FivePart.placeFactors intervalLimits }


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
    , fiveToFour =
        { max = Interval.perfectOctave
        , min = Interval.augmentedUnison
        }
    }


voicingClassesforAllOctaves :
    (AvailableTensions
     -> List FivePart.VoicingClass
    )
    -> Chord.Chord
    -> FivePart.Ranges
    -> AvailableTensions
    -> List (Voicing.Voicing FivePart.VoicingClass)
voicingClassesforAllOctaves generate chord ranges available =
    List.concatMap
        (\oct ->
            List.map
                (\class -> Voicing.voicing chord oct class)
                (generate available)
        )
        Octave.allValid
        |> List.filter (Voicing.withInstrumentRanges FivePart.allVoices FivePart.allRanges ranges)
        |> List.Extra.uniqueBy (Voicing.toString FivePart.allVoices)



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


availables : ChordType.ChordType -> Maybe AvailableTensions
availables chordType =
    let
        maybeQuality =
            determineJazzChordQuality chordType

        maybeRoot =
            Maybe.andThen (chordToneForClass chordType Root) maybeQuality

        maybeThird =
            Maybe.andThen (chordToneForClass chordType Third) maybeQuality

        maybeFifth =
            Maybe.andThen (chordToneForClass chordType Fifth) maybeQuality

        maybeSeventh =
            Maybe.andThen (chordToneForClass chordType Seventh) maybeQuality

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
                availableTensionsForChordQuality chordType voiceCategory chordQuality
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
    ChordType.ChordType
    -> VoiceCategory
    -> JazzChordQuality
    -> List Interval.Interval
availableTensionsForChordQuality chordType voiceCategory jazzChordQuality =
    let
        chordIntervals =
            ChordType.toIntervals chordType

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


determineJazzChordQuality : ChordType.ChordType -> Maybe JazzChordQuality
determineJazzChordQuality chordType =
    let
        intervals =
            ChordType.toIntervals chordType

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
    ChordType.ChordType
    -> VoiceCategory
    -> JazzChordQuality
    -> Maybe Interval.Interval
chordToneForClass chordType voiceCategory jazzChordQuality =
    let
        intervals =
            ChordType.toIntervals chordType

        takeFirst available =
            List.filter (\item -> List.member item intervals) available
                |> List.head
    in
    takeFirst (availableTensionsForChordQuality chordType voiceCategory jazzChordQuality)
