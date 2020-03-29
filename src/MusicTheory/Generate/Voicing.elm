module MusicTheory.Generate.Voicing exposing
    ( Error(..)
    , FivePartVoicing
    , FourPartVoicing
    , ThreePartVoicing
    , fiveWayDrop2
    , fiveWayDrop2and4
    , fiveWayDrop3
    , fiveWaySpread
    , fourWayClose
    , fourWayCloseDoubleLead
    , fourWayDrop2
    , fourWayDrop2and4
    , fourWayDrop3
    , fourWaySpread
    , nextVoiceCategory
    , substituteDoubleLead
    )

import MusicTheory.Analyze.Chord as AnalyzeChord
    exposing
        ( AvailablePitchClasses
        , VoiceCategory(..)
        )
import MusicTheory.Interval as Interval
import MusicTheory.Pitch as Pitch exposing (Pitch)
import MusicTheory.PitchClass as PitchClass


type Error
    = CouldNotCompleteVoicing (List (Maybe Pitch.Pitch))
    | VoiceCategoriesWereUndefined (List (Maybe VoiceCategory))
    | AnalyzeChordError AnalyzeChord.Error


type alias ThreePartVoicing =
    { voiceOne : Pitch.Pitch
    , voiceTwo : Pitch.Pitch
    , voiceThree : Pitch.Pitch
    }


type alias FourPartVoicing =
    { voiceOne : Pitch.Pitch
    , voiceTwo : Pitch.Pitch
    , voiceThree : Pitch.Pitch
    , voiceFour : Pitch.Pitch
    }


type alias FivePartVoicing =
    { voiceOne : Pitch.Pitch
    , voiceTwo : Pitch.Pitch
    , voiceThree : Pitch.Pitch
    , voiceFour : Pitch.Pitch
    , voiceFive : Pitch.Pitch
    }


type Step
    = AssignToVoice Int Pitch.Pitch
    | AssignFirstBelow Int (List VoiceSelection)
    | AssignFirstAbove Int (List VoiceSelection)
    | CopyVoice Int Int
    | DropVoiceByOctave Int


type VoiceSelection
    = VoiceSelection VoiceCategory VoiceSpecificity


type VoiceSpecificity
    = ChordTone
    | SubstituteTone
    | ChordToneOrSubstitute


chordToneOnly : VoiceCategory -> VoiceSelection
chordToneOnly voiceCategory =
    VoiceSelection voiceCategory ChordTone


substituteToneOnly : VoiceCategory -> VoiceSelection
substituteToneOnly voiceCategory =
    VoiceSelection voiceCategory SubstituteTone


chordToneOrSubstitute : VoiceCategory -> VoiceSelection
chordToneOrSubstitute voiceCategory =
    VoiceSelection voiceCategory ChordToneOrSubstitute


pitchClassesFromVoiceSelections :
    List VoiceSelection
    -> AvailablePitchClasses
    -> List PitchClass.PitchClass
pitchClassesFromVoiceSelections voiceSelections availables =
    let
        pitchClassesFromVoiceSelection :
            VoiceSelection
            -> List PitchClass.PitchClass
        pitchClassesFromVoiceSelection (VoiceSelection voiceCategory voiceSpecificity) =
            case voiceSpecificity of
                ChordTone ->
                    case voiceCategory of
                        Root ->
                            [ availables.root.true ]

                        Seventh ->
                            [ availables.seventh.true ]

                        Fifth ->
                            [ availables.fifth.true ]

                        Third ->
                            [ availables.third.true ]

                SubstituteTone ->
                    case voiceCategory of
                        Root ->
                            availables.root.substitutes

                        Seventh ->
                            availables.seventh.substitutes

                        Fifth ->
                            availables.fifth.substitutes

                        Third ->
                            availables.third.substitutes

                ChordToneOrSubstitute ->
                    case voiceCategory of
                        Root ->
                            availables.root.true
                                :: availables.root.substitutes

                        Seventh ->
                            availables.seventh.true
                                :: availables.seventh.substitutes

                        Fifth ->
                            availables.fifth.true
                                :: availables.fifth.substitutes

                        Third ->
                            availables.third.true
                                :: availables.third.substitutes
    in
    List.concatMap pitchClassesFromVoiceSelection voiceSelections


usePitchClassFromVoiceSelections :
    List VoiceSelection
    -> AvailablePitchClasses
    -> List PitchClass.PitchClass
    -> Maybe PitchClass.PitchClass
usePitchClassFromVoiceSelections voiceSelections availables used =
    let
        chooseFirstUnused list =
            List.filter (\item -> List.member item used |> not) list
                |> List.head
    in
    -- TODO Choosing tones is not parameterized, we just take the head of the list
    pitchClassesFromVoiceSelections voiceSelections availables
        |> chooseFirstUnused


determineVoiceCategory :
    PitchClass.PitchClass
    -> AvailablePitchClasses
    -> Maybe VoiceCategory
determineVoiceCategory pitchClass availablePitchClasses =
    let
        rootPitchClasses =
            availablePitchClasses.root.true
                :: availablePitchClasses.root.substitutes

        thirdPitchClasses =
            availablePitchClasses.third.true
                :: availablePitchClasses.third.substitutes

        fifthPitchClasses =
            availablePitchClasses.fifth.true
                :: availablePitchClasses.fifth.substitutes

        seventhPitchClasses =
            availablePitchClasses.seventh.true
                :: availablePitchClasses.seventh.substitutes
    in
    if List.member pitchClass rootPitchClasses then
        Just Root

    else if List.member pitchClass thirdPitchClasses then
        Just Third

    else if List.member pitchClass fifthPitchClasses then
        Just Fifth

    else if List.member pitchClass seventhPitchClasses then
        Just Seventh

    else
        Nothing


nextVoiceCategory : VoiceCategory -> VoiceCategory
nextVoiceCategory voiceCategory =
    case voiceCategory of
        Root ->
            Seventh

        Seventh ->
            Fifth

        Fifth ->
            Third

        Third ->
            Root


fourWayClose :
    Pitch.Pitch
    -> AvailablePitchClasses
    -> Result Error FourPartVoicing
fourWayClose leadVoice availablePitchClasses =
    let
        maybeFirstVoiceCategory =
            determineVoiceCategory
                (Pitch.pitchClass leadVoice)
                availablePitchClasses

        maybeSecondVoiceCategory =
            Maybe.map nextVoiceCategory maybeFirstVoiceCategory

        maybeThirdVoiceCategory =
            Maybe.map nextVoiceCategory maybeSecondVoiceCategory

        maybeFourthVoiceCategory =
            Maybe.map nextVoiceCategory maybeThirdVoiceCategory

        maybeVoicing =
            Maybe.map3
                (\secondVoiceCategory thirdVoiceCategory fourthVoiceCategory ->
                    List.foldl
                        applyStepFourPart
                        (fourPartInit availablePitchClasses)
                        [ AssignToVoice 1 leadVoice
                        , AssignFirstBelow 1
                            [ chordToneOrSubstitute secondVoiceCategory ]
                        , AssignFirstBelow 2
                            [ chordToneOrSubstitute thirdVoiceCategory ]
                        , AssignFirstBelow 3
                            [ chordToneOrSubstitute fourthVoiceCategory ]
                        ]
                )
                maybeSecondVoiceCategory
                maybeThirdVoiceCategory
                maybeFourthVoiceCategory
    in
    case maybeVoicing of
        Just voicing ->
            voicing
                |> completeFourPart

        Nothing ->
            Err <|
                VoiceCategoriesWereUndefined
                    [ maybeFirstVoiceCategory
                    , maybeSecondVoiceCategory
                    , maybeThirdVoiceCategory
                    , maybeFourthVoiceCategory
                    ]


fourWayCloseDoubleLead :
    Pitch.Pitch
    -> AvailablePitchClasses
    -> Result Error FivePartVoicing
fourWayCloseDoubleLead leadVoice availablePitchClasses =
    let
        maybeFirstVoiceCategory =
            determineVoiceCategory
                (Pitch.pitchClass leadVoice)
                availablePitchClasses

        maybeSecondVoiceCategory =
            Maybe.map nextVoiceCategory maybeFirstVoiceCategory

        maybeThirdVoiceCategory =
            Maybe.map nextVoiceCategory maybeSecondVoiceCategory

        maybeFourthVoiceCategory =
            Maybe.map nextVoiceCategory maybeThirdVoiceCategory

        maybeVoicing =
            Maybe.map3
                (\secondVoiceCategory thirdVoiceCategory fourthVoiceCategory ->
                    List.foldl
                        applyStepFivePart
                        (fivePartInit availablePitchClasses)
                        [ AssignToVoice 1 leadVoice
                        , AssignFirstBelow 1
                            [ chordToneOrSubstitute secondVoiceCategory ]
                        , AssignFirstBelow 2
                            [ chordToneOrSubstitute thirdVoiceCategory ]
                        , AssignFirstBelow 3
                            [ chordToneOrSubstitute fourthVoiceCategory ]
                        , CopyVoice 1 5
                        , DropVoiceByOctave 5
                        ]
                )
                maybeSecondVoiceCategory
                maybeThirdVoiceCategory
                maybeFourthVoiceCategory
    in
    case maybeVoicing of
        Just voicing ->
            voicing
                |> completeFivePart

        Nothing ->
            Err <|
                VoiceCategoriesWereUndefined
                    [ maybeFirstVoiceCategory
                    , maybeSecondVoiceCategory
                    , maybeThirdVoiceCategory
                    , maybeFourthVoiceCategory
                    ]


fourWaySpread :
    Pitch.Pitch
    -> AvailablePitchClasses
    -> Result Error FourPartVoicing
fourWaySpread bass availablePitchClasses =
    let
        voicing =
            List.foldl
                applyStepFourPart
                (fourPartInit availablePitchClasses)
                [ AssignToVoice 4 bass
                , AssignFirstAbove 4
                    [ chordToneOnly Third
                    , chordToneOnly Seventh
                    ]
                , AssignFirstAbove 3
                    [ chordToneOnly Third
                    , chordToneOnly Seventh
                    ]
                , AssignFirstAbove 2
                    [ substituteToneOnly Root
                    , chordToneOrSubstitute Fifth
                    , substituteToneOnly Seventh
                    , substituteToneOnly Third
                    ]
                ]
    in
    voicing
        |> completeFourPart


fiveWaySpread :
    Pitch.Pitch
    -> AvailablePitchClasses
    -> Result Error FivePartVoicing
fiveWaySpread bass availablePitchClasses =
    let
        voicing =
            List.foldl
                applyStepFivePart
                (fivePartInit availablePitchClasses)
                [ AssignToVoice 5 bass
                , AssignFirstAbove 5
                    [ chordToneOnly Third
                    , chordToneOnly Seventh
                    ]
                , AssignFirstAbove 4
                    [ chordToneOnly Third
                    , chordToneOnly Seventh
                    ]
                , AssignFirstAbove 3
                    [ substituteToneOnly Root
                    , chordToneOrSubstitute Fifth
                    , substituteToneOnly Seventh
                    , substituteToneOnly Third
                    ]
                , AssignFirstAbove 2
                    [ chordToneOrSubstitute Fifth
                    , substituteToneOnly Root
                    , substituteToneOnly Seventh
                    , substituteToneOnly Third
                    ]
                ]
    in
    voicing
        |> completeFivePart


fourWayDrop2 : AvailablePitchClasses -> FourPartVoicing -> Result Error FourPartVoicing
fourWayDrop2 availablePitchClasses { voiceOne, voiceTwo, voiceThree, voiceFour } =
    { voiceOne = Just voiceOne
    , voiceTwo = Just voiceThree
    , voiceThree = Just voiceFour
    , voiceFour =
        Pitch.transposeDown Interval.perfectOctave voiceTwo
            |> Result.toMaybe
    , used = []
    , availablePitchClasses = availablePitchClasses
    }
        |> completeFourPart


fourWayDrop3 : AvailablePitchClasses -> FourPartVoicing -> Result Error FourPartVoicing
fourWayDrop3 availablePitchClasses { voiceOne, voiceTwo, voiceThree, voiceFour } =
    { voiceOne = Just voiceOne
    , voiceTwo = Just voiceTwo
    , voiceThree = Just voiceFour
    , voiceFour =
        Pitch.transposeDown Interval.perfectOctave voiceThree
            |> Result.toMaybe
    , used = []
    , availablePitchClasses = availablePitchClasses
    }
        |> completeFourPart


fourWayDrop2and4 : AvailablePitchClasses -> FourPartVoicing -> Result Error FourPartVoicing
fourWayDrop2and4 availablePitchClasses { voiceOne, voiceTwo, voiceThree, voiceFour } =
    { voiceOne = Just voiceOne
    , voiceTwo = Just voiceThree
    , voiceThree =
        Pitch.transposeDown Interval.perfectOctave voiceTwo
            |> Result.toMaybe
    , voiceFour =
        Pitch.transposeDown Interval.perfectOctave voiceFour
            |> Result.toMaybe
    , used = []
    , availablePitchClasses = availablePitchClasses
    }
        |> completeFourPart


substituteDoubleLead :
    Pitch.Pitch
    -> AvailablePitchClasses
    -> Result Error FivePartVoicing
substituteDoubleLead leadVoice availablePitchClasses =
    let
        maybeFirstVoiceCategory =
            determineVoiceCategory
                (Pitch.pitchClass leadVoice)
                availablePitchClasses

        maybeSecondVoiceCategory =
            Maybe.map nextVoiceCategory maybeFirstVoiceCategory

        maybeThirdVoiceCategory =
            Maybe.map nextVoiceCategory maybeSecondVoiceCategory

        maybeFourthVoiceCategory =
            Maybe.map nextVoiceCategory maybeThirdVoiceCategory

        maybeVoicing =
            Maybe.map4
                (\firstVoiceCategory secondVoiceCategory thirdVoiceCategory fourthVoiceCategory ->
                    List.foldl
                        applyStepFivePart
                        (fivePartInit availablePitchClasses)
                        [ AssignToVoice 1 leadVoice
                        , AssignFirstBelow 1
                            [ chordToneOrSubstitute secondVoiceCategory ]
                        , AssignFirstBelow 2
                            [ chordToneOrSubstitute thirdVoiceCategory ]
                        , AssignFirstBelow 3
                            [ chordToneOrSubstitute fourthVoiceCategory ]
                        , AssignFirstBelow 4
                            [ chordToneOrSubstitute firstVoiceCategory
                            , chordToneOrSubstitute secondVoiceCategory
                            , chordToneOrSubstitute thirdVoiceCategory
                            , chordToneOrSubstitute fourthVoiceCategory
                            ]
                        ]
                )
                maybeFirstVoiceCategory
                maybeSecondVoiceCategory
                maybeThirdVoiceCategory
                maybeFourthVoiceCategory
    in
    case maybeVoicing of
        Just voicing ->
            voicing
                |> completeFivePart

        Nothing ->
            Err <|
                VoiceCategoriesWereUndefined
                    [ maybeFirstVoiceCategory
                    , maybeSecondVoiceCategory
                    , maybeThirdVoiceCategory
                    , maybeFourthVoiceCategory
                    ]


fiveWayDrop2 : AvailablePitchClasses -> FivePartVoicing -> Result Error FivePartVoicing
fiveWayDrop2 availablePitchClasses { voiceOne, voiceTwo, voiceThree, voiceFour, voiceFive } =
    { voiceOne = Just voiceOne
    , voiceTwo = Just voiceThree
    , voiceThree = Just voiceFour
    , voiceFour =
        Pitch.transposeDown Interval.perfectOctave voiceTwo
            |> Result.toMaybe
    , voiceFive = Just voiceFive
    , used = []
    , availablePitchClasses = availablePitchClasses
    }
        |> completeFivePart


fiveWayDrop3 : AvailablePitchClasses -> FivePartVoicing -> Result Error FivePartVoicing
fiveWayDrop3 availablePitchClasses { voiceOne, voiceTwo, voiceThree, voiceFour, voiceFive } =
    { voiceOne = Just voiceOne
    , voiceTwo = Just voiceTwo
    , voiceThree = Just voiceFour
    , voiceFour =
        Pitch.transposeDown Interval.perfectOctave voiceThree
            |> Result.toMaybe
    , voiceFive = Just voiceFive
    , used = []
    , availablePitchClasses = availablePitchClasses
    }
        |> completeFivePart


fiveWayDrop2and4 : AvailablePitchClasses -> FivePartVoicing -> Result Error FivePartVoicing
fiveWayDrop2and4 availablePitchClasses { voiceOne, voiceTwo, voiceThree, voiceFour, voiceFive } =
    { voiceOne = Just voiceOne
    , voiceTwo = Just voiceThree
    , voiceThree =
        Pitch.transposeDown Interval.perfectOctave voiceTwo
            |> Result.toMaybe
    , voiceFour =
        Pitch.transposeDown Interval.perfectOctave voiceFour
            |> Result.toMaybe
    , voiceFive = Just voiceFive
    , used = []
    , availablePitchClasses = availablePitchClasses
    }
        |> completeFivePart



-- Four-part voicing utils


type alias FourPartVoicingInProgress =
    { voiceOne : Maybe Pitch.Pitch
    , voiceTwo : Maybe Pitch.Pitch
    , voiceThree : Maybe Pitch.Pitch
    , voiceFour : Maybe Pitch.Pitch
    , availablePitchClasses : AvailablePitchClasses
    , used : List PitchClass.PitchClass
    }


getAtFourPartVoicing : Int -> FourPartVoicingInProgress -> Maybe Pitch.Pitch
getAtFourPartVoicing index voicingInProgress =
    case index of
        1 ->
            voicingInProgress.voiceOne

        2 ->
            voicingInProgress.voiceTwo

        3 ->
            voicingInProgress.voiceThree

        4 ->
            voicingInProgress.voiceFour

        _ ->
            Nothing


setAtFourPartVoicing :
    Int
    -> FourPartVoicingInProgress
    -> Maybe Pitch.Pitch
    -> FourPartVoicingInProgress
setAtFourPartVoicing index voicingInProgress pitchToSet =
    let
        updateUsed currentUsed usedPitch =
            currentUsed
                ++ List.filterMap identity
                    [ Maybe.map Pitch.pitchClass usedPitch
                    ]
    in
    case index of
        1 ->
            { voicingInProgress
                | voiceOne = pitchToSet
                , used =
                    updateUsed voicingInProgress.used pitchToSet
            }

        2 ->
            { voicingInProgress
                | voiceTwo = pitchToSet
                , used =
                    updateUsed voicingInProgress.used pitchToSet
            }

        3 ->
            { voicingInProgress
                | voiceThree = pitchToSet
                , used =
                    updateUsed voicingInProgress.used pitchToSet
            }

        4 ->
            { voicingInProgress
                | voiceFour = pitchToSet
                , used =
                    updateUsed voicingInProgress.used pitchToSet
            }

        _ ->
            voicingInProgress


fourPartInit : AvailablePitchClasses -> FourPartVoicingInProgress
fourPartInit availablePitchClasses =
    { voiceOne = Nothing
    , voiceTwo = Nothing
    , voiceThree = Nothing
    , voiceFour = Nothing
    , availablePitchClasses = availablePitchClasses
    , used = []
    }


completeFourPart : FourPartVoicingInProgress -> Result Error FourPartVoicing
completeFourPart { voiceOne, voiceTwo, voiceThree, voiceFour } =
    Maybe.map4 FourPartVoicing voiceOne voiceTwo voiceThree voiceFour
        |> Result.fromMaybe
            (CouldNotCompleteVoicing
                [ voiceOne
                , voiceTwo
                , voiceThree
                , voiceFour
                ]
            )


applyStepFourPart : Step -> FourPartVoicingInProgress -> FourPartVoicingInProgress
applyStepFourPart step voicingInProgress =
    case step of
        AssignToVoice voice pitch ->
            setAtFourPartVoicing voice voicingInProgress (Just pitch)

        AssignFirstBelow voice voiceSelections ->
            getAtFourPartVoicing voice voicingInProgress
                |> Maybe.map2
                    Pitch.firstBelow
                    (usePitchClassFromVoiceSelections
                        voiceSelections
                        voicingInProgress.availablePitchClasses
                        voicingInProgress.used
                    )
                |> Maybe.andThen Result.toMaybe
                |> setAtFourPartVoicing (voice + 1) voicingInProgress

        AssignFirstAbove voice voiceSelections ->
            getAtFourPartVoicing voice voicingInProgress
                |> Maybe.map2
                    Pitch.firstAbove
                    (usePitchClassFromVoiceSelections
                        voiceSelections
                        voicingInProgress.availablePitchClasses
                        voicingInProgress.used
                    )
                |> Maybe.andThen Result.toMaybe
                |> setAtFourPartVoicing (voice - 1) voicingInProgress

        CopyVoice fromVoice toVoice ->
            let
                voiceToGet =
                    getAtFourPartVoicing fromVoice voicingInProgress
            in
            setAtFourPartVoicing toVoice voicingInProgress voiceToGet

        DropVoiceByOctave voice ->
            getAtFourPartVoicing voice voicingInProgress
                |> Maybe.map (Pitch.transposeDown Interval.perfectOctave)
                |> Maybe.andThen Result.toMaybe
                |> setAtFourPartVoicing voice voicingInProgress



-- Five-part voicing utils


type alias FivePartVoicingInProgress =
    { voiceOne : Maybe Pitch.Pitch
    , voiceTwo : Maybe Pitch.Pitch
    , voiceThree : Maybe Pitch.Pitch
    , voiceFour : Maybe Pitch.Pitch
    , voiceFive : Maybe Pitch.Pitch
    , availablePitchClasses : AvailablePitchClasses
    , used : List PitchClass.PitchClass
    }


getAtFivePartVoicing : Int -> FivePartVoicingInProgress -> Maybe Pitch.Pitch
getAtFivePartVoicing index voicingInProgress =
    case index of
        1 ->
            voicingInProgress.voiceOne

        2 ->
            voicingInProgress.voiceTwo

        3 ->
            voicingInProgress.voiceThree

        4 ->
            voicingInProgress.voiceFour

        5 ->
            voicingInProgress.voiceFive

        _ ->
            Nothing


setAtFivePartVoicing :
    Int
    -> FivePartVoicingInProgress
    -> Maybe Pitch.Pitch
    -> FivePartVoicingInProgress
setAtFivePartVoicing index voicingInProgress pitchToSet =
    let
        updateUsed currentUsed usedPitch =
            currentUsed
                ++ List.filterMap identity
                    [ Maybe.map Pitch.pitchClass usedPitch
                    ]
    in
    case index of
        1 ->
            { voicingInProgress
                | voiceOne = pitchToSet
                , used =
                    updateUsed voicingInProgress.used pitchToSet
            }

        2 ->
            { voicingInProgress
                | voiceTwo = pitchToSet
                , used =
                    updateUsed voicingInProgress.used pitchToSet
            }

        3 ->
            { voicingInProgress
                | voiceThree = pitchToSet
                , used =
                    updateUsed voicingInProgress.used pitchToSet
            }

        4 ->
            { voicingInProgress
                | voiceFour = pitchToSet
                , used =
                    updateUsed voicingInProgress.used pitchToSet
            }

        5 ->
            { voicingInProgress
                | voiceFive = pitchToSet
                , used =
                    updateUsed voicingInProgress.used pitchToSet
            }

        _ ->
            voicingInProgress


fivePartInit : AvailablePitchClasses -> FivePartVoicingInProgress
fivePartInit availablePitchClasses =
    { voiceOne = Nothing
    , voiceTwo = Nothing
    , voiceThree = Nothing
    , voiceFour = Nothing
    , voiceFive = Nothing
    , availablePitchClasses = availablePitchClasses
    , used = []
    }


completeFivePart : FivePartVoicingInProgress -> Result Error FivePartVoicing
completeFivePart { voiceOne, voiceTwo, voiceThree, voiceFour, voiceFive } =
    Maybe.map5 FivePartVoicing voiceOne voiceTwo voiceThree voiceFour voiceFive
        |> Result.fromMaybe
            (CouldNotCompleteVoicing
                [ voiceOne
                , voiceTwo
                , voiceThree
                , voiceFour
                ]
            )


applyStepFivePart : Step -> FivePartVoicingInProgress -> FivePartVoicingInProgress
applyStepFivePart step voicingInProgress =
    case step of
        AssignToVoice voice pitch ->
            setAtFivePartVoicing voice voicingInProgress (Just pitch)

        AssignFirstBelow voice voiceSelections ->
            getAtFivePartVoicing voice voicingInProgress
                |> Maybe.map2
                    Pitch.firstBelow
                    (usePitchClassFromVoiceSelections
                        voiceSelections
                        voicingInProgress.availablePitchClasses
                        voicingInProgress.used
                    )
                |> Maybe.andThen Result.toMaybe
                |> setAtFivePartVoicing (voice + 1) voicingInProgress

        AssignFirstAbove voice voiceSelections ->
            getAtFivePartVoicing voice voicingInProgress
                |> Maybe.map2
                    Pitch.firstAbove
                    (usePitchClassFromVoiceSelections
                        voiceSelections
                        voicingInProgress.availablePitchClasses
                        voicingInProgress.used
                    )
                |> Maybe.andThen Result.toMaybe
                |> setAtFivePartVoicing (voice - 1) voicingInProgress

        CopyVoice fromVoice toVoice ->
            let
                voiceToGet =
                    getAtFivePartVoicing fromVoice voicingInProgress
            in
            setAtFivePartVoicing toVoice voicingInProgress voiceToGet

        DropVoiceByOctave voice ->
            getAtFivePartVoicing voice voicingInProgress
                |> Maybe.map (Pitch.transposeDown Interval.perfectOctave)
                |> Maybe.andThen Result.toMaybe
                |> setAtFivePartVoicing voice voicingInProgress



-- Low interval limit utils


type alias LowIntervalLimit =
    { intervalInSemitones : Interval.Interval
    , lowestAllowedPitch : Pitch
    }


lowIntervalLimitForInterval : Interval.Interval -> Pitch -> LowIntervalLimit
lowIntervalLimitForInterval theInterval thePitch =
    LowIntervalLimit theInterval thePitch


lowIntervalLimits : List LowIntervalLimit
lowIntervalLimits =
    [ lowIntervalLimitForInterval Interval.minorSecond Pitch.e3
    , lowIntervalLimitForInterval Interval.majorSecond Pitch.eFlat3
    , lowIntervalLimitForInterval Interval.minorThird Pitch.c3
    , lowIntervalLimitForInterval Interval.majorThird Pitch.bFlat2
    , lowIntervalLimitForInterval Interval.perfectFourth Pitch.bFlat2
    , lowIntervalLimitForInterval Interval.augmentedFourth Pitch.bFlat2
    , lowIntervalLimitForInterval Interval.perfectFifth Pitch.bFlat1
    , lowIntervalLimitForInterval Interval.minorSixth Pitch.g2
    , lowIntervalLimitForInterval Interval.majorSixth Pitch.f2
    , lowIntervalLimitForInterval Interval.minorSeventh Pitch.f2
    , lowIntervalLimitForInterval Interval.majorSeventh Pitch.f2
    , lowIntervalLimitForInterval Interval.minorNinth Pitch.e2
    , lowIntervalLimitForInterval Interval.majorNinth Pitch.eFlat2
    , lowIntervalLimitForInterval Interval.minorTenth Pitch.c2
    , lowIntervalLimitForInterval Interval.majorTenth Pitch.bFlat1
    ]
