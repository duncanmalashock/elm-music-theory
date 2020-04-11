module MusicTheory.Generate.JazzVoicing exposing
    ( Error(..)
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
    , substituteDoubleLead
    )

import MusicTheory.Analyze.JazzChord as AnalyzeChord
    exposing
        ( Availables
        , VoiceCategory(..)
        )
import MusicTheory.Interval as Interval
import MusicTheory.VoicingClass
    exposing
        ( FivePartVoicingClass
        , FourPartVoicingClass
        , ThreePartVoicingClass
        )


type Error
    = CouldNotCompleteVoicing
    | VoiceCategoriesWereUndefined (List (Maybe VoiceCategory))
    | AnalyzeChordError AnalyzeChord.Error


type Step
    = AssignToVoice Int Interval.Interval
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
    -> Availables
    -> List Interval.Interval
pitchClassesFromVoiceSelections voiceSelections availables =
    let
        pitchClassesFromVoiceSelection :
            VoiceSelection
            -> List Interval.Interval
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


useFromVoiceSelections :
    List VoiceSelection
    -> Availables
    -> List Interval.Interval
    -> Maybe Interval.Interval
useFromVoiceSelections voiceSelections availables used =
    let
        chooseFirstUnused list =
            List.filter (\item -> List.member item used |> not) list
                |> List.head
    in
    -- TODO Choosing tones is not parameterized, we just take the head of the list
    pitchClassesFromVoiceSelections voiceSelections availables
        |> chooseFirstUnused


determineVoiceCategory :
    Interval.Interval
    -> Availables
    -> Maybe VoiceCategory
determineVoiceCategory interval availables =
    let
        rootPitchClasses =
            availables.root.true
                :: availables.root.substitutes

        thirdPitchClasses =
            availables.third.true
                :: availables.third.substitutes

        fifthPitchClasses =
            availables.fifth.true
                :: availables.fifth.substitutes

        seventhPitchClasses =
            availables.seventh.true
                :: availables.seventh.substitutes
    in
    if List.member interval rootPitchClasses then
        Just Root

    else if List.member interval thirdPitchClasses then
        Just Third

    else if List.member interval fifthPitchClasses then
        Just Fifth

    else if List.member interval seventhPitchClasses then
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
    Interval.Interval
    -> Availables
    -> Result Error FourPartVoicingClass
fourWayClose leadVoice availables =
    let
        maybeFirstVoiceCategory =
            determineVoiceCategory leadVoice availables

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
                        (fourPartInit availables)
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
                |> Result.fromMaybe CouldNotCompleteVoicing

        Nothing ->
            Err <|
                VoiceCategoriesWereUndefined
                    [ maybeFirstVoiceCategory
                    , maybeSecondVoiceCategory
                    , maybeThirdVoiceCategory
                    , maybeFourthVoiceCategory
                    ]


fourWayCloseDoubleLead :
    Interval.Interval
    -> Availables
    -> Result Error FivePartVoicingClass
fourWayCloseDoubleLead leadVoice availables =
    let
        maybeFirstVoiceCategory =
            determineVoiceCategory leadVoice availables

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
                        (fivePartInit availables)
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
                |> Result.fromMaybe CouldNotCompleteVoicing

        Nothing ->
            Err <|
                VoiceCategoriesWereUndefined
                    [ maybeFirstVoiceCategory
                    , maybeSecondVoiceCategory
                    , maybeThirdVoiceCategory
                    , maybeFourthVoiceCategory
                    ]


fourWaySpread :
    Interval.Interval
    -> Availables
    -> Result Error FourPartVoicingClass
fourWaySpread bass availables =
    let
        voicing =
            List.foldl
                applyStepFourPart
                (fourPartInit availables)
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
        |> Result.fromMaybe CouldNotCompleteVoicing


fiveWaySpread :
    Interval.Interval
    -> Availables
    -> Result Error FivePartVoicingClass
fiveWaySpread bass availables =
    let
        voicing =
            List.foldl
                applyStepFivePart
                (fivePartInit availables)
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
        |> Result.fromMaybe CouldNotCompleteVoicing


fourWayDrop2 :
    Availables
    -> FourPartVoicingClass
    -> Result Error FourPartVoicingClass
fourWayDrop2 availables { voiceOne, voiceTwo, voiceThree, voiceFour } =
    { voiceOne = Just voiceOne
    , voiceTwo = Just voiceThree
    , voiceThree = Just voiceFour
    , voiceFour =
        Interval.subtract Interval.perfectOctave voiceTwo
            |> Just
    , used = []
    , availables = availables
    }
        |> completeFourPart
        |> Result.fromMaybe CouldNotCompleteVoicing


fourWayDrop3 :
    Availables
    -> FourPartVoicingClass
    -> Result Error FourPartVoicingClass
fourWayDrop3 availables { voiceOne, voiceTwo, voiceThree, voiceFour } =
    { voiceOne = Just voiceOne
    , voiceTwo = Just voiceTwo
    , voiceThree = Just voiceFour
    , voiceFour =
        Interval.subtract Interval.perfectOctave voiceThree
            |> Just
    , used = []
    , availables = availables
    }
        |> completeFourPart
        |> Result.fromMaybe CouldNotCompleteVoicing


fourWayDrop2and4 :
    Availables
    -> FourPartVoicingClass
    -> Result Error FourPartVoicingClass
fourWayDrop2and4 availables { voiceOne, voiceTwo, voiceThree, voiceFour } =
    { voiceOne = Just voiceOne
    , voiceTwo = Just voiceThree
    , voiceThree =
        Interval.subtract Interval.perfectOctave voiceTwo
            |> Just
    , voiceFour =
        Interval.subtract Interval.perfectOctave voiceFour
            |> Just
    , used = []
    , availables = availables
    }
        |> completeFourPart
        |> Result.fromMaybe CouldNotCompleteVoicing


substituteDoubleLead :
    Interval.Interval
    -> Availables
    -> Result Error FivePartVoicingClass
substituteDoubleLead leadVoice availables =
    let
        maybeFirstVoiceCategory =
            determineVoiceCategory leadVoice availables

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
                        (fivePartInit availables)
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
                |> Result.fromMaybe CouldNotCompleteVoicing

        Nothing ->
            Err <|
                VoiceCategoriesWereUndefined
                    [ maybeFirstVoiceCategory
                    , maybeSecondVoiceCategory
                    , maybeThirdVoiceCategory
                    , maybeFourthVoiceCategory
                    ]


fiveWayDrop2 :
    Availables
    -> FivePartVoicingClass
    -> Result Error FivePartVoicingClass
fiveWayDrop2 availables { voiceOne, voiceTwo, voiceThree, voiceFour, voiceFive } =
    { voiceOne = Just voiceOne
    , voiceTwo = Just voiceThree
    , voiceThree = Just voiceFour
    , voiceFour =
        Interval.subtract Interval.perfectOctave voiceTwo
            |> Just
    , voiceFive = Just voiceFive
    , used = []
    , availables = availables
    }
        |> completeFivePart
        |> Result.fromMaybe CouldNotCompleteVoicing


fiveWayDrop3 :
    Availables
    -> FivePartVoicingClass
    -> Result Error FivePartVoicingClass
fiveWayDrop3 availables { voiceOne, voiceTwo, voiceThree, voiceFour, voiceFive } =
    { voiceOne = Just voiceOne
    , voiceTwo = Just voiceTwo
    , voiceThree = Just voiceFour
    , voiceFour =
        Interval.subtract Interval.perfectOctave voiceThree
            |> Just
    , voiceFive = Just voiceFive
    , used = []
    , availables = availables
    }
        |> completeFivePart
        |> Result.fromMaybe CouldNotCompleteVoicing


fiveWayDrop2and4 : Availables -> FivePartVoicingClass -> Result Error FivePartVoicingClass
fiveWayDrop2and4 availables { voiceOne, voiceTwo, voiceThree, voiceFour, voiceFive } =
    { voiceOne = Just voiceOne
    , voiceTwo = Just voiceThree
    , voiceThree =
        Interval.subtract Interval.perfectOctave voiceTwo
            |> Just
    , voiceFour =
        Interval.subtract Interval.perfectOctave voiceFour
            |> Just
    , voiceFive = Just voiceFive
    , used = []
    , availables = availables
    }
        |> completeFivePart
        |> Result.fromMaybe CouldNotCompleteVoicing



-- Four-part voicing utils


type alias FourPartVoicingClassInProgress =
    { voiceOne : Maybe Interval.Interval
    , voiceTwo : Maybe Interval.Interval
    , voiceThree : Maybe Interval.Interval
    , voiceFour : Maybe Interval.Interval
    , availables : Availables
    , used : List Interval.Interval
    }


getAtFourPartVoicingClass : Int -> FourPartVoicingClassInProgress -> Maybe Interval.Interval
getAtFourPartVoicingClass index voicingInProgress =
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


setAtFourPartVoicingClass :
    Int
    -> FourPartVoicingClassInProgress
    -> Maybe Interval.Interval
    -> FourPartVoicingClassInProgress
setAtFourPartVoicingClass index voicingInProgress toSet =
    let
        updateUsed currentUsed used =
            currentUsed
                ++ List.filterMap identity
                    [ used
                    ]
    in
    case index of
        1 ->
            { voicingInProgress
                | voiceOne = toSet
                , used =
                    updateUsed voicingInProgress.used toSet
            }

        2 ->
            { voicingInProgress
                | voiceTwo = toSet
                , used =
                    updateUsed voicingInProgress.used toSet
            }

        3 ->
            { voicingInProgress
                | voiceThree = toSet
                , used =
                    updateUsed voicingInProgress.used toSet
            }

        4 ->
            { voicingInProgress
                | voiceFour = toSet
                , used =
                    updateUsed voicingInProgress.used toSet
            }

        _ ->
            voicingInProgress


fourPartInit : Availables -> FourPartVoicingClassInProgress
fourPartInit availables =
    { voiceOne = Nothing
    , voiceTwo = Nothing
    , voiceThree = Nothing
    , voiceFour = Nothing
    , availables = availables
    , used = []
    }


completeFourPart :
    FourPartVoicingClassInProgress
    -> Maybe FourPartVoicingClass
completeFourPart { voiceOne, voiceTwo, voiceThree, voiceFour } =
    Maybe.map4 FourPartVoicingClass voiceOne voiceTwo voiceThree voiceFour


applyStepFourPart :
    Step
    -> FourPartVoicingClassInProgress
    -> FourPartVoicingClassInProgress
applyStepFourPart step voicingInProgress =
    case step of
        AssignToVoice voice pitch ->
            setAtFourPartVoicingClass voice voicingInProgress (Just pitch)

        AssignFirstBelow voice voiceSelections ->
            getAtFourPartVoicingClass voice voicingInProgress
                |> Maybe.map2
                    Interval.firstBelow
                    (useFromVoiceSelections
                        voiceSelections
                        voicingInProgress.availables
                        voicingInProgress.used
                    )
                |> setAtFourPartVoicingClass (voice + 1) voicingInProgress

        AssignFirstAbove voice voiceSelections ->
            getAtFourPartVoicingClass voice voicingInProgress
                |> Maybe.map2
                    Interval.firstAbove
                    (useFromVoiceSelections
                        voiceSelections
                        voicingInProgress.availables
                        voicingInProgress.used
                    )
                |> setAtFourPartVoicingClass (voice - 1) voicingInProgress

        CopyVoice fromVoice toVoice ->
            let
                voiceToGet =
                    getAtFourPartVoicingClass fromVoice voicingInProgress
            in
            setAtFourPartVoicingClass toVoice voicingInProgress voiceToGet

        DropVoiceByOctave voice ->
            getAtFourPartVoicingClass voice voicingInProgress
                |> Maybe.map
                    (Interval.subtract Interval.perfectOctave)
                |> setAtFourPartVoicingClass voice voicingInProgress



-- Five-part voicing utils


type alias FivePartVoicingClassInProgress =
    { voiceOne : Maybe Interval.Interval
    , voiceTwo : Maybe Interval.Interval
    , voiceThree : Maybe Interval.Interval
    , voiceFour : Maybe Interval.Interval
    , voiceFive : Maybe Interval.Interval
    , availables : Availables
    , used : List Interval.Interval
    }


getAtFivePartVoicingClass :
    Int
    -> FivePartVoicingClassInProgress
    -> Maybe Interval.Interval
getAtFivePartVoicingClass index voicingInProgress =
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


setAtFivePartVoicingClass :
    Int
    -> FivePartVoicingClassInProgress
    -> Maybe Interval.Interval
    -> FivePartVoicingClassInProgress
setAtFivePartVoicingClass index voicingInProgress toSet =
    let
        updateUsed currentUsed used =
            currentUsed
                ++ List.filterMap identity
                    [ used
                    ]
    in
    case index of
        1 ->
            { voicingInProgress
                | voiceOne = toSet
                , used =
                    updateUsed voicingInProgress.used toSet
            }

        2 ->
            { voicingInProgress
                | voiceTwo = toSet
                , used =
                    updateUsed voicingInProgress.used toSet
            }

        3 ->
            { voicingInProgress
                | voiceThree = toSet
                , used =
                    updateUsed voicingInProgress.used toSet
            }

        4 ->
            { voicingInProgress
                | voiceFour = toSet
                , used =
                    updateUsed voicingInProgress.used toSet
            }

        5 ->
            { voicingInProgress
                | voiceFive = toSet
                , used =
                    updateUsed voicingInProgress.used toSet
            }

        _ ->
            voicingInProgress


fivePartInit : Availables -> FivePartVoicingClassInProgress
fivePartInit availables =
    { voiceOne = Nothing
    , voiceTwo = Nothing
    , voiceThree = Nothing
    , voiceFour = Nothing
    , voiceFive = Nothing
    , availables = availables
    , used = []
    }


completeFivePart :
    FivePartVoicingClassInProgress
    -> Maybe FivePartVoicingClass
completeFivePart { voiceOne, voiceTwo, voiceThree, voiceFour, voiceFive } =
    Maybe.map5 FivePartVoicingClass voiceOne voiceTwo voiceThree voiceFour voiceFive


applyStepFivePart :
    Step
    -> FivePartVoicingClassInProgress
    -> FivePartVoicingClassInProgress
applyStepFivePart step voicingInProgress =
    case step of
        AssignToVoice voice pitch ->
            setAtFivePartVoicingClass voice voicingInProgress (Just pitch)

        AssignFirstBelow voice voiceSelections ->
            getAtFivePartVoicingClass voice voicingInProgress
                |> Maybe.map2
                    Interval.firstBelow
                    (useFromVoiceSelections
                        voiceSelections
                        voicingInProgress.availables
                        voicingInProgress.used
                    )
                |> setAtFivePartVoicingClass (voice + 1) voicingInProgress

        AssignFirstAbove voice voiceSelections ->
            getAtFivePartVoicingClass voice voicingInProgress
                |> Maybe.map2
                    Interval.firstAbove
                    (useFromVoiceSelections
                        voiceSelections
                        voicingInProgress.availables
                        voicingInProgress.used
                    )
                |> setAtFivePartVoicingClass (voice - 1) voicingInProgress

        CopyVoice fromVoice toVoice ->
            let
                voiceToGet =
                    getAtFivePartVoicingClass fromVoice voicingInProgress
            in
            setAtFivePartVoicingClass toVoice voicingInProgress voiceToGet

        DropVoiceByOctave voice ->
            getAtFivePartVoicingClass voice voicingInProgress
                |> Maybe.map
                    (Interval.subtract Interval.perfectOctave)
                |> setAtFivePartVoicingClass voice voicingInProgress
