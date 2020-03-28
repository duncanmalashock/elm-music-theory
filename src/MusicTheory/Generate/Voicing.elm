module MusicTheory.Generate.Voicing exposing
    ( Error(..)
    , FivePartVoicing
    , FourPartVoicing
    , ThreePartVoicing
    , fourWayClose
    )

import MusicTheory.Analyze.Chord as AnalyzeChord
    exposing
        ( AvailablePitchClasses
        , VoiceCategory(..)
        )
import MusicTheory.Interval as Interval
import MusicTheory.Pitch as Pitch exposing (Pitch)
import MusicTheory.PitchClass as PitchClass


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


type Step
    = AssignToVoice Int Pitch.Pitch
    | AssignFirstBelow Int VoiceSelection
    | AssignFirstAbove Int VoiceSelection
    | CopyVoice Int Int
    | DropVoiceByOctave Int
    | RaiseVoiceByOctave Int


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


usePitchClassFromVoiceSelection : VoiceSelection -> FourPartVoicingInProgress -> Maybe PitchClass.PitchClass
usePitchClassFromVoiceSelection (VoiceSelection voiceCategory voiceSpecificity) voicingInProgress =
    -- TODO Choosing tones is not parameterized, we just take the head of the list
    let
        chooseFirstUnused list =
            List.filter (\item -> List.member item voicingInProgress.used |> not) list
                |> List.head

        availables =
            voicingInProgress.availablePitchClasses
    in
    case voiceSpecificity of
        ChordTone ->
            case voiceCategory of
                Root ->
                    [ availables.root.true ]
                        |> chooseFirstUnused

                Seventh ->
                    [ availables.seventh.true ]
                        |> chooseFirstUnused

                Fifth ->
                    [ availables.fifth.true ]
                        |> chooseFirstUnused

                Third ->
                    [ availables.third.true ]
                        |> chooseFirstUnused

        SubstituteTone ->
            case voiceCategory of
                Root ->
                    availables.root.substitutes
                        |> chooseFirstUnused

                Seventh ->
                    availables.seventh.substitutes
                        |> chooseFirstUnused

                Fifth ->
                    availables.fifth.substitutes
                        |> chooseFirstUnused

                Third ->
                    availables.third.substitutes
                        |> chooseFirstUnused

        ChordToneOrSubstitute ->
            case voiceCategory of
                Root ->
                    availables.root.true
                        :: availables.root.substitutes
                        |> chooseFirstUnused

                Seventh ->
                    availables.seventh.true
                        :: availables.seventh.substitutes
                        |> chooseFirstUnused

                Fifth ->
                    availables.fifth.true
                        :: availables.fifth.substitutes
                        |> chooseFirstUnused

                Third ->
                    availables.third.true
                        :: availables.third.substitutes
                        |> chooseFirstUnused


determineVoiceCategory : PitchClass.PitchClass -> AvailablePitchClasses -> Maybe VoiceCategory
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


fourWayClose : Pitch.Pitch -> AvailablePitchClasses -> Result Error FourPartVoicing
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
                        , AssignFirstBelow 1 (chordToneOrSubstitute secondVoiceCategory)
                        , AssignFirstBelow 2 (chordToneOrSubstitute thirdVoiceCategory)
                        , AssignFirstBelow 3 (chordToneOrSubstitute fourthVoiceCategory)
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


setAtFourPartVoicing : Int -> FourPartVoicingInProgress -> Maybe Pitch.Pitch -> FourPartVoicingInProgress
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


type Error
    = CouldNotCompleteVoicing (List (Maybe Pitch.Pitch))
    | VoiceCategoriesWereUndefined (List (Maybe VoiceCategory))
    | AnalyzeChordError AnalyzeChord.Error


applyStepFourPart : Step -> FourPartVoicingInProgress -> FourPartVoicingInProgress
applyStepFourPart step voicingInProgress =
    case step of
        AssignToVoice voice pitch ->
            setAtFourPartVoicing voice voicingInProgress (Just pitch)

        AssignFirstBelow voice voiceSelection ->
            getAtFourPartVoicing voice voicingInProgress
                |> Maybe.map2
                    Pitch.firstBelow
                    (usePitchClassFromVoiceSelection
                        voiceSelection
                        voicingInProgress
                    )
                |> Maybe.andThen Result.toMaybe
                |> setAtFourPartVoicing (voice + 1) voicingInProgress

        AssignFirstAbove voice voiceSelection ->
            getAtFourPartVoicing voice voicingInProgress
                |> Maybe.map2
                    Pitch.firstAbove
                    (usePitchClassFromVoiceSelection
                        voiceSelection
                        voicingInProgress
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

        RaiseVoiceByOctave voice ->
            getAtFourPartVoicing voice voicingInProgress
                |> Maybe.map (Pitch.transposeUp Interval.perfectOctave)
                |> Maybe.andThen Result.toMaybe
                |> setAtFourPartVoicing voice voicingInProgress
