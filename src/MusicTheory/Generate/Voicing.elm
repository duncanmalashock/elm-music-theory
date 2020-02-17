module MusicTheory.Generate.Voicing exposing
    ( FourPartVoicing
    , VoicingError(..)
    , containsIntervalFourParts
    , containsSemitoneDistanceFourParts
    , diffFourParts
    , drop2
    , drop2and4
    , fiveWayCloseDoubleLead
    , fourWayClose
    , isAboveLowIntervalLimits
    , lowIntervalLimitsDict
    , passesMinorNinthRule
    , semitoneDistancesContainedFourParts
    )

import Dict exposing (Dict)
import Libs.Permutations
import MusicTheory.Analyze.ChordClass as AnalyzeChordClass
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.Interval as Interval
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.Octave as Octave
import MusicTheory.Pitch as Pitch exposing (Pitch)
import MusicTheory.PitchClass as PitchClass
import MusicTheory.TertianFactors as TertianFactors


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


type alias FourCategoryVoicingPlan =
    { voiceOne : PitchClass.PitchClass
    , voiceTwo : PitchClass.PitchClass
    , voiceThree : PitchClass.PitchClass
    , voiceFour : PitchClass.PitchClass
    }


rotateFourVoices : FourCategoryVoicingPlan -> FourCategoryVoicingPlan
rotateFourVoices fourPartVoicingPlan =
    { voiceOne = fourPartVoicingPlan.voiceTwo
    , voiceTwo = fourPartVoicingPlan.voiceThree
    , voiceThree = fourPartVoicingPlan.voiceFour
    , voiceFour = fourPartVoicingPlan.voiceOne
    }


type VoicingError
    = CantVoiceNonTertianChord ChordClass.ChordClassError
    | MissingVoiceCategory
    | VoiceOutOfRange Pitch.PitchError
    | NoVoicingsFound


type alias LowIntervalLimit =
    { intervalInSemitones : Int
    , lowestAllowedPitch : Pitch
    }


lowIntervalLimitForInterval : Interval.Interval -> Pitch -> LowIntervalLimit
lowIntervalLimitForInterval theInterval thePitch =
    LowIntervalLimit (Interval.semitones theInterval) thePitch


lowIntervalLimitForSemitones : Int -> Pitch -> LowIntervalLimit
lowIntervalLimitForSemitones semitones thePitch =
    LowIntervalLimit semitones thePitch


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
    , lowIntervalLimitForSemitones 13 Pitch.e2
    , lowIntervalLimitForSemitones 14 Pitch.eFlat2
    , lowIntervalLimitForSemitones 15 Pitch.c2
    , lowIntervalLimitForSemitones 16 Pitch.bFlat1
    ]


lowIntervalLimitsDict : Dict Int Pitch
lowIntervalLimitsDict =
    List.map
        (\{ intervalInSemitones, lowestAllowedPitch } ->
            ( intervalInSemitones, lowestAllowedPitch )
        )
        lowIntervalLimits
        |> Dict.fromList


isAboveLowIntervalLimits : Pitch -> Pitch -> Bool
isAboveLowIntervalLimits first second =
    let
        semitonesBetween =
            Pitch.semitones second - Pitch.semitones first

        maybeLimit =
            Dict.get semitonesBetween lowIntervalLimitsDict
    in
    case maybeLimit of
        Nothing ->
            True

        Just limit ->
            Pitch.semitones first >= Pitch.semitones limit


checkLowIntervalLimitsFourParts : Chord.Chord -> FourPartVoicing -> Bool
checkLowIntervalLimitsFourParts chord voicing =
    let
        rootIsInBass =
            Chord.root chord == Pitch.pitchClass voicing.voiceFour
    in
    if rootIsInBass then
        isAboveLowIntervalLimits voicing.voiceFour voicing.voiceThree

    else
        let
            imaginaryRootVoice =
                Pitch.firstBelow (Chord.root chord) voicing.voiceFour
        in
        case imaginaryRootVoice of
            Ok rootVoice ->
                isAboveLowIntervalLimits rootVoice voicing.voiceFour

            Err _ ->
                False


checkLowIntervalLimitsFiveParts : Chord.Chord -> FivePartVoicing -> Bool
checkLowIntervalLimitsFiveParts chord voicing =
    let
        rootIsInBass =
            Chord.root chord == Pitch.pitchClass voicing.voiceFive
    in
    if rootIsInBass then
        isAboveLowIntervalLimits voicing.voiceFive voicing.voiceFour

    else
        let
            imaginaryRootVoice =
                Pitch.firstBelow (Chord.root chord) voicing.voiceFive
        in
        case imaginaryRootVoice of
            Ok rootVoice ->
                isAboveLowIntervalLimits rootVoice voicing.voiceFive

            Err _ ->
                False


fourWayClose : Chord.Chord -> Result VoicingError (List FourPartVoicing)
fourWayClose chord =
    getFourCategoryVoicingPlans chord
        |> Result.map
            (List.map
                (planToFourPartVoicings planToFourWayCloseVoicing chord)
            )
        |> Result.map (List.filterMap Result.toMaybe)
        |> Result.map List.concat


drop2 : Chord.Chord -> Result VoicingError (List FourPartVoicing)
drop2 chord =
    getFourCategoryVoicingPlans chord
        |> Result.map
            (List.map
                (planToFourPartVoicings planToDrop2Voicing chord)
            )
        |> Result.map (List.filterMap Result.toMaybe)
        |> Result.map List.concat


drop2and4 : Chord.Chord -> Result VoicingError (List FourPartVoicing)
drop2and4 chord =
    getFourCategoryVoicingPlans chord
        |> Result.map
            (List.map
                (planToFourPartVoicings planToDrop2and4Voicing chord)
            )
        |> Result.map (List.filterMap Result.toMaybe)
        |> Result.map List.concat


fiveWayCloseDoubleLead : Chord.Chord -> Result VoicingError (List FivePartVoicing)
fiveWayCloseDoubleLead chord =
    getFourCategoryVoicingPlans chord
        |> Result.map
            (List.map
                (planToFivePartVoicings planToFiveWayCloseDoubleLeadVoicing chord)
            )
        |> Result.map (List.filterMap Result.toMaybe)
        |> Result.map List.concat


getFourCategoryVoicingPlans :
    Chord.Chord
    -> Result VoicingError (List FourCategoryVoicingPlan)
getFourCategoryVoicingPlans chord =
    AnalyzeChordClass.tertianFactorsByCategory
        (Chord.chordClass chord)
        |> Result.mapError CantVoiceNonTertianChord
        |> Result.andThen (fourPartVoicingPlans (Chord.root chord))


planToFourPartVoicings :
    (Chord.Chord
     -> FourCategoryVoicingPlan
     -> Octave.Octave
     -> Result VoicingError FourPartVoicing
    )
    -> Chord.Chord
    -> FourCategoryVoicingPlan
    -> Result VoicingError (List FourPartVoicing)
planToFourPartVoicings voicingStrategy chord plan =
    let
        voicings =
            List.map (voicingStrategy chord plan) Octave.all
                |> List.filterMap Result.toMaybe
                |> List.filter (checkLowIntervalLimitsFourParts chord)
    in
    case voicings of
        [] ->
            Err NoVoicingsFound

        nonEmptyList ->
            Ok nonEmptyList


planToFivePartVoicings :
    (Chord.Chord
     -> FourCategoryVoicingPlan
     -> Octave.Octave
     -> Result VoicingError FivePartVoicing
    )
    -> Chord.Chord
    -> FourCategoryVoicingPlan
    -> Result VoicingError (List FivePartVoicing)
planToFivePartVoicings voicingStrategy chord plan =
    let
        voicings =
            List.map (voicingStrategy chord plan) Octave.all
                |> List.filterMap Result.toMaybe
                |> List.filter (checkLowIntervalLimitsFiveParts chord)
    in
    case voicings of
        [] ->
            Err NoVoicingsFound

        nonEmptyList ->
            Ok nonEmptyList


planToFourWayCloseVoicing :
    Chord.Chord
    -> FourCategoryVoicingPlan
    -> Octave.Octave
    -> Result VoicingError FourPartVoicing
planToFourWayCloseVoicing chord plan octave =
    let
        voiceOne =
            Pitch.fromPitchClass octave plan.voiceOne

        voiceTwo =
            Result.andThen (Pitch.firstBelow plan.voiceTwo) voiceOne

        voiceThree =
            Result.andThen (Pitch.firstBelow plan.voiceThree) voiceTwo

        voiceFour =
            Result.andThen (Pitch.firstBelow plan.voiceFour) voiceThree
    in
    Result.map4 FourPartVoicing voiceOne voiceTwo voiceThree voiceFour
        |> Result.mapError VoiceOutOfRange


planToDrop2Voicing :
    Chord.Chord
    -> FourCategoryVoicingPlan
    -> Octave.Octave
    -> Result VoicingError FourPartVoicing
planToDrop2Voicing chord plan octave =
    let
        voiceOne =
            Pitch.fromPitchClass octave plan.voiceOne

        voiceTwo =
            Result.andThen (Pitch.firstBelow plan.voiceThree) voiceOne

        voiceThree =
            Result.andThen (Pitch.firstBelow plan.voiceFour) voiceTwo

        voiceFour =
            Result.andThen (Pitch.firstBelow plan.voiceTwo) voiceThree
    in
    Result.map4 FourPartVoicing voiceOne voiceTwo voiceThree voiceFour
        |> Result.mapError VoiceOutOfRange


planToDrop2and4Voicing :
    Chord.Chord
    -> FourCategoryVoicingPlan
    -> Octave.Octave
    -> Result VoicingError FourPartVoicing
planToDrop2and4Voicing chord plan octave =
    let
        voiceOne =
            Pitch.fromPitchClass octave plan.voiceOne

        voiceTwo =
            Result.andThen (Pitch.firstBelow plan.voiceThree) voiceOne

        voiceThree =
            Result.andThen (Pitch.firstBelow plan.voiceTwo) voiceTwo

        voiceFour =
            Result.andThen (Pitch.firstBelow plan.voiceFour) voiceThree
    in
    Result.map4 FourPartVoicing voiceOne voiceTwo voiceThree voiceFour
        |> Result.mapError VoiceOutOfRange


planToFiveWayCloseDoubleLeadVoicing :
    Chord.Chord
    -> FourCategoryVoicingPlan
    -> Octave.Octave
    -> Result VoicingError FivePartVoicing
planToFiveWayCloseDoubleLeadVoicing chord plan octave =
    let
        voiceOne =
            Pitch.fromPitchClass octave plan.voiceOne

        voiceTwo =
            Result.andThen (Pitch.firstBelow plan.voiceTwo) voiceOne

        voiceThree =
            Result.andThen (Pitch.firstBelow plan.voiceThree) voiceTwo

        voiceFour =
            Result.andThen (Pitch.firstBelow plan.voiceFour) voiceThree

        voiceFive =
            Result.andThen (Pitch.firstBelow plan.voiceOne) voiceFour
    in
    Result.map5 FivePartVoicing voiceOne voiceTwo voiceThree voiceFour voiceFive
        |> Result.mapError VoiceOutOfRange


fourPartVoicingPlans :
    PitchClass.PitchClass
    -> AnalyzeChordClass.TertianFactorsByCategory
    -> Result VoicingError (List FourCategoryVoicingPlan)
fourPartVoicingPlans root factorCats =
    let
        factorToPitchClass factor =
            PitchClass.transposeUp (TertianFactors.toInterval factor) root

        plans =
            Libs.Permutations.permutations4
                (List.map factorToPitchClass factorCats.root)
                (List.map factorToPitchClass factorCats.seventh)
                (List.map factorToPitchClass factorCats.fifth)
                (List.map factorToPitchClass factorCats.third)
                FourCategoryVoicingPlan

        planRotatedOnce =
            List.map rotateFourVoices plans

        planRotatedTwice =
            List.map rotateFourVoices planRotatedOnce

        planRotatedThreeTimes =
            List.map rotateFourVoices planRotatedTwice
    in
    case plans of
        [] ->
            Err MissingVoiceCategory

        nonEmptyPlans ->
            Ok
                (nonEmptyPlans
                    ++ planRotatedOnce
                    ++ planRotatedTwice
                    ++ planRotatedThreeTimes
                )


diffFourParts : FourPartVoicing -> FourPartVoicing -> Int
diffFourParts firstVoicing secondVoicing =
    let
        diff a b =
            Basics.abs (Pitch.semitones a - Pitch.semitones b)
    in
    diff firstVoicing.voiceOne secondVoicing.voiceOne
        + diff firstVoicing.voiceTwo secondVoicing.voiceTwo
        + diff firstVoicing.voiceThree secondVoicing.voiceThree
        + diff firstVoicing.voiceFour secondVoicing.voiceFour


containsIntervalFourParts : Interval.Interval -> FourPartVoicing -> Int
containsIntervalFourParts theInterval voicing =
    let
        intervalSemitones =
            Interval.semitones theInterval

        checkForInterval a b =
            Basics.abs (Pitch.semitones a - Pitch.semitones b) == intervalSemitones
    in
    [ checkForInterval voicing.voiceOne voicing.voiceTwo
    , checkForInterval voicing.voiceOne voicing.voiceThree
    , checkForInterval voicing.voiceOne voicing.voiceFour
    , checkForInterval voicing.voiceTwo voicing.voiceThree
    , checkForInterval voicing.voiceTwo voicing.voiceFour
    , checkForInterval voicing.voiceThree voicing.voiceFour
    ]
        |> List.filter identity
        |> List.length


containsSemitoneDistanceFourParts : Int -> FourPartVoicing -> Int
containsSemitoneDistanceFourParts semitoneDistance voicing =
    let
        checkForSemitoneDistance a b =
            Basics.abs (Pitch.semitones a - Pitch.semitones b) == semitoneDistance
    in
    [ checkForSemitoneDistance voicing.voiceOne voicing.voiceTwo
    , checkForSemitoneDistance voicing.voiceOne voicing.voiceThree
    , checkForSemitoneDistance voicing.voiceOne voicing.voiceFour
    , checkForSemitoneDistance voicing.voiceTwo voicing.voiceThree
    , checkForSemitoneDistance voicing.voiceTwo voicing.voiceFour
    , checkForSemitoneDistance voicing.voiceThree voicing.voiceFour
    ]
        |> List.filter identity
        |> List.length


passesMinorNinthRule : FourPartVoicing -> Bool
passesMinorNinthRule voicing =
    -- TODO: This should return True in cases with a root-to-9th interval in dominant 7 b9 chords
    containsSemitoneDistanceFourParts 13 voicing == 0


semitoneDistancesContainedFourParts : FourPartVoicing -> List Int
semitoneDistancesContainedFourParts voicing =
    let
        semitoneDistance a b =
            Pitch.semitones b - Pitch.semitones a
    in
    [ semitoneDistance voicing.voiceOne voicing.voiceTwo
    , semitoneDistance voicing.voiceOne voicing.voiceThree
    , semitoneDistance voicing.voiceOne voicing.voiceFour
    , semitoneDistance voicing.voiceTwo voicing.voiceThree
    , semitoneDistance voicing.voiceTwo voicing.voiceFour
    , semitoneDistance voicing.voiceThree voicing.voiceFour
    ]
