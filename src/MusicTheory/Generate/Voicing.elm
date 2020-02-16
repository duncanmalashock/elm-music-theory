module MusicTheory.Generate.Voicing exposing
    ( FourPartVoicing
    , VoicingError(..)
    , containsIntervalFourParts
    , containsSemitoneDistanceFourParts
    , diffFourParts
    , fourWayClose
    , passesMinorNinthRule
    , semitoneDistancesContainedFourParts
    )

import Libs.Permutations
import MusicTheory.Analyze.ChordClass as AnalyzeChordClass
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.Interval as Interval
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.Octave as Octave
import MusicTheory.Pitch as Pitch
import MusicTheory.PitchClass as PitchClass
import MusicTheory.TertianFactors as TertianFactors


type alias FourPartVoicing =
    { voiceOne : Pitch.Pitch
    , voiceTwo : Pitch.Pitch
    , voiceThree : Pitch.Pitch
    , voiceFour : Pitch.Pitch
    }


type alias FourPartVoicingPlan =
    { voiceOne : PitchClass.PitchClass
    , voiceTwo : PitchClass.PitchClass
    , voiceThree : PitchClass.PitchClass
    , voiceFour : PitchClass.PitchClass
    }


rotateVoices : FourPartVoicingPlan -> FourPartVoicingPlan
rotateVoices fourPartVoicingPlan =
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


fourWayClose : Chord.Chord -> Result VoicingError (List FourPartVoicing)
fourWayClose chord =
    getFourPartVoicingPlans chord
        |> Result.map (List.map planToFourWayCloseVoicings)
        |> Result.map (List.filterMap Result.toMaybe)
        |> Result.map List.concat


getFourPartVoicingPlans :
    Chord.Chord
    -> Result VoicingError (List FourPartVoicingPlan)
getFourPartVoicingPlans chord =
    AnalyzeChordClass.tertianFactorsByCategory
        (Chord.chordClass chord)
        |> Result.mapError CantVoiceNonTertianChord
        |> Result.andThen (fourPartVoicingPlans (Chord.root chord))


planToFourWayCloseVoicings :
    FourPartVoicingPlan
    -> Result VoicingError (List FourPartVoicing)
planToFourWayCloseVoicings plan =
    let
        voicings =
            List.map (planToFourWayCloseVoicingForOctave plan) Octave.all
                |> List.filterMap Result.toMaybe
    in
    case voicings of
        [] ->
            Err NoVoicingsFound

        nonEmptyList ->
            Ok nonEmptyList


planToFourWayCloseVoicingForOctave :
    FourPartVoicingPlan
    -> Octave.Octave
    -> Result VoicingError FourPartVoicing
planToFourWayCloseVoicingForOctave plan octave =
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


fourPartVoicingPlans :
    PitchClass.PitchClass
    -> AnalyzeChordClass.TertianFactorsByCategory
    -> Result VoicingError (List FourPartVoicingPlan)
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
                FourPartVoicingPlan

        planRotatedOnce =
            List.map rotateVoices plans

        planRotatedTwice =
            List.map rotateVoices planRotatedOnce

        planRotatedThreeTimes =
            List.map rotateVoices planRotatedTwice
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
