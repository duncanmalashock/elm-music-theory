module MusicTheory.Generate.Voicing exposing
    ( FourPartVoicing
    , VoicingError(..)
    , fourWayClose
    )

import Libs.Permutations
import MusicTheory.Analyze.ChordClass as AnalyzeChordClass
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
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
