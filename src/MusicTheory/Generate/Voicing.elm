module MusicTheory.Generate.Voicing exposing
    ( VoicingError(..)
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


fourWayClose : Chord.Chord -> Result VoicingError (List FourPartVoicingPlan)
fourWayClose chord =
    Result.map
        (\factorsByCategory ->
            factorsByCategoryToFourPartVoicingPlan (Chord.root chord) factorsByCategory
        )
        (AnalyzeChordClass.tertianFactorsByCategory (Chord.chordClass chord))
        |> Result.mapError CantVoiceNonTertianChord
        |> Result.andThen
            (\list ->
                case list of
                    [] ->
                        Err MissingVoiceCategory

                    theList ->
                        Ok theList
            )


factorToPitchClass : PitchClass.PitchClass -> TertianFactors.TertianFactor -> PitchClass.PitchClass
factorToPitchClass root factor =
    PitchClass.transposeUp (TertianFactors.toInterval factor) root


factorsByCategoryToFourPartVoicingPlan : PitchClass.PitchClass -> AnalyzeChordClass.TertianFactorsByCategory -> List FourPartVoicingPlan
factorsByCategoryToFourPartVoicingPlan root factorCats =
    let
        plans =
            Libs.Permutations.permutations4
                (List.map (factorToPitchClass root) factorCats.root)
                (List.map (factorToPitchClass root) factorCats.seventh)
                (List.map (factorToPitchClass root) factorCats.fifth)
                (List.map (factorToPitchClass root) factorCats.third)
                FourPartVoicingPlan

        planRotatedOnce =
            List.map rotateVoices plans

        planRotatedTwice =
            List.map rotateVoices planRotatedOnce

        planRotatedThreeTimes =
            List.map rotateVoices planRotatedTwice
    in
    plans
        ++ planRotatedOnce
        ++ planRotatedTwice
        ++ planRotatedThreeTimes
