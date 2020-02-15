module MusicTheory.Analyze.Voicing exposing (diffFourParts)

import MusicTheory.Generate.Voicing exposing (FourPartVoicing)
import MusicTheory.Pitch as Pitch


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
