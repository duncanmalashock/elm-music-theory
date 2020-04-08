module Test.Analyze.JazzChord exposing (all)

import Expect
import MusicTheory.Analyze.JazzChord as AnalyzeChord
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.PitchClass as PitchClass exposing (natural)
import Test exposing (..)


all : Test
all =
    describe "all"
        [ describe "containsPitchClass"
            [ test "C major seventh should contain B natural" <|
                \_ ->
                    let
                        cMajorSeventh =
                            Chord.chord
                                (PitchClass.pitchClass C natural)
                                ChordClass.majorSeventh

                        bNatural =
                            PitchClass.pitchClass B natural

                        result =
                            AnalyzeChord.containsPitchClass bNatural cMajorSeventh
                    in
                    Expect.equal result True
            ]
        ]
