module Generate.Voicing exposing (all)

import Expect
import MusicTheory.Chord as Chord
import MusicTheory.ChordClass as ChordClass
import MusicTheory.Generate.Voicing as GenerateVoicing
import MusicTheory.Letter exposing (Letter(..))
import MusicTheory.PitchClass as PitchClass exposing (natural)
import Test exposing (..)


all : Test
all =
    describe "all"
        [ describe "fourWayClose"
            [ test "Correct voicing plans for C major 6/9 chord" <|
                \_ ->
                    let
                        cMajorSixNineChord =
                            Chord.chord (PitchClass.pitchClass C natural) ChordClass.majorSixNine

                        expected =
                            Ok
                                [ { voiceOne = PitchClass.pitchClass C natural
                                  , voiceTwo = PitchClass.pitchClass A natural
                                  , voiceThree = PitchClass.pitchClass G natural
                                  , voiceFour = PitchClass.pitchClass E natural
                                  }
                                , { voiceOne = PitchClass.pitchClass D natural
                                  , voiceTwo = PitchClass.pitchClass A natural
                                  , voiceThree = PitchClass.pitchClass G natural
                                  , voiceFour = PitchClass.pitchClass E natural
                                  }
                                , { voiceOne = PitchClass.pitchClass A natural
                                  , voiceTwo = PitchClass.pitchClass G natural
                                  , voiceThree = PitchClass.pitchClass E natural
                                  , voiceFour = PitchClass.pitchClass C natural
                                  }
                                , { voiceOne = PitchClass.pitchClass A natural
                                  , voiceTwo = PitchClass.pitchClass G natural
                                  , voiceThree = PitchClass.pitchClass E natural
                                  , voiceFour = PitchClass.pitchClass D natural
                                  }
                                , { voiceOne = PitchClass.pitchClass G natural
                                  , voiceTwo = PitchClass.pitchClass E natural
                                  , voiceThree = PitchClass.pitchClass C natural
                                  , voiceFour = PitchClass.pitchClass A natural
                                  }
                                , { voiceOne = PitchClass.pitchClass G natural
                                  , voiceTwo = PitchClass.pitchClass E natural
                                  , voiceThree = PitchClass.pitchClass D natural
                                  , voiceFour = PitchClass.pitchClass A natural
                                  }
                                , { voiceOne = PitchClass.pitchClass E natural
                                  , voiceTwo = PitchClass.pitchClass C natural
                                  , voiceThree = PitchClass.pitchClass A natural
                                  , voiceFour = PitchClass.pitchClass G natural
                                  }
                                , { voiceOne = PitchClass.pitchClass E natural
                                  , voiceTwo = PitchClass.pitchClass D natural
                                  , voiceThree = PitchClass.pitchClass A natural
                                  , voiceFour = PitchClass.pitchClass G natural
                                  }
                                ]

                        result =
                            GenerateVoicing.fourWayClose cMajorSixNineChord
                    in
                    Expect.equal expected result
            ]
        ]
