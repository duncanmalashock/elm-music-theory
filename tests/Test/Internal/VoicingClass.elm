module Test.Internal.VoicingClass exposing (..)

import Expect
import Internal.Interval as Interval
import Internal.Voicing.FourPart as FourPart
import Internal.VoicingClass as VoicingClass
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Voicing Tests"
        [ describe "building voicing classes"
            [ describe "withFactor"
                [ test "builds a single voicing class" <|
                    \_ ->
                        let
                            expected =
                                [ { voiceOne = Interval.perfectUnison
                                  , voiceTwo = Interval.majorThird
                                  , voiceThree = Interval.perfectFifth
                                  , voiceFour = Interval.majorSeventh
                                  }
                                ]

                            result =
                                VoicingClass.builder FourPart.VoicingClass
                                    |> VoicingClass.withFactor Interval.perfectUnison
                                        { mustBeUnique = False }
                                    |> VoicingClass.withFactor Interval.majorThird
                                        { mustBeUnique = False }
                                    |> VoicingClass.withFactor Interval.perfectFifth
                                        { mustBeUnique = False }
                                    |> VoicingClass.withFactor Interval.majorSeventh
                                        { mustBeUnique = False }
                                    |> VoicingClass.execute
                                        { placeFactors = List.singleton }
                        in
                        Expect.equal expected result
                , test "returns no results for a repeated factor if mustBeUnique is True" <|
                    \_ ->
                        let
                            expected =
                                []

                            result =
                                VoicingClass.builder FourPart.VoicingClass
                                    |> VoicingClass.withFactor Interval.perfectUnison
                                        { mustBeUnique = False }
                                    |> VoicingClass.withFactor Interval.majorThird
                                        { mustBeUnique = False }
                                    |> VoicingClass.withFactor Interval.perfectFifth
                                        { mustBeUnique = False }
                                    |> VoicingClass.withFactor Interval.perfectUnison
                                        { mustBeUnique = True }
                                    |> VoicingClass.execute
                                        { placeFactors = List.singleton }
                        in
                        Expect.equal expected result
                ]
            , describe "withFactorFrom"
                [ test "builds multiple voicing classes from permutations of options" <|
                    \_ ->
                        let
                            expected =
                                [ { voiceOne = Interval.perfectUnison
                                  , voiceTwo = Interval.majorThird
                                  , voiceThree = Interval.perfectFifth
                                  , voiceFour = Interval.minorSeventh
                                  }
                                , { voiceOne = Interval.perfectUnison
                                  , voiceTwo = Interval.majorThird
                                  , voiceThree = Interval.majorThirteenth
                                  , voiceFour = Interval.minorSeventh
                                  }
                                , { voiceOne = Interval.majorNinth
                                  , voiceTwo = Interval.majorThird
                                  , voiceThree = Interval.perfectFifth
                                  , voiceFour = Interval.minorSeventh
                                  }
                                , { voiceOne = Interval.majorNinth
                                  , voiceTwo = Interval.majorThird
                                  , voiceThree = Interval.majorThirteenth
                                  , voiceFour = Interval.minorSeventh
                                  }
                                ]

                            result =
                                VoicingClass.builder FourPart.VoicingClass
                                    |> VoicingClass.withFactorFrom
                                        [ Interval.perfectUnison
                                        , Interval.majorNinth
                                        ]
                                        { mustBeUnique = False }
                                    |> VoicingClass.withFactor Interval.majorThird
                                        { mustBeUnique = False }
                                    |> VoicingClass.withFactorFrom
                                        [ Interval.perfectFifth
                                        , Interval.majorThirteenth
                                        ]
                                        { mustBeUnique = False }
                                    |> VoicingClass.withFactor Interval.minorSeventh
                                        { mustBeUnique = False }
                                    |> VoicingClass.execute
                                        { placeFactors = List.singleton }
                        in
                        Expect.equal expected result
                , test "does not duplicate already used factors when mustBeUnique is True" <|
                    \_ ->
                        let
                            expected =
                                [ { voiceOne = Interval.perfectUnison
                                  , voiceTwo = Interval.majorThird
                                  , voiceThree = Interval.perfectFifth
                                  , voiceFour = Interval.minorSeventh
                                  }
                                ]

                            result =
                                VoicingClass.builder FourPart.VoicingClass
                                    |> VoicingClass.withFactor
                                        Interval.perfectUnison
                                        { mustBeUnique = False }
                                    |> VoicingClass.withFactor
                                        Interval.majorThird
                                        { mustBeUnique = False }
                                    |> VoicingClass.withFactorFrom
                                        [ Interval.perfectFifth
                                        , Interval.perfectUnison
                                        ]
                                        { mustBeUnique = True }
                                    |> VoicingClass.withFactor
                                        Interval.minorSeventh
                                        { mustBeUnique = False }
                                    |> VoicingClass.execute
                                        { placeFactors = List.singleton }
                        in
                        Expect.equal expected result
                ]
            , describe "withTwoFactorsFrom"
                [ test "builds multiple voicing classes from permutations of options" <|
                    \_ ->
                        let
                            expected =
                                [ { voiceOne = Interval.majorNinth
                                  , voiceTwo = Interval.majorThird
                                  , voiceThree = Interval.majorSeventh
                                  , voiceFour = Interval.perfectUnison
                                  }
                                , { voiceOne = Interval.majorNinth
                                  , voiceTwo = Interval.majorThird
                                  , voiceThree = Interval.perfectFifth
                                  , voiceFour = Interval.perfectUnison
                                  }
                                , { voiceOne = Interval.majorNinth
                                  , voiceTwo = Interval.majorSeventh
                                  , voiceThree = Interval.majorThird
                                  , voiceFour = Interval.perfectUnison
                                  }
                                , { voiceOne = Interval.majorNinth
                                  , voiceTwo = Interval.majorSeventh
                                  , voiceThree = Interval.perfectFifth
                                  , voiceFour = Interval.perfectUnison
                                  }
                                , { voiceOne = Interval.majorNinth
                                  , voiceTwo = Interval.perfectFifth
                                  , voiceThree = Interval.majorThird
                                  , voiceFour = Interval.perfectUnison
                                  }
                                , { voiceOne = Interval.majorNinth
                                  , voiceTwo = Interval.perfectFifth
                                  , voiceThree = Interval.majorSeventh
                                  , voiceFour = Interval.perfectUnison
                                  }
                                ]

                            result =
                                VoicingClass.builder FourPart.VoicingClass
                                    |> VoicingClass.withFactor Interval.majorNinth
                                        { mustBeUnique = False }
                                    |> VoicingClass.withTwoFactorsFrom
                                        [ Interval.majorThird
                                        , Interval.majorSeventh
                                        , Interval.perfectFifth
                                        ]
                                        { mustBeUnique = False }
                                    |> VoicingClass.withFactor Interval.perfectUnison
                                        { mustBeUnique = False }
                                    |> VoicingClass.execute
                                        { placeFactors = List.singleton }
                        in
                        Expect.equal expected result
                , test "does not duplicate already used factors when mustBeUnique is True" <|
                    \_ ->
                        let
                            expected =
                                [ { voiceOne = Interval.majorNinth
                                  , voiceTwo = Interval.majorThird
                                  , voiceThree = Interval.perfectFifth
                                  , voiceFour = Interval.perfectUnison
                                  }
                                , { voiceOne = Interval.majorNinth
                                  , voiceTwo = Interval.perfectFifth
                                  , voiceThree = Interval.majorThird
                                  , voiceFour = Interval.perfectUnison
                                  }
                                ]

                            result =
                                VoicingClass.builder FourPart.VoicingClass
                                    |> VoicingClass.withFactor Interval.majorNinth
                                        { mustBeUnique = False }
                                    |> VoicingClass.withTwoFactorsFrom
                                        [ Interval.majorThird
                                        , Interval.majorNinth
                                        , Interval.perfectFifth
                                        ]
                                        { mustBeUnique = True }
                                    |> VoicingClass.withFactor Interval.perfectUnison
                                        { mustBeUnique = False }
                                    |> VoicingClass.execute
                                        { placeFactors = List.singleton }
                        in
                        Expect.equal expected result
                ]
            , describe "withThreeFactorsFrom"
                [ test "builds multiple voicing classes from permutations of options" <|
                    \_ ->
                        let
                            expected =
                                [ { voiceOne = Interval.majorThird
                                  , voiceTwo = Interval.perfectFifth
                                  , voiceThree = Interval.minorSeventh
                                  , voiceFour = Interval.perfectUnison
                                  }
                                , { voiceOne = Interval.majorThird
                                  , voiceTwo = Interval.minorSeventh
                                  , voiceThree = Interval.perfectFifth
                                  , voiceFour = Interval.perfectUnison
                                  }
                                , { voiceOne = Interval.perfectFifth
                                  , voiceTwo = Interval.majorThird
                                  , voiceThree = Interval.minorSeventh
                                  , voiceFour = Interval.perfectUnison
                                  }
                                , { voiceOne = Interval.perfectFifth
                                  , voiceTwo = Interval.minorSeventh
                                  , voiceThree = Interval.majorThird
                                  , voiceFour = Interval.perfectUnison
                                  }
                                , { voiceOne = Interval.minorSeventh
                                  , voiceTwo = Interval.majorThird
                                  , voiceThree = Interval.perfectFifth
                                  , voiceFour = Interval.perfectUnison
                                  }
                                , { voiceOne = Interval.minorSeventh
                                  , voiceTwo = Interval.perfectFifth
                                  , voiceThree = Interval.majorThird
                                  , voiceFour = Interval.perfectUnison
                                  }
                                ]

                            result : List FourPart.VoicingClass
                            result =
                                VoicingClass.builder FourPart.VoicingClass
                                    |> VoicingClass.withThreeFactorsFrom
                                        [ Interval.majorThird
                                        , Interval.perfectFifth
                                        , Interval.minorSeventh
                                        ]
                                        { mustBeUnique = False }
                                    |> VoicingClass.withFactor Interval.perfectUnison
                                        { mustBeUnique = False }
                                    |> VoicingClass.execute
                                        { placeFactors = List.singleton }
                        in
                        Expect.equal expected result
                , test "does not duplicate already used factors when mustBeUnique is True" <|
                    \_ ->
                        let
                            expected =
                                [ { voiceOne = Interval.majorNinth
                                  , voiceTwo = Interval.majorThird
                                  , voiceThree = Interval.perfectFifth
                                  , voiceFour = Interval.majorThirteenth
                                  }
                                , { voiceOne = Interval.majorNinth
                                  , voiceTwo = Interval.majorThird
                                  , voiceThree = Interval.majorThirteenth
                                  , voiceFour = Interval.perfectFifth
                                  }
                                , { voiceOne = Interval.majorNinth
                                  , voiceTwo = Interval.perfectFifth
                                  , voiceThree = Interval.majorThird
                                  , voiceFour = Interval.majorThirteenth
                                  }
                                , { voiceOne = Interval.majorNinth
                                  , voiceTwo = Interval.perfectFifth
                                  , voiceThree = Interval.majorThirteenth
                                  , voiceFour = Interval.majorThird
                                  }
                                , { voiceOne = Interval.majorNinth
                                  , voiceTwo = Interval.majorThirteenth
                                  , voiceThree = Interval.majorThird
                                  , voiceFour = Interval.perfectFifth
                                  }
                                , { voiceOne = Interval.majorNinth
                                  , voiceTwo = Interval.majorThirteenth
                                  , voiceThree = Interval.perfectFifth
                                  , voiceFour = Interval.majorThird
                                  }
                                ]

                            result =
                                VoicingClass.builder FourPart.VoicingClass
                                    |> VoicingClass.withFactor Interval.majorNinth
                                        { mustBeUnique = False }
                                    |> VoicingClass.withThreeFactorsFrom
                                        [ Interval.majorThird
                                        , Interval.majorNinth
                                        , Interval.perfectFifth
                                        , Interval.majorThirteenth
                                        ]
                                        { mustBeUnique = True }
                                    |> VoicingClass.execute
                                        { placeFactors = List.singleton }
                        in
                        Expect.equal expected result
                ]
            ]
        ]
