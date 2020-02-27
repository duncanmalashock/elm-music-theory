module Test.Libs.ConstraintSolver exposing (all)

import Expect
import Libs.ConstraintSolver as ConstraintSolver
import Test exposing (Test, describe, test)


all : Test
all =
    describe "all"
        [ test "should find all solutions that fit the constraints" <|
            \_ ->
                let
                    solutions =
                        ConstraintSolver.solve
                            { problemSetup = initialSetup
                            , getNextSetups = getNextSetups
                            , constraints = constraints
                            , setupToSolution = setupToSolution
                            }

                    expected =
                        4

                    result =
                        List.length solutions
                in
                Expect.equal expected result
        ]


type alias ProblemSetup =
    { a : Maybe Int
    , b : Maybe Int
    , c : Maybe Int
    , d : Maybe Int
    }


type alias Solution =
    { a : Int
    , b : Int
    , c : Int
    , d : Int
    }


initialSetup : ProblemSetup
initialSetup =
    { a = Nothing
    , b = Nothing
    , c = Nothing
    , d = Nothing
    }


getNextSetups : ProblemSetup -> List ProblemSetup
getNextSetups theProblemSetup =
    case theProblemSetup.a of
        Nothing ->
            List.map
                (\val ->
                    { theProblemSetup | a = val }
                )
                possibleNumberValues

        Just a ->
            case theProblemSetup.b of
                Nothing ->
                    List.map
                        (\val ->
                            { theProblemSetup | b = val }
                        )
                        possibleNumberValues

                Just b ->
                    case theProblemSetup.c of
                        Nothing ->
                            List.map
                                (\val ->
                                    { theProblemSetup | c = val }
                                )
                                possibleNumberValues

                        Just c ->
                            case theProblemSetup.d of
                                Nothing ->
                                    List.map
                                        (\val ->
                                            { theProblemSetup | d = val }
                                        )
                                        possibleNumberValues

                                Just d ->
                                    []


possibleNumberValues : List (Maybe Int)
possibleNumberValues =
    [ Just 1
    , Just 2
    , Just 3
    , Just 4
    , Just 5
    , Just 6
    , Just 7
    , Just 8
    , Just 9
    , Just 10
    ]


setupToSolution : ProblemSetup -> Maybe Solution
setupToSolution theProblemSetup =
    Maybe.map4
        Solution
        theProblemSetup.a
        theProblemSetup.b
        theProblemSetup.c
        theProblemSetup.d


constraints : List (ProblemSetup -> Bool)
constraints =
    [ endsOnAnEvenNumber
    , allNumbersAreSequential
    ]


endsOnAnEvenNumber : ProblemSetup -> Bool
endsOnAnEvenNumber theProblemSetup =
    case theProblemSetup.d of
        Just theNumber ->
            modBy 2 theNumber == 0

        Nothing ->
            True


allNumbersAreSequential : ProblemSetup -> Bool
allNumbersAreSequential theProblemSetup =
    List.all
        identity
        [ areSequential theProblemSetup.a theProblemSetup.b
        , areSequential theProblemSetup.b theProblemSetup.c
        , areSequential theProblemSetup.c theProblemSetup.d
        ]


areSequential : Maybe Int -> Maybe Int -> Bool
areSequential maybeA maybeB =
    case maybeA of
        Nothing ->
            True

        Just a ->
            case maybeB of
                Nothing ->
                    True

                Just b ->
                    b - a == 1
