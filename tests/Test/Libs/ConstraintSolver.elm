module Test.Libs.ConstraintSolver exposing (all)

import Expect
import Libs.ConstraintSolver as ConstraintSolver
import Result.Extra
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
                        [ { a = 1, b = 2, c = 3, d = 4 }
                        , { a = 3, b = 4, c = 5, d = 6 }
                        , { a = 5, b = 6, c = 7, d = 8 }
                        , { a = 7, b = 8, c = 9, d = 10 }
                        ]

                    result =
                        solutions
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
    [ theProblemSetup ]
        |> ConstraintSolver.tryAllFor
            .a
            possibleNumberValues
            (\problemSetup val -> { problemSetup | a = Just val })
        |> ConstraintSolver.tryAllFor
            .b
            possibleNumberValues
            (\problemSetup val -> { problemSetup | b = Just val })
        |> ConstraintSolver.tryAllFor
            .c
            possibleNumberValues
            (\problemSetup val -> { problemSetup | c = Just val })
        |> ConstraintSolver.tryAllFor
            .d
            possibleNumberValues
            (\problemSetup val -> { problemSetup | d = Just val })


possibleNumberValues : List Int
possibleNumberValues =
    [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]


setupToSolution : ProblemSetup -> Maybe Solution
setupToSolution theProblemSetup =
    Maybe.map4
        Solution
        theProblemSetup.a
        theProblemSetup.b
        theProblemSetup.c
        theProblemSetup.d


constraints : List (ProblemSetup -> Result String Bool)
constraints =
    [ endsOnAnEvenNumber
    , allNumbersAreSequential
    ]


endsOnAnEvenNumber : ProblemSetup -> Result String Bool
endsOnAnEvenNumber theProblemSetup =
    Maybe.map
        (\theNumber ->
            if modBy 2 theNumber == 0 then
                Ok True

            else
                Err "Last number is not even"
        )
        theProblemSetup.d
        |> Maybe.withDefault (Ok True)


allNumbersAreSequential : ProblemSetup -> Result String Bool
allNumbersAreSequential theProblemSetup =
    ConstraintSolver.combineConstraints
        [ areSequential theProblemSetup.a theProblemSetup.b
        , areSequential theProblemSetup.b theProblemSetup.c
        , areSequential theProblemSetup.c theProblemSetup.d
        ]


areSequential : Maybe Int -> Maybe Int -> Result String Bool
areSequential maybeA maybeB =
    Maybe.map2
        (\a b ->
            if b - a == 1 then
                Ok True

            else
                Err <|
                    String.fromInt a
                        ++ " and "
                        ++ String.fromInt b
                        ++ " are not sequential"
        )
        maybeA
        maybeB
        |> Maybe.withDefault (Ok True)
