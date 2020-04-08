module Test.Util.ConstraintSolver exposing (all)

import Expect
import Test exposing (Test, describe, test)
import Util.ConstraintSolver as ConstraintSolver


all : Test
all =
    describe "all"
        [ test "should find all solutions that fit the constraints" <|
            \_ ->
                let
                    solveResult =
                        ConstraintSolver.solve
                            { problemSetup = initialSetup
                            , getNextSetups = getNextSetups
                            , constraints = constraints
                            , setupToSolution = setupToSolution
                            }

                    expected =
                        4

                    result =
                        solveResult.solved
                            |> List.length
                in
                Expect.equal expected result
        , test "should include failed attempts" <|
            \_ ->
                let
                    solveResult =
                        ConstraintSolver.solve
                            { problemSetup = initialSetup
                            , getNextSetups = getNextSetups
                            , constraints = constraints
                            , setupToSolution = setupToSolution
                            }

                    expected =
                        9995

                    result =
                        solveResult.failed
                            |> List.length
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


setupToSolution : ProblemSetup -> Result String Solution
setupToSolution theProblemSetup =
    Maybe.map4
        Solution
        theProblemSetup.a
        theProblemSetup.b
        theProblemSetup.c
        theProblemSetup.d
        |> Maybe.map Ok
        |> Maybe.withDefault (Err "Couldn't convert to solution")


constraints : List (ProblemSetup -> Result String ProblemSetup)
constraints =
    [ endsOnAnEvenNumber
    , allNumbersAreSequential
    ]


endsOnAnEvenNumber : ProblemSetup -> Result String ProblemSetup
endsOnAnEvenNumber theProblemSetup =
    Maybe.map
        (\theNumber ->
            if modBy 2 theNumber == 0 then
                Ok theProblemSetup

            else
                Err <|
                    "Last number ( "
                        ++ String.fromInt theNumber
                        ++ ") is not even"
        )
        theProblemSetup.d
        |> Maybe.withDefault (Ok theProblemSetup)


allNumbersAreSequential : ProblemSetup -> Result String ProblemSetup
allNumbersAreSequential theProblemSetup =
    ConstraintSolver.combineConstraints
        [ areSequential theProblemSetup.a theProblemSetup.b theProblemSetup
        , areSequential theProblemSetup.b theProblemSetup.c theProblemSetup
        , areSequential theProblemSetup.c theProblemSetup.d theProblemSetup
        ]
        theProblemSetup


areSequential : Maybe Int -> Maybe Int -> ProblemSetup -> Result String ProblemSetup
areSequential maybeA maybeB theProblemSetup =
    Maybe.map2
        (\a b ->
            if b - a == 1 then
                Ok theProblemSetup

            else
                Err <|
                    String.fromInt a
                        ++ " and "
                        ++ String.fromInt b
                        ++ " are not sequential"
        )
        maybeA
        maybeB
        |> Maybe.withDefault (Ok theProblemSetup)
