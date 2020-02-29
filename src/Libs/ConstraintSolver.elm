module Libs.ConstraintSolver exposing (combineConstraints, solve, tryAllFor)

import Result.Extra


type alias SolveResult solution problemSetup error =
    { solved : List solution
    , failed : List ( error, problemSetup )
    }


solve :
    { problemSetup : problemSetup
    , getNextSetups : problemSetup -> List problemSetup
    , constraints : List (problemSetup -> Result error problemSetup)
    , setupToSolution : problemSetup -> Result error solution
    }
    -> SolveResult solution problemSetup error
solve { problemSetup, getNextSetups, constraints, setupToSolution } =
    { current = problemSetup
    , backtrack = []
    , solved = []
    , failed = []
    }
        |> solveHelp constraints getNextSetups setupToSolution
        |> (\inProgress ->
                { solved = inProgress.solved
                , failed = inProgress.failed
                }
           )


tryAllFor :
    (problemSetup -> Maybe part)
    -> List part
    -> (problemSetup -> part -> problemSetup)
    -> List problemSetup
    -> List problemSetup
tryAllFor accessor possibleValues updateValue problemSetups =
    List.concatMap
        (\problemSetup ->
            case accessor problemSetup of
                Nothing ->
                    List.map (updateValue problemSetup) possibleValues

                Just value ->
                    []
        )
        problemSetups


combineConstraints :
    List (Result error problemSetup)
    -> problemSetup
    -> Result error problemSetup
combineConstraints constraints theProblemSetup =
    List.foldl
        (\constraint state ->
            state
                |> Result.map always
                |> Result.Extra.andMap constraint
        )
        (Ok theProblemSetup)
        constraints


type alias InProgress problemSetup error solution =
    { current : problemSetup
    , backtrack : List problemSetup
    , solved : List solution
    , failed : List ( error, problemSetup )
    }


solveHelp :
    List (problemSetup -> Result error problemSetup)
    -> (problemSetup -> List problemSetup)
    -> (problemSetup -> Result error solution)
    -> InProgress problemSetup error solution
    -> InProgress problemSetup error solution
solveHelp constraints getNextSetups setupToSolution { current, backtrack, solved, failed } =
    case extractSolution setupToSolution constraints current of
        Ok solution ->
            case backtrack of
                [] ->
                    { current = current
                    , backtrack = backtrack
                    , solved = solved ++ [ solution ]
                    , failed = failed
                    }

                head :: tail ->
                    solveHelp
                        constraints
                        getNextSetups
                        setupToSolution
                        { current = head
                        , backtrack = tail
                        , solved = solved ++ [ solution ]
                        , failed = failed
                        }

        Err error ->
            case checkConstraints constraints current of
                Ok _ ->
                    case getNextSetups current of
                        [] ->
                            { current = current
                            , backtrack = backtrack
                            , solved = solved
                            , failed = failed
                            }

                        head :: tail ->
                            solveHelp
                                constraints
                                getNextSetups
                                setupToSolution
                                { current = head
                                , backtrack = backtrack ++ tail
                                , solved = solved
                                , failed = failed
                                }

                Err validationError ->
                    case backtrack of
                        [] ->
                            { current = current
                            , backtrack = backtrack
                            , solved = solved
                            , failed = failed
                            }

                        head :: tail ->
                            solveHelp
                                constraints
                                getNextSetups
                                setupToSolution
                                { current = head
                                , backtrack = tail
                                , solved = solved
                                , failed = failed ++ [ ( validationError, current ) ]
                                }


extractSolution :
    (problemSetup -> Result error solution)
    -> List (problemSetup -> Result error problemSetup)
    -> problemSetup
    -> Result error solution
extractSolution validateSolution constraints theProblemSetup =
    case checkConstraints constraints theProblemSetup of
        Ok value ->
            validateSolution value

        Err error ->
            Err error


checkConstraints :
    List (problemSetup -> Result error problemSetup)
    -> problemSetup
    -> Result error problemSetup
checkConstraints constraints theProblemSetup =
    List.foldl
        (\constraint state ->
            state
                |> Result.andThen constraint
        )
        (Ok theProblemSetup)
        constraints
