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
    case checkConstraints constraints current of
        Ok _ ->
            -- Constraints are fulfilled.
            -- Are there next setups?
            case getNextSetups current of
                [] ->
                    -- No next setups, solution should be valid.
                    case setupToSolution current of
                        Ok solution ->
                            -- Converted to solution successfully
                            case backtrack of
                                [] ->
                                    -- No backtrack setups to complete,
                                    -- add solution to solved list
                                    { current = current
                                    , backtrack = backtrack
                                    , solved = solved ++ [ solution ]
                                    , failed = failed
                                    }

                                head :: tail ->
                                    -- Backtrack setups to complete,
                                    -- add solution to solved list and solve them
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
                            -- Couldn't be converted to solution.
                            case backtrack of
                                [] ->
                                    -- No backtrack setups to complete,
                                    -- add solution to solved list
                                    { current = current
                                    , backtrack = backtrack
                                    , solved = solved
                                    , failed = failed ++ [ ( error, current ) ]
                                    }

                                head :: tail ->
                                    -- Backtrack setups to complete,
                                    -- add solution to solved list and solve them
                                    solveHelp
                                        constraints
                                        getNextSetups
                                        setupToSolution
                                        { current = head
                                        , backtrack = tail
                                        , solved = solved
                                        , failed = failed ++ [ ( error, current ) ]
                                        }

                head :: tail ->
                    -- Next setups exist, add them to the backtrack list
                    -- and attempt to solve first setup
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
            -- Constraints are not fulfilled
            case backtrack of
                [] ->
                    -- No backtrack setups to complete
                    { current = current
                    , backtrack = backtrack
                    , solved = solved
                    , failed = failed
                    }

                head :: tail ->
                    -- Backtrack setups to complete,
                    -- add errored attempt to failed list and
                    -- attempt to solve backtracks
                    solveHelp
                        constraints
                        getNextSetups
                        setupToSolution
                        { current = head
                        , backtrack = tail
                        , solved = solved
                        , failed = failed ++ [ ( validationError, current ) ]
                        }


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
