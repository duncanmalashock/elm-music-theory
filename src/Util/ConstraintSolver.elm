module Util.ConstraintSolver exposing (solve, solveList)


type alias SolveResult solution setup error =
    { solved : List solution
    , failed : List ( error, setup )
    }


solve :
    { setup : setup
    , nextSetups : setup -> List setup
    , constraints : List (setup -> Result error setup)
    , toSolution : setup -> Result error solution
    }
    ->
        { solved : List solution
        , failed : List ( error, setup )
        }
solve { setup, nextSetups, constraints, toSolution } =
    { current = setup
    , backtrack = []
    , solved = []
    , failed = []
    }
        |> solveHelp constraints nextSetups toSolution
        |> (\inProgress ->
                { solved = inProgress.solved
                , failed = inProgress.failed
                }
           )


solveList :
    { setups : List setup
    , nextSetups : setup -> List setup
    , constraints : List (setup -> Result error setup)
    , toSolution : setup -> Result error solution
    }
    ->
        { solved : List solution
        , failed : List ( error, setup )
        }
solveList { setups, nextSetups, constraints, toSolution } =
    List.map
        (\setup ->
            solve
                { setup = setup
                , nextSetups = nextSetups
                , constraints = constraints
                , toSolution = toSolution
                }
        )
        setups
        |> solveResultFromList


solveResultFromList : List (SolveResult solution setup error) -> SolveResult solution setup error
solveResultFromList list =
    { solved = List.concatMap .solved list
    , failed = List.concatMap .failed list
    }


type alias InProgress setup error solution =
    { current : setup
    , backtrack : List setup
    , solved : List solution
    , failed : List ( error, setup )
    }


solveHelp :
    List (setup -> Result error setup)
    -> (setup -> List setup)
    -> (setup -> Result error solution)
    -> InProgress setup error solution
    -> InProgress setup error solution
solveHelp constraints nextSetups toSolution { current, backtrack, solved, failed } =
    case checkConstraints constraints current of
        Ok _ ->
            -- Constraints are fulfilled.
            -- Are there next setups?
            case nextSetups current of
                [] ->
                    -- No next setups, solution should be valid.
                    case toSolution current of
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
                                        nextSetups
                                        toSolution
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
                                        nextSetups
                                        toSolution
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
                        nextSetups
                        toSolution
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
                        nextSetups
                        toSolution
                        { current = head
                        , backtrack = tail
                        , solved = solved
                        , failed = failed ++ [ ( validationError, current ) ]
                        }


checkConstraints :
    List (setup -> Result error setup)
    -> setup
    -> Result error setup
checkConstraints constraints theProblemSetup =
    List.foldl
        (\constraint state ->
            state
                |> Result.andThen constraint
        )
        (Ok theProblemSetup)
        constraints
