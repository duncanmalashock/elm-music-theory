module Test.Util exposing (..)

import Expect


expectAllInList : (a -> Maybe String) -> List a -> Expect.Expectation
expectAllInList maybeErrorDescription list =
    List.map maybeErrorDescription list
        |> List.filterMap identity
        |> (\errorDescriptions ->
                case errorDescriptions of
                    [] ->
                        Expect.pass

                    nonEmptyList ->
                        nonEmptyList
                            |> String.join ", "
                            |> (\str ->
                                    "[ " ++ str ++ "]"
                               )
                            |> Expect.fail
           )
