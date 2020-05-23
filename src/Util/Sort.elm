module Util.Sort exposing (..)


sortByMultiple : List (a -> a -> Order) -> List a -> List a
sortByMultiple compareFns list =
    List.sortWith (combineCompareFns compareFns) list


combineCompareFns : List (a -> a -> Order) -> (a -> a -> Order)
combineCompareFns compareFns =
    let
        combine : (a -> a -> Order) -> (a -> a -> Order) -> a -> a -> Order
        combine oldSorter newSorter a b =
            case oldSorter a b of
                EQ ->
                    newSorter a b

                LT ->
                    LT

                GT ->
                    GT
    in
    List.foldl combine (\a b -> EQ) compareFns
