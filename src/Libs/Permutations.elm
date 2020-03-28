module Libs.Permutations exposing
    ( permutations2
    , permutations3
    , permutations4
    , permutations5
    )


permutations2 :
    List a
    -> List b
    -> (a -> b -> final)
    -> List final
permutations2 listA listB construct =
    List.concatMap
        (\a ->
            List.map
                (\b ->
                    construct a b
                )
                listB
        )
        listA


permutations3 :
    List a
    -> List b
    -> List c
    -> (a -> b -> c -> final)
    -> List final
permutations3 listA listB listC construct =
    List.concatMap
        (\a ->
            List.concatMap
                (\b ->
                    List.map
                        (\c ->
                            construct a b c
                        )
                        listC
                )
                listB
        )
        listA


permutations4 :
    List a
    -> List b
    -> List c
    -> List d
    -> (a -> b -> c -> d -> final)
    -> List final
permutations4 listA listB listC listD construct =
    List.concatMap
        (\a ->
            List.concatMap
                (\b ->
                    List.concatMap
                        (\c ->
                            List.map
                                (\d ->
                                    construct a b c d
                                )
                                listD
                        )
                        listC
                )
                listB
        )
        listA


permutations5 :
    List a
    -> List b
    -> List c
    -> List d
    -> List e
    -> (a -> b -> c -> d -> e -> final)
    -> List final
permutations5 listA listB listC listD listE construct =
    List.concatMap
        (\a ->
            List.concatMap
                (\b ->
                    List.concatMap
                        (\c ->
                            List.concatMap
                                (\d ->
                                    List.map
                                        (\e ->
                                            construct a b c d e
                                        )
                                        listE
                                )
                                listD
                        )
                        listC
                )
                listB
        )
        listA
