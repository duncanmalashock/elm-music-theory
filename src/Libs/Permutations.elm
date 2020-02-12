module Libs.Permutations exposing (permutations4)


permutations4 :
    List a
    -> List b
    -> List c
    -> List d
    -> (a -> b -> c -> d -> record)
    -> List record
permutations4 listA listB listC listD constructRecord =
    List.map
        (\a ->
            List.map
                (\b ->
                    List.map
                        (\c ->
                            List.map
                                (\d ->
                                    constructRecord a b c d
                                )
                                listD
                        )
                        listC
                )
                listB
        )
        listA
        |> List.concat
        |> List.concat
        |> List.concat
