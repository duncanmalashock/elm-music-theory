module Test.Util.Sort exposing (all)

import Expect
import Test exposing (Test, describe, test)
import Util.Sort


all : Test
all =
    describe "all"
        [ describe "sortByMultiple"
            [ test "should sort with combined ordering fns" <|
                \_ ->
                    let
                        expected =
                            [ 2
                            , 0
                            , 3
                            , 1
                            ]

                        orderGreatestFirst : Int -> Int -> Order
                        orderGreatestFirst a b =
                            case compare a b of
                                EQ ->
                                    EQ

                                LT ->
                                    GT

                                GT ->
                                    LT

                        orderEvenFirst : Int -> Int -> Order
                        orderEvenFirst a b =
                            case ( modBy 2 a == 0, modBy 2 b == 0 ) of
                                ( True, True ) ->
                                    EQ

                                ( False, True ) ->
                                    GT

                                ( True, False ) ->
                                    LT

                                ( False, False ) ->
                                    EQ

                        result =
                            [ 0, 1, 2, 3 ]
                                |> Util.Sort.sortByMultiple
                                    [ orderGreatestFirst
                                    , orderEvenFirst
                                    ]
                    in
                    Expect.equal expected result
            ]
        ]
